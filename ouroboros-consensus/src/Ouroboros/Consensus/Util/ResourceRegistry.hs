{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.Util.ResourceRegistry (
    ResourceRegistry -- opaque
  , RegistryClosedException(..)
  , ResourceRegistryThreadException
    -- * Creating and releasing the registry itself
  , withRegistry
  , registryThread
    -- * Allocating and releasing regular resources
  , ResourceKey
  , allocate
  , allocateEither
  , release
  , unsafeRelease
  , releaseAll
  , unsafeReleaseAll
    -- * Threads
  , Thread -- opaque
  , threadId
  , forkThread
  , cancelThread
  , waitThread
  , waitAnyThread
  , linkToRegistry
  , forkLinkedThread
    -- * Combinators primarily for testing
  , unsafeNewRegistry
  , closeRegistry
  , countResources
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (Exception, asyncExceptionFromException)
import           Control.Monad
import           Control.Monad.State
import           Data.Bifunctor
import           Data.Either (lefts)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tuple (swap)
import           GHC.Generics (Generic)
import           GHC.Stack

import           Cardano.Prelude (NoUnexpectedThunks (..), OnlyCheckIsWHNF (..),
                     UseIsNormalFormNamed (..), allNoUnexpectedThunks)

import           Ouroboros.Consensus.Util (mustBeRight)
import           Ouroboros.Consensus.Util.IOLike
import           Ouroboros.Consensus.Util.Orphans ()

-- | Resource registry
--
-- Note on terminology: when thread A forks thread B, we will say that thread A
-- is the " parent " and thread B is the " child ". No further relationship
-- between the two threads is implied by this terminology. In particular, note
-- that the child may outlive the parent. We will use "fork" and "spawn"
-- interchangeably.
--
-- = Motivation
--
-- Whenever we allocate resources, we must keep track of them so that we can
-- deallocate them when they are no longer required. The most important tool we
-- have to achieve this is 'bracket':
--
-- > bracket allocateResource releaseResource $ \r ->
-- >   .. use r ..
--
-- Often 'bracket' comes in the guise of a with-style combinator
--
-- > withResource $ \r ->
-- >   .. use r ..
--
-- Where this pattern is applicable, it should be used and there is no need to
-- use the 'ResourceRegistry'. However, 'bracket' introduces strict lexical
-- scoping: the resource is available inside the scope of the bracket, and
-- will be deallocated once we leave that scope. That pattern is sometimes
-- hard to use.
--
-- For example, suppose we have this interface to an SQL server
--
-- > query :: Query -> IO QueryHandle
-- > close :: QueryHandle -> IO ()
-- > next  :: QueryHandle -> IO Row
--
-- and suppose furthermore that we are writing a simple webserver that allows a
-- client to send multiple SQL queries, get rows from any open query, and close
-- queries when no longer required:
--
-- > server :: IO ()
-- > server = go Map.empty
-- >   where
-- >     go :: Map QueryId QueryHandle -> IO ()
-- >     go handles = getRequest >>= \case
-- >         New q -> do
-- >           h   <- query q                        -- allocate
-- >           qId <- generateQueryId
-- >           sendResponse qId
-- >           go $ Map.insert qId h handles
-- >         Close qId -> do
-- >           close (handles ! qId)                 -- release
-- >           go $ Map.delete qId handles
-- >         Next qId -> do
-- >           sendResponse =<< next (handles ! qId)
-- >           go handles
--
-- The server opens and closes query handles in response to client requests.
-- Restructuring this code to use 'bracket' would be awkward, but as it stands
-- this code does not ensure that resources get deallocated; for example, if
-- the server thread is killed ('killThread'), resources will be leaked.
--
-- Another, perhaps simpler, example is spawning threads. Threads too should
-- be considered to be resources that we should keep track of and deallocate
-- when they are no longer required, primarily because when we deallocate
-- (terminate) those threads they too will have a chance to deallocate /their/
-- resources. As for other resources, we have a with-style combinator for this
--
-- > withAsync $ \thread -> ..
--
-- Lexical scoping of threads is often inconvenient, however, more so than for
-- regular resources. The temptation is therefore to simply fork a thread and
-- forget about it, but if we are serious about resource deallocation this is
-- not an acceptable solution.
--
-- = The resource registry
--
-- The resource registry is essentially a piece of state tracking which
-- resources have been allocated. The registry itself is allocated with a
-- with-style combinator 'withRegistry', and when we leave that scope any
-- resources not yet deallocated will be released at that point. Typically
-- the registry is only used as a fall-back, ensuring that resources will
-- deallocated even in the presence of exceptions. For example, here's how
-- we might rewrite the above server example using a registry:
--
-- > server' :: IO ()
-- > server' =
-- >     withRegistry $ \registry -> go registry Map.empty
-- >   where
-- >     go :: ResourceRegistry IO
-- >        -> Map QueryId (ResourceKey, QueryHandle)
-- >        -> IO ()
-- >     go registry handles = getRequest >>= \case
-- >         New q -> do
-- >           (key, h) <- allocate registry (query q) close  -- allocate
-- >           qId      <- generateQueryId
-- >           sendResponse qId
-- >           go registry $ Map.insert qId (key, h) handles
-- >         Close qId -> do
-- >           release registry (fst (handles ! qId))         -- release
-- >           go registry $ Map.delete qId handles
-- >         Next qId -> do
-- >           sendResponse =<< next (snd (handles ! qId))
-- >           go registry handles
--
-- We allocate the query with the help of the registry, providing the registry
-- with the means to deallocate the query should that be required. We can /and
-- should/ still manually release resources also: in this particular example,
-- the (lexical) scope of the registry is the entire server thread, so delaying
-- releasing queries until we exit that scope will probably mean we hold on to
-- resources for too long. The registry is only there as a fall-back.
--
-- = Spawning threads
--
-- We already observed in the introduction that insisting on lexical scoping
-- for threads is often inconvenient, and that simply using 'fork' is no
-- solution as it means we might leak resources. There is however another
-- problem with 'fork'. Consider this snippet:
--
-- > withRegistry $ \registry ->
-- >   r <- allocate registry allocateResource releaseResource
-- >   fork $ .. use r ..
--
-- It is easy to see that this code is problematic: we allocate a resource @r@,
-- then spawn a thread that uses @r@, and finally leave the scope of
-- 'withRegistry', thereby deallocating @r@ -- leaving the thread to run with
-- a now deallocated resource.
--
-- It is /only/ safe for threads to use a given registry, and/or its registered
-- resources, if the lifetime of those threads is tied to the lifetime of the
-- registry. There would be no problem with the example above if the thread
-- would be terminated when we exit the scope of 'withRegistry'.
--
-- The 'forkThread' combinator provided by the registry therefore does two
-- things: it allocates the thread as a resource in the registry, so that it can
-- kill the thread when releasing all resources in the registry. It also records
-- the thread ID in a set of known threads. Whenever the registry is accessed
-- from a thread /not/ in this set, the registry throws a runtime exception,
-- since such a thread might outlive the registry and hence its contents. The
-- intention is that this guards against dangerous patterns like the one above.
--
-- = Linking
--
-- When thread A spawns thread B using 'withAsync', the lifetime of B is tied
-- to the lifetime of A:
--
-- > withAsync .. $ \threadB -> ..
--
-- After all, when A exits the scope of the 'withAsync', thread B will be
-- killed. The reverse is however not true: thread B can terminate before
-- thread A. It is often useful for thread A to be able to declare a dependency
-- on thread B: if B somehow fails, that is, terminates with an exception, we
-- want that exception to be rethrown in thread A as well. A can achieve this
-- by /linking/ to B:
--
-- > withAsync .. $ \threadB -> do
-- >   link threadB
-- >   ..
--
-- Linking a parent to a child is however of limited value if the lifetime of
-- the child is not limited by the lifetime of the parent. For example, if A
-- does
--
-- > threadB <- async $ ..
-- > link threadB
--
-- and A terminates before B does, any exception thrown by B might be send to a
-- thread that no longer exists. This is particularly problematic when we start
-- chaining threads: if A spawns-and-links-to B which spawns-and-links-to C, and
-- C throws an exception, perhaps the intention is that this gets rethrown to B,
-- and then rethrown to A, terminating all three threads; however, if B has
-- terminated before the exception is thrown, C will throw the exception to a
-- non-existent thread and A is never notified.
--
-- For this reason, the registry's 'linkToRegistry' combinator does not link the
-- specified to the thread calling 'linkToRegistry', but rather to the thread
-- that created the registry. After all, the lifetime of threads spawned with
-- 'forkThread' can certainly exceed the lifetime of their parent threads, but
-- the lifetime of /all/ threads spawned using the registry will be limited by
-- the scope of that registry, and hence the lifetime of the thread that created
-- it. So, when we call 'linkToRegistry', the exception will be thrown the
-- thread that created the registry, which (if not caught) will cause that that
-- to exit the scope of 'withRegistry', thereby terminating all threads in that
-- registry.
--
-- # Combining the registry and with-style allocation
--
-- The presence of a registry does not mean it /must/ be used. Whenever
-- 'bracket' is applicable, it can (and probably should) still be used. T
--
-- This includes 'withAsync': after all, 'withAsync' can't result in a child
-- thread that outlives its parent, so this is safe. In particular, if a parent
-- thread wants to link to a child thread /and handle any exceptions arising
-- from the link/, it should probably use `withAsync`:
--
-- > let handleLinkException :: ExceptionInLinkedThread -> m ()
-- >     handleLinkException = ..
-- > in handle handleLinkException $
-- >      withAsync codeInChild $ \child ->
-- >        ..
--
-- instead of
--
-- > handle handleLinkException $ do  -- PROBABLY NOT CORRECT!
-- >   child <- async codeInChild
-- >   ..
--
-- where the parent may exit the scope of the exception handler before the child
-- terminates. If the lifetime of the child cannot be limited to the lifetime of
-- the parent, the child should probably be linked to the registry instead and
-- the thread that spawned the registry should handle any exceptions.
--
-- Apart from `withAsync`, it is /also/ fine to includes nested calls to
-- 'withRegistry'. Since the lifetime of such a registry (and all resources
-- within) is tied to the thread calling 'withRegistry', which itself is tied to
-- the "parent registry" in which it was created, this creates a hierarchy of
-- registries. It is of course essential for compositionality that we should be
-- able to create local registries, but even if we do have easy access to a
-- parent regisry, creating a local one where possibly is useful as it limits
-- the scope of the resources created within, and hence their maximum lifetimes.
data ResourceRegistry m = ResourceRegistry {
      -- | Context in which the registry was created
      registryContext :: !(Context m)

      -- | Registry state
    , registryState   :: !(StrictTVar m (RegistryState m))
    }
  deriving (Generic)

deriving instance IOLike m => NoUnexpectedThunks (ResourceRegistry m)

{-------------------------------------------------------------------------------
  Internal: registry state
-------------------------------------------------------------------------------}

-- | Internal registry state
data RegistryState m = RegistryState {
      -- | Forked threads
      registryThreads   :: !(KnownThreads m)

      -- | Currently allocated resources
    , registryResources :: !(Map ResourceId (Resource m))

      -- | Next available resource key
    , registryNextKey   :: !ResourceId

      -- | Does the registry still accept new allocations?
      --
      -- See 'RegistryClosedException' for discussion.
    , registryStatus    :: !RegistryStatus
    }
  deriving (Generic, NoUnexpectedThunks)

-- | Threads known to the registry
--
-- This is the set of threads spawned using 'forkThread'. The lifetimes of all
-- of these threads are limited by the lifetime of the registry.
--
-- Does not include the thread ID of the thread that created the registry. After
-- all, this thread may well outlive the registry (though the registry cannot
-- outlive it).
--
-- Invariant (informal): the set of registered threads is a subset of the
-- registered resources ('registryResources'). (This invariant is temporarily
-- broken when we start a new thread in 'forkThread' but will be re-established
-- before that thread starts execution proper.)
newtype KnownThreads m = KnownThreads (Set (ThreadId m))
  deriving NoUnexpectedThunks via UseIsNormalFormNamed "KnownThreads" (KnownThreads m)

-- | Status of the registry (open or closed)
data RegistryStatus =
    RegistryOpen

    -- | We record the 'CallStack' to the call to 'close
  | RegistryClosed !PrettyCallStack
  deriving (Generic, NoUnexpectedThunks)

-- | Resource key
--
-- Resource keys are tied to a particular registry.
data ResourceKey m = ResourceKey !(ResourceRegistry m) !ResourceId
  deriving (Generic, NoUnexpectedThunks)

-- | Resource ID
--
-- Resources allocated later have a "larger" key (in terms of the 'Ord'
-- instance) than resources allocated earlier. We take advantage of this when we
-- close the registry to release "younger" resources before "older" resources.
newtype ResourceId = ResourceId Int
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Enum, NoUnexpectedThunks)

-- | Information about a resource
data Resource m = Resource {
      -- | Context in which the resource was created
      resourceContext :: !(Context m)

      -- | Deallocate the resource
    , resourceRelease :: !(Release m)
    }
  deriving (Generic, NoUnexpectedThunks)

newtype Release m = Release (m ())
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "Release" (Release m)

releaseResource :: Resource m -> m ()
releaseResource Resource{resourceRelease = Release f} = f

instance Show (Release m) where
  show _ = "<<release>>"

{-------------------------------------------------------------------------------
  Internal: pure functions on the registry state
-------------------------------------------------------------------------------}

modifyKnownThreads :: (Set (ThreadId m) -> Set (ThreadId m))
                   -> KnownThreads m -> KnownThreads m
modifyKnownThreads f (KnownThreads ts) = KnownThreads (f ts)

-- | Auxiliary for functions that should be disallowed when registry is closed
unlessClosed :: State (RegistryState m) a
             -> State (RegistryState m) (Either PrettyCallStack a)
unlessClosed f = do
    status <- gets registryStatus
    case status of
      RegistryClosed closed -> return $ Left closed
      RegistryOpen          -> Right <$> f

-- | Allocate key for new resource
allocKey :: State (RegistryState m) (Either PrettyCallStack ResourceId)
allocKey = unlessClosed $ do
    nextKey <- gets registryNextKey
    modify $ \st -> st {registryNextKey = succ nextKey}
    return nextKey

-- | Insert new resource
insertResource :: ResourceId
               -> Resource m
               -> State (RegistryState m) (Either PrettyCallStack ())
insertResource key r = unlessClosed $ do
    modify $ \st -> st {
        registryResources = Map.insert key r (registryResources st)
      }

-- | Remove resource from the registry (if it exists)
removeResource :: ResourceId -> State (RegistryState m) (Maybe (Resource m))
removeResource key = state $ \st ->
      second (\x -> st {registryResources = x})
    . Map.updateLookupWithKey (\_ _ -> Nothing) key
    $ registryResources st

-- | Insert thread into the set of known threads
insertThread :: IOLike m => ThreadId m -> State (RegistryState m) ()
insertThread tid =
    modify $ \st -> st {
        registryThreads = modifyKnownThreads (Set.insert tid) $
                            registryThreads st
      }

-- | Remove thread from set of known threads
removeThread :: IOLike m => ThreadId m -> State (RegistryState m) ()
removeThread tid =
    modify $ \st -> st {
        registryThreads = modifyKnownThreads (Set.delete tid) $
                            registryThreads st
      }

-- | Close the registry
--
-- Returns the keys currently registered if the registry is not already closed.
close :: PrettyCallStack
      -> State (RegistryState m) (Either PrettyCallStack (Set ResourceId))
close closeCallStack = unlessClosed $ do
    modify $ \st -> st {registryStatus = RegistryClosed closeCallStack}
    gets $ Map.keysSet . registryResources

-- | Convenience function for updating the registry state
updateState :: forall m a. IOLike m
            => ResourceRegistry m
            -> State (RegistryState m) a
            -> m a
updateState rr f =
    atomically $ updateTVar (registryState rr) (swap . runState f)

-- | Attempt to allocate a resource in a registry which is closed
--
-- When calling 'closeRegistry' (typically, leaving the scope of
-- 'withRegistry'), all resources in the registry must be released. If a
-- concurrent thread is still allocating resources, we end up with a race
-- between the thread trying to allocate new resources and the registry trying
-- to free them all. To avoid this, before releasing anything, the registry will
-- record itself as closed. Any attempt by a concurrent thread to allocate a new
-- resource will then result in a 'RegistryClosedException'.
--
-- It is probably not particularly useful for threads to try and catch this
-- exception (apart from in a generic handler that does local resource cleanup).
-- The thread will anyway soon receive a 'ThreadKilled' exception.
data RegistryClosedException =
    forall m. IOLike m => RegistryClosedException {
        -- | The context in which the registry was created
        registryClosedRegistryContext :: !(Context m)

        -- | Callstack to the call to 'close'
        --
        -- Note that 'close' can only be called from the same thread that
        -- created the registry.
      , registryClosedCloseCallStack  :: !PrettyCallStack

        -- | Context of the call resulting in the exception
      , registryClosedAllocContext    :: !(Context m)
      }

deriving instance Show RegistryClosedException
instance Exception RegistryClosedException

{-------------------------------------------------------------------------------
  Creating and releasing the registry itself
-------------------------------------------------------------------------------}

-- | Create a new registry
--
-- You are strongly encouraged to use 'withRegistry' instead.
-- Exported primarily for the benefit of tests.
unsafeNewRegistry :: (IOLike m, HasCallStack) => m (ResourceRegistry m)
unsafeNewRegistry = do
    context  <- captureContext
    stateVar <- newTVarM initState
    return ResourceRegistry {
          registryContext = context
        , registryState   = stateVar
        }
  where
    initState :: RegistryState m
    initState = RegistryState {
          registryThreads   = KnownThreads Set.empty
        , registryResources = Map.empty
        , registryNextKey   = ResourceId 1
        , registryStatus    = RegistryOpen
        }

-- | Close the registry
--
-- This can only be called from the same thread that created the registry.
-- This is a no-op if the registry is already closed.
--
-- This entire function runs with exceptions masked, so that we are not
-- interrupted while we release all resources.
--
-- Resources will be allocated from young to old, so that resources allocated
-- later can safely refer to resources created earlier.
--
-- The release functions are run in the scope of an exception handler, so that
-- if releasing one resource throws an exception, we still attempt to release
-- the other resources. Should we catch an exception whilst we close the
-- registry, we will rethrow it after having attempted to release all resources.
-- If there is more than one, we will pick a random one to rethrow, though we
-- will prioritize asynchronous exceptions over other exceptions. This may be
-- important for exception handlers that catch all-except-asynchronous
-- exceptions.
closeRegistry :: (IOLike m, HasCallStack) => ResourceRegistry m -> m ()
closeRegistry rr = mask_ $ do
    context <- captureContext
    unless (contextThreadId context == contextThreadId (registryContext rr)) $
      throwM $ ResourceRegistryClosedFromWrongThread {
          resourceRegistryCreatedIn = registryContext rr
        , resourceRegistryUsedIn    = context
        }

    -- Close the registry so that we cannot allocate any further resources
    alreadyClosed <- updateState rr $ close (contextCallStack context)
    case alreadyClosed of
      Left _ ->
        return ()
      Right keys -> do
        -- At this point we have not /removed/ any elements from the map,
        -- allowing concurrent threads to do their own cleanup of resources
        -- (this may for instance be important if a thread deallocates its
        -- resources in a particular order -- note that cancelling a thread
        -- is a synchronous operation, so we will wait for it to finish
        -- releasing its resources.)
        -- /If/ a concurrent thread does some cleanup, then some of the calls
        -- to 'release' that we do here might be no-ops.
       releaseResources rr keys release

-- | Helper for 'closeRegistry', 'releaseAll', and 'unsafeReleaseAll': release
-- the resources allocated with the given 'ResourceId's.
releaseResources :: (IOLike m, HasCallStack)
                 => ResourceRegistry m
                 -> Set ResourceId
                 -> (ResourceKey m -> m ())
                    -- ^ How to release the resource, e.g., 'release' or
                    -- 'unsafeRelease'.
                 ->  m ()
releaseResources rr keys releaser = do
    exs <- forM (newToOld keys) $ try . releaser . ResourceKey rr
    case prioritize exs of
      Nothing -> return ()
      Just e  -> throwM e
  where
    newToOld :: Set ResourceId -> [ResourceId]
    newToOld = Set.toDescList -- depends on 'Ord' instance

    prioritize :: [Either SomeException ()] -> Maybe SomeException
    prioritize =
          (\(asyncEx, otherEx) -> listToMaybe asyncEx <|> listToMaybe otherEx)
        . first catMaybes
        . unzip
        . map (\e -> (asyncExceptionFromException e, e))
        . lefts

-- | Create a new registry
--
-- See documentation of 'ResourceRegistry' for a detailed discussion.
withRegistry :: (IOLike m, HasCallStack) => (ResourceRegistry m -> m a) -> m a
withRegistry = bracket unsafeNewRegistry closeRegistry

{-------------------------------------------------------------------------------
  Simple queries on the registry
-------------------------------------------------------------------------------}

-- | The thread that created the registry
registryThread :: ResourceRegistry m -> ThreadId m
registryThread = contextThreadId . registryContext

-- | Number of currently allocated resources
--
-- Primarily for the benefit of testing.
countResources :: IOLike m => ResourceRegistry m -> m Int
countResources rr = atomically $ aux <$> readTVar (registryState rr)
  where
    aux :: RegistryState m -> Int
    aux = Map.size . registryResources

{-------------------------------------------------------------------------------
  Allocating resources
-------------------------------------------------------------------------------}

-- | Allocate new resource
--
-- The allocation function will be run with asynchronous exceptions masked. This
-- means that the resource allocation must either be fast or else interruptible;
-- see "Dealing with Asynchronous Exceptions during Resource Acquisition"
-- <http://www.well-typed.com/blog/97/> for details.
allocate :: forall m a. (IOLike m, HasCallStack)
         => ResourceRegistry m
         -> (ResourceId -> m a)
         -> (a -> m ())
         -> m (ResourceKey m, a)
allocate rr alloc = fmap mustBeRight . allocateEither rr (fmap Right . alloc)

-- | Generalization of 'allocate' for allocation functions that may fail
allocateEither :: forall m e a. (IOLike m, HasCallStack)
               => ResourceRegistry m
               -> (ResourceId -> m (Either e a))
               -> (a -> m ())
               -> m (Either e (ResourceKey m, a))
allocateEither rr alloc free = do
    context <- captureContext
    ensureKnownThread rr context
    -- We check if the registry has been closed when we allocate the key, so
    -- that we can avoid needlessly allocating the resource.
    mKey <- updateState rr $ allocKey
    case mKey of
      Left closed ->
        throwRegistryClosed rr context closed
      Right key -> mask_ $ do
        ma <- alloc key
        case ma of
          Left  e -> return $ Left e
          Right a -> do
            -- TODO: Might want to have an exception handler around this call to
            -- 'updateState' just in case /that/ throws an exception.
            inserted <- updateState rr $ insertResource key (mkResource context a)
            case inserted of
              Left closed -> do
                -- Despite the earlier check, it's possible that the registry
                -- got closed after we allocated a new key but before we got a
                -- chance to register the resource. In this case, we must
                -- deallocate the resource again before throwing the exception.
                free a
                throwRegistryClosed rr context closed
              Right () ->
                return $ Right (ResourceKey rr key, a)
  where
    mkResource :: Context m -> a -> Resource m
    mkResource context a = Resource {
          resourceContext = context
        , resourceRelease = Release $ free a
        }

throwRegistryClosed :: IOLike m
                    => ResourceRegistry m
                    -> Context m
                    -> PrettyCallStack
                    -> m x
throwRegistryClosed rr context closed = throwM RegistryClosedException {
      registryClosedRegistryContext = registryContext rr
    , registryClosedCloseCallStack  = closed
    , registryClosedAllocContext    = context
    }

-- | Release resource
--
-- This deallocates the resource and removes it from the registry. It will be
-- the responsibility of the caller to make sure that the resource is no longer
-- used in any thread.
--
-- The deallocation function is run with exceptions masked, so that we are
-- guaranteed not to remove the resource from the registry without releasing it.
--
-- Releasing an already released resource is a no-op.
release :: (IOLike m, HasCallStack) => ResourceKey m -> m ()
release key@(ResourceKey rr _) = do
    context <- captureContext
    ensureKnownThread rr context
    unsafeRelease key

-- | Unsafe version of 'release'
--
-- The only difference between 'release' and 'unsafeRelease' is that the latter
-- does not insist that it is called from a thread that is known to the
-- registry. This is dangerous, because it implies that there is a thread with
-- access to a resource which may be deallocated before that thread is
-- terminated. Of course, we can't detect all such situations (when the thread
-- merely uses a resource but does not allocate or release we can't tell), but
-- normally when we /do/ detect this we throw an exception.
--
-- This function should only be used if the above situation can be ruled out
-- or handled by other means.
unsafeRelease :: IOLike m => ResourceKey m -> m ()
unsafeRelease (ResourceKey rr rid) = do
    mask_ $ do
      mResource <- updateState rr $ removeResource rid
      mapM_ releaseResource mResource

-- | Release all resources in the 'ResourceRegistry' without closing.
--
-- See 'closeRegistry' for more details.
releaseAll :: (IOLike m, HasCallStack) => ResourceRegistry m -> m ()
releaseAll rr = do
    context <- captureContext
    unless (contextThreadId context == contextThreadId (registryContext rr)) $
      throwM $ ResourceRegistryClosedFromWrongThread {
          resourceRegistryCreatedIn = registryContext rr
        , resourceRegistryUsedIn    = context
        }
    releaseAllHelper rr context release

-- | This is to 'releaseAll' what 'unsafeRelease' is to 'release': we do not
-- insist that this funciton is called from a thread that is known to the
-- registry. See 'unsafeRelease' for why this is dangerous.
unsafeReleaseAll :: (IOLike m, HasCallStack) => ResourceRegistry m -> m ()
unsafeReleaseAll rr = do
    context <- captureContext
    releaseAllHelper rr context unsafeRelease

-- | Internal helper used by 'releaseAll' and 'unsafeReleaseAll'.
releaseAllHelper :: IOLike m
                 => ResourceRegistry m
                 -> Context m
                 -> (ResourceKey m -> m ())  -- ^ How to release a resource
                 -> m ()
releaseAllHelper rr context releaser = mask_ $ do
    mKeys   <- updateState rr $ unlessClosed $
      gets $ Map.keysSet . registryResources
    case mKeys of
      Left closed -> throwRegistryClosed rr context closed
      Right keys  -> releaseResources rr keys releaser

{-------------------------------------------------------------------------------
  Threads
-------------------------------------------------------------------------------}

-- | Thread
--
-- The internals of this type are not exported.
data Thread m a = IOLike m => Thread {
      threadId         :: !(ThreadId m)
    , threadResourceId :: !ResourceId
    , threadAsync      :: !(Async m a)
    , threadRegistry   :: !(ResourceRegistry m)
    }
  deriving NoUnexpectedThunks via OnlyCheckIsWHNF "Thread" (Thread m a)

-- | 'Eq' instance for 'Thread' compares 'threadId' only.
instance Eq (Thread m a) where
  Thread{threadId = a} == Thread{threadId = b} = a == b

-- | Cancel a thread
--
-- This is a synchronous operation: the thread will have terminated when this
-- function returns.
--
-- Uses 'uninterruptibleCancel' because that's what 'withAsync' does.
cancelThread :: IOLike m => Thread m a -> m ()
cancelThread = uninterruptibleCancel . threadAsync

-- | Wait for thread to terminate and return its result.
--
-- If the thread throws an exception, this will rethrow that exception.
--
-- NOTE: If A waits on B, and B is linked to the registry, and B throws an
-- exception, then A might /either/ receive the exception thrown by B /or/
-- the 'ThreadKilled' exception thrown by the registry.
waitThread :: IOLike m => Thread m a -> m a
waitThread = wait . threadAsync

-- | Lift 'waitAny' to 'Thread'
waitAnyThread :: forall m a. IOLike m => [Thread m a] -> m a
waitAnyThread ts = snd <$> waitAny (map threadAsync ts)

-- | Fork a new thread
forkThread :: forall m a. (IOLike m, HasCallStack)
           => ResourceRegistry m
           -> m a
           -> m (Thread m a)
forkThread rr body = snd <$>
    allocate rr (\key -> mkThread key <$> async (body' key)) cancelThread
  where
    mkThread :: ResourceId -> Async m a -> Thread m a
    mkThread rid child = Thread {
          threadId         = asyncThreadId (Proxy @m) child
        , threadResourceId = rid
        , threadAsync      = child
        , threadRegistry   = rr
        }

    body' :: ResourceId -> m a
    body' rid = do
        me <- myThreadId
        (registerThread me >> body) `finally` unregisterThread me rid

    -- Register the thread
    --
    -- We must add the thread to the list of known threads before the thread
    -- will start to use the registry.
    registerThread :: ThreadId m -> m ()
    registerThread tid = updateState rr $ insertThread tid

    -- Unregister the thread
    --
    -- Threads are the only kinds of resources that "deallocate themselves".
    -- We remove the thread from the resources as well as the set of known
    -- threads, so that these datastructures do not grow without bound.
    --
    -- This runs with asynchronous exceptions masked (due to 'finally'),
    -- though for the current implementation of 'unregisterThread' this
    -- makes no difference.
    unregisterThread :: ThreadId m -> ResourceId -> m ()
    unregisterThread tid rid =
        updateState rr $ do
          removeThread tid
          void $ removeResource rid

-- | Link specified 'Thread' to the (thread that created) the registry
linkToRegistry :: IOLike m => Thread m a -> m ()
linkToRegistry t = linkTo (registryThread $ threadRegistry t) (threadAsync t)

-- | Fork a thread and link to it to the registry.
--
-- This function is just a convenience.
forkLinkedThread :: (IOLike m, HasCallStack)
                 => ResourceRegistry m -> m a -> m (Thread m a)
forkLinkedThread rr body = do
    t <- forkThread rr body
    -- There is no race condition here between the new thread throwing an
    -- exception and the 'linkToRegistry': if the thread /already/ threw the
    -- exception when we link it, the exception will be raised immediately
    -- (see 'linkTo' for details).
    linkToRegistry t
    return t

{-------------------------------------------------------------------------------
  Check that registry is used from known thread
-------------------------------------------------------------------------------}

ensureKnownThread :: forall m. IOLike m
                  => ResourceRegistry m -> Context m -> m ()
ensureKnownThread rr context = do
    isKnown <- checkIsKnown
    unless isKnown $
      throwM $ ResourceRegistryUsedFromUntrackedThread {
                   resourceRegistryCreatedIn = registryContext rr
                 , resourceRegistryUsedIn    = context
                 }
  where
    checkIsKnown :: m Bool
    checkIsKnown
      | contextThreadId context == contextThreadId (registryContext rr) =
          return True
      | otherwise = atomically $ do
          KnownThreads ts <- registryThreads <$> readTVar (registryState rr)
          return $ contextThreadId context `Set.member` ts

-- | Registry used from untracked threads
--
-- If this exception is raised, it indicates a bug in the caller.
data ResourceRegistryThreadException =
    -- | If the registry is used from an untracked thread, we cannot do proper
    -- reference counting. The following threads are /tracked/: the thread
    -- that spawned the registry and all threads spawned by the registry.
    forall m. IOLike m => ResourceRegistryUsedFromUntrackedThread {
          -- | Information about the context in which the registry was created
          resourceRegistryCreatedIn :: !(Context m)

          -- | The context in which it was used
        , resourceRegistryUsedIn    :: !(Context m)
        }

    -- | Registry closed from different threat than that created it
  | forall m. IOLike m => ResourceRegistryClosedFromWrongThread {
          -- | Information about the context in which the registry was created
          resourceRegistryCreatedIn :: !(Context m)

          -- | The context in which it was used
        , resourceRegistryUsedIn    :: !(Context m)
        }

deriving instance Show ResourceRegistryThreadException
instance Exception ResourceRegistryThreadException

{-------------------------------------------------------------------------------
  Auxiliary: context
-------------------------------------------------------------------------------}

data Context m = IOLike m => Context {
      -- | CallStack in which it was created
      contextCallStack :: !PrettyCallStack

      -- | Thread that created the registry or resource
    , contextThreadId  :: !(ThreadId m)
    }

-- Existential type; we can't use generics
instance NoUnexpectedThunks (Context m) where
  showTypeOf _ = "Context"
  whnfNoUnexpectedThunks ctxt (Context cs tid) = allNoUnexpectedThunks
    [ noUnexpectedThunks ctxt cs
    , noUnexpectedThunks ctxt (UseIsNormalFormNamed @"ThreadId" tid)
    ]

deriving instance Show (Context m)

captureContext :: IOLike m => HasCallStack => m (Context m)
captureContext = Context (PrettyCallStack callStack) <$> myThreadId

{-------------------------------------------------------------------------------
  Auxiliary: CallStack with different Show instance

  TODO: May we should move this someplace more general.
-------------------------------------------------------------------------------}

-- | CallStack with 'Show' instance using 'prettyCallStack'
newtype PrettyCallStack = PrettyCallStack CallStack
  deriving newtype (NoUnexpectedThunks)

instance Show PrettyCallStack where
  show (PrettyCallStack cs) = prettyCallStack cs
