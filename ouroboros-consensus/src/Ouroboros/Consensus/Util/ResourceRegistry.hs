{-# OPTIONS_GHC -Werror #-}

{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeApplications           #-}

module Ouroboros.Consensus.Util.ResourceRegistry (
    ResourceRegistry -- opaque
    -- * Creating and releasing the registry itself
  , newRegistry
  , closeRegistry
  , withRegistry
    -- * Simple queries
  , registryThread
  , countResources
    -- * Allocating and releasing regular resources
  , allocate
  , release
    -- * Threads
  , Thread -- opaque
  , threadId
  , forkThread
  , forkThreadSelf
  , cancelThread
  , waitThread
  , linkToRegistry
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (asyncExceptionFromException)
import           Control.Monad
import           Data.Bifunctor
import           Data.Either (lefts)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Tuple (swap)
import           GHC.Stack

import           Control.Monad.Class.MonadAsync
import           Control.Monad.Class.MonadFork
import           Control.Monad.Class.MonadSTM
import           Control.Monad.Class.MonadThrow

-- | Resource registry
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
-- It is /only/ safe for threads to use a given registry if the lifetime of
-- those threads is tied to the lifetime of the registry. There would be no
-- problem with the example above if the thread would be terminated when we
-- exit the scope of 'withRegistry'.
--
-- The 'forkThread' combinator provided by the registry therefore does two
-- things: it allocates the thread as a resource in the registry, so that it can
-- kill the thread when releasing all resources in the registry. It also records
-- the thread ID in a set of known threads. Whenever the registry is accessed
-- from a thread /not/ in this set, the registry throws a runtime exception. The
-- intention is that this guards against dangerous patterns like the one above.
--
-- = Linking
--
-- When thread A spawns thread B using 'withAsync', the lifetime of B is tied
-- to the lifetime of A:
--
-- > withAsync $ \threadB -> ..
--
-- After all, when A exits the scope of the 'withAsync', thread B will be
-- killed. The reverse is however not true: thread B can terminate before
-- thread A. It is often useful for thread A to be able to declare a dependency
-- on thread B: if B somehow fails, that is, terminates with an exception, we
-- want that exception to be rethrown in thread A as well. A can achieve this
-- by /linking/ to B:
--
-- > withAsync $ \threadB -> do
-- >   link threadB
-- >   ..
--
-- Linking is often useful to create a hierarchy of threads that should all
-- be terminated if one of them fails. It is however less useful if B's lifetime
-- is not tied to A's. For example, if A does
--
-- > threadB <- async $ ..
-- > link threadB
--
-- and A terminates before B does, any exception thrown by B might be send to
-- a thread that no longer exists. This is particularly problematic when we
-- start chaining threads: if A spawns-and-links-to B which spawns-and-links-to
-- C, and C throws an exception, both A and B should be notified, but this will
-- not be the case if the B link is missing because B has already terminated.
--
-- For this reason, the registry's 'linkToRegistry' combinator does not link the
-- specified to the thread calling 'linkToRegistry', but rather to the thread
-- that created the registry. After all, the lifetime of threads spawned with
-- 'forkThread' can certainly exceed the lifetime of their parent threads, but
-- the lifetime of /all/ threads spawned using the registry will be limited by
-- the scope of that registry, and hence the lifetime of the thread that created
-- it. So, when we call 'linkToRegistry', the exception will be thrown the
-- thread that created the registry, which (if not caught) will cause that that
-- to exit the scope of 'withRegistry', thereby terminating the entire hierarchy
-- of child threads in that registry.
--
-- # Combining the registry and with-style allocation
--
-- The presence of a registry does not mean it /must/ be used. Whenever
-- 'bracket' is applicable, it can (and probably should) still be used. This
-- includes 'withAsync': after all, 'withAsync' can't result in a child thread
-- that outlives its parent, so this is safe.
--
-- This /also/ includes nested calls to 'withRegistry'. Since the lifetime of
-- such a registry (and all resources within) is tied to the thread calling
-- 'withRegistry', which itself is tied to the "parent registry" in which it was
-- created, this creates a hierarchy of registries. It is of course essential
-- for compositionality that we should be able to create local registries, but
-- even if we do have easy access to a parent regisry, creating a local one
-- where possibly is useful as it limits the scope of the resources created
-- within, and hence their maximum lifetimes.
data ResourceRegistry m = ResourceRegistry {
      -- | Context in which the registry was created
      registryContext :: Context m

      -- | Registry state
    , registryState   :: TVar m (RegistryState m)
    }

{-------------------------------------------------------------------------------
  Internal: registry state
-------------------------------------------------------------------------------}

-- | Internal registry state
data RegistryState m = RegistryState {
      -- | Forked threads
      --
      -- This is the set of threads spawned using 'forkThread'. The lifetimes of
      -- all of these threads are limited by the lifetime of the registry.
      --
      -- Does not include the thread ID of the thread that created the registry.
      -- After all, this thread may well outlive the registry (though the
      -- registry cannot outlive it).
      registryThreads   :: !(Set (ThreadId m))

      -- | Currently allocated resources
    , registryResources :: !(Map ResourceKey (Resource m))

      -- | Next available resource key
    , registryNextKey   :: !ResourceKey

      -- | Does the registry still accept new allocations?
      --
      -- See 'RegistryClosedException' for discussion.
    , registryClosed    :: !(Maybe RegistryClosed)
    }

-- | Information recorded when we close the registry
data RegistryClosed = RegistryClosed {
      -- | CallStack to the call to 'close'
      registryCloseCallStack :: PrettyCallStack
    }

-- | Key to a resource
--
-- Resource keys should not be used in registries other than the one in which
-- they were created.
--
-- Resources allocated later have a "larger" key (in terms of the 'Ord'
-- instance) than resources allocated earlier. We take advantage of this when we
-- close the registry to release "younger" resources before "older" resources.
newtype ResourceKey = ResourceKey Int
  deriving (Show, Eq, Ord, Enum)

-- | Information about a resource
data Resource m = Resource {
      -- | Context in which the resource was created
      resourceContext :: Context m

      -- | Deallocate the resource
    , resourceRelease :: Release m
    }

-- | Newtype wrapper just for the show instance
newtype Release m = Release (m ())

releaseResource :: Resource m -> m ()
releaseResource Resource{resourceRelease = Release f} = f

instance Show (Release m) where
  show _ = "<<release>>"

{-------------------------------------------------------------------------------
  Internal: pure functions on the registry state
-------------------------------------------------------------------------------}

-- | Auxiliary for functions that should be disallowed when registry is closed
unlessClosed :: RegistryState m
             -> (RegistryState m, a)
             -> (RegistryState m, Either RegistryClosed a)
unlessClosed st (st', result) =
    case registryClosed st of
      Just closed -> (st  , Left  closed)
      Nothing     -> (st' , Right result)

-- | Allocate key for new resource
allocKey :: RegistryState m
         -> (RegistryState m, Either RegistryClosed ResourceKey)
allocKey st = unlessClosed st $
    (st {registryNextKey = succ nextKey}, nextKey)
  where
    nextKey :: ResourceKey
    nextKey = registryNextKey st

-- | Insert new resource
insertResource :: ResourceKey
               -> Resource m
               -> RegistryState m
               -> (RegistryState m, Either RegistryClosed ())
insertResource key r st = unlessClosed st $
    (st {registryResources = Map.insert key r (registryResources st)}, ())

-- | Remove resource from the registry (if it exists)
removeResource :: ResourceKey
               -> RegistryState m
               -> (RegistryState m, Maybe (Resource m))
removeResource key st =
      first (\x -> st { registryResources = x })
    . swap
    . Map.updateLookupWithKey (\_ _ -> Nothing) key
    $ registryResources st

-- | Close the registry
--
-- Returns the keys currently registered if the registry is not already closed.
close :: PrettyCallStack
      -> RegistryState m
      -> (RegistryState m, Either RegistryClosed (Set ResourceKey))
close closeCallStack st = unlessClosed st $ (
      st { registryClosed = Just (RegistryClosed closeCallStack) }
    , Map.keysSet (registryResources st)
    )

-- | Convenience function for updating the registry state
updateState :: forall m a. MonadSTM m
            => ResourceRegistry m
            -> (RegistryState m -> (RegistryState m, a))
            -> m a
updateState rr f = atomically $ updateTVar' (registryState rr) f

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
    forall m. MonadFork m => RegistryClosedException {
        -- | The context in which the registry was created
        registryClosedRegistryContext :: Context m

        -- | Callstack to the call to 'close'
        --
        -- Note that 'close' can only be called from the same thread that
        -- created the registry.
      , registryClosedCloseCallStack  :: PrettyCallStack

        -- | Context of the call resulting in the exception
      , registryClosedAllocContext    :: Context m
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
newRegistry :: (MonadFork m, MonadSTM m) => m (ResourceRegistry m)
newRegistry = do
    context <- captureContext
    state   <- atomically $ newTVar initState
    return ResourceRegistry {
          registryContext = context
        , registryState   = state
        }
  where
    initState :: RegistryState m
    initState = RegistryState {
          registryThreads   = Set.empty
        , registryResources = Map.empty
        , registryNextKey   = ResourceKey 1
        , registryClosed    = Nothing
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
-- will prioritize asynchronous exceptions over other exceptions.
closeRegistry :: (MonadSTM m, MonadFork m, MonadMask m, HasCallStack)
              => ResourceRegistry m -> m ()
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
        -- allowing concurrent threads to do their own cleanup of resources.
        -- /If/ a concurrent thread does some cleanup, then some of the calls
        -- to 'release' that we do here might be no-ops.
       exs <- forM (newToOld keys) $ try . release rr
       case prioritize exs of
         Nothing -> return ()
         Just e  -> throwM e
  where
    newToOld :: Set ResourceKey -> [ResourceKey]
    newToOld = Set.toDescList -- depends on 'Ord' instance

    prioritize :: [Either SomeException ()] -> Maybe SomeException
    prioritize =
          (\(asyncEx, otherEx) -> listToMaybe asyncEx <|> listToMaybe otherEx)
        . first catMaybes
        . unzip
        . map (\e -> (asyncExceptionFromException e, e))
        . lefts

withRegistry :: (MonadFork m, MonadMask m, MonadSTM m)
             => (ResourceRegistry m -> m a) -> m a
withRegistry = bracket newRegistry closeRegistry

{-------------------------------------------------------------------------------
  Simple queries on the registry
-------------------------------------------------------------------------------}

-- | The thread that created the registry
registryThread :: ResourceRegistry m -> ThreadId m
registryThread = contextThreadId . registryContext

-- | Number of currently allocated resources
--
-- Primarily for the benefit of testing.
countResources :: MonadSTM m => ResourceRegistry m -> m Int
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
allocate :: forall m a. (
                MonadSTM  m
              , MonadMask m
              , MonadFork m
              , HasCallStack
              )
         => ResourceRegistry m
         -> (ResourceKey -> m a)
         -> (a -> m ())
         -> m (ResourceKey, a)
allocate rr alloc free = do
    context <- captureContext
    ensureKnownThread rr context
    -- We check if the registry has been closed when we allocate the key, so
    -- that we can avoid needlessly allocating the resource.
    mKey <- updateState rr $ allocKey
    case mKey of
      Left closed ->
        throwRegistryClosed context closed
      Right key -> mask_ $ do
        a <- alloc key
        inserted <- updateState rr $ insertResource key (mkResource context a)
        case inserted of
          Left closed -> do
            -- Despite the earlier check, it's possible that the registry got
            -- closed after we allocated a new key but before we got a chance to
            -- register the resource. In this case, we must deallocate the
            -- resource again before throwing the exception.
            free a
            throwRegistryClosed context closed
          Right () ->
            return (key, a)
  where
    mkResource :: Context m -> a -> Resource m
    mkResource context a = Resource {
          resourceContext = context
        , resourceRelease = Release $ free a
        }

    throwRegistryClosed :: forall x. Context m -> RegistryClosed -> m x
    throwRegistryClosed context closed = throwM RegistryClosedException {
          registryClosedRegistryContext = registryContext rr
        , registryClosedCloseCallStack  = registryCloseCallStack closed
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
release :: (MonadSTM m, MonadFork m, MonadMask m)
        => ResourceRegistry m -> ResourceKey -> m ()
release rr key = do
    context <- captureContext
    ensureKnownThread rr context
    mask_ $ do
      mResource <- updateState rr $ removeResource key
      mapM_ releaseResource mResource

{-------------------------------------------------------------------------------
  Threads
-------------------------------------------------------------------------------}

-- | Thread
--
-- The internals of this type are not exported.
data Thread m a = MonadFork m => Thread {
      threadId          :: ThreadId m
    , threadResourceKey :: ResourceKey
    , threadAsync       :: Async m a
    , threadRegistry    :: ResourceRegistry m
    }

-- | 'Eq' instance for 'Thread' compares 'threadId' only.
instance Eq (Thread m a) where
  Thread{threadId = a} == Thread{threadId = b} = a == b

-- | Cancel a thread
--
-- This is a synchronous operation: the thread will have terminated when this
-- function returns.
cancelThread :: MonadAsync m => Thread m a -> m ()
cancelThread = uninterruptibleCancel . threadAsync

-- | Wait for thread to terminate and return its result.
--
-- If the thread throws an exception, this will rethrow that exception.
waitThread :: MonadAsync m => Thread m a -> m a
waitThread = wait . threadAsync

-- | Simple wrapper around 'forkThreadSelf' when self argument is not needed.
forkThread :: forall m a. (
                  MonadMask  m
                , MonadFork  m
                , MonadAsync m
                , HasCallStack
                )
           => ResourceRegistry m
           -> m a
           -> m (Thread m a)
forkThread rr = forkThreadSelf rr . const

forkThreadSelf :: forall m a. (
                      MonadMask  m
                    , MonadFork  m
                    , MonadAsync m
                    , HasCallStack
                    )
               => ResourceRegistry m
               -> (Thread m a -> m a)
               -> m (Thread m a)
forkThreadSelf rr body = do
    snd <$> allocate rr spawnThread cancelThread
  where
    spawnThread :: ResourceKey -> m (Thread m a)
    spawnThread key = do
        self  <- atomically newEmptyTMVar
        child <- async $ body' =<< atomically (readTMVar self)
        let thread = Thread {
                         threadId          = asyncThreadId (Proxy @m) child
                       , threadResourceKey = key
                       , threadAsync       = child
                       , threadRegistry    = rr
                       }
        atomically $ putTMVar self thread
        return thread

    body' :: Thread m a -> m a
    body' t = body t `finally` unregisterThread (threadResourceKey t)

    -- Threads are the only kinds of resources that "deallocate themselves"
    unregisterThread :: ResourceKey -> m ()
    unregisterThread key = void $ updateState rr $ removeResource key

-- | Link specified 'Thread' to the (thread that created) the registry
linkToRegistry :: (MonadAsync m, MonadFork m, MonadMask m)
               => Thread m a -> m ()
linkToRegistry t = linkTo (registryThread $ threadRegistry t) (threadAsync t)

{-------------------------------------------------------------------------------
  Check that registry is used from known thread
-------------------------------------------------------------------------------}

ensureKnownThread :: forall m. (MonadSTM m, MonadFork m, MonadThrow m)
                  => ResourceRegistry m -> Context m -> m ()
ensureKnownThread rr context = do
    isKnown <- checkIsKnown
    unless isKnown $
      throwM $ ResourceRegistryUsedFromUnknownThread {
                   resourceRegistryCreatedIn = registryContext rr
                 , resourceRegistryUsedIn    = context
                 }
  where
    checkIsKnown :: m Bool
    checkIsKnown
      | contextThreadId context == contextThreadId (registryContext rr) =
          return True
      | otherwise = atomically $ do
          knownThreads <- registryThreads <$> readTVar (registryState rr)
          return $ contextThreadId context `Set.member` knownThreads

-- | Registry used from unknown threads
--
-- If this exception is raised, it indicates a bug in the caller.
data ResourceRegistryThreadException =
    -- | If the registry is used from an unknown thread, we cannot do proper
    -- reference counting
    forall m. MonadFork m => ResourceRegistryUsedFromUnknownThread {
          -- | Information about the context in which the registry was created
          resourceRegistryCreatedIn :: Context m

          -- | The context in which it was used
        , resourceRegistryUsedIn    :: Context m
        }

    -- | Registry closed from different threat than that created it
  | forall m. MonadFork m => ResourceRegistryClosedFromWrongThread {
          -- | Information about the context in which the registry was created
          resourceRegistryCreatedIn :: Context m

          -- | The context in which it was used
        , resourceRegistryUsedIn    :: Context m
        }

deriving instance Show ResourceRegistryThreadException
instance Exception ResourceRegistryThreadException

{-------------------------------------------------------------------------------
  Auxiliary: context
-------------------------------------------------------------------------------}

data Context m = MonadFork m => Context {
      -- | CallStack in which it was created
      contextCallStack :: PrettyCallStack

      -- | Thread that created the registry or resource
    , contextThreadId  :: ThreadId m
    }

deriving instance Show (Context m)

captureContext :: MonadFork m => HasCallStack => m (Context m)
captureContext = Context (PrettyCallStack callStack) <$> myThreadId

{-------------------------------------------------------------------------------
  Auxiliary: CallStack with different Show instance

  TODO: May we should move this someplace more general.
-------------------------------------------------------------------------------}

-- | CallStack with 'Show' instance using 'prettyCallStack'
newtype PrettyCallStack = PrettyCallStack CallStack

instance Show PrettyCallStack where
  show (PrettyCallStack cs) = prettyCallStack cs
