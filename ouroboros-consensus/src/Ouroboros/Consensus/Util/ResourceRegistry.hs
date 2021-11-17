{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE UndecidableInstances       #-}

module Ouroboros.Consensus.Util.ResourceRegistry (
    RegistryClosedException (..)
  , ResourceRegistryThreadException
    -- * Creating and releasing the registry itself
  , bracketWithPrivateRegistry
  , registryThread
  , withRegistry
    -- * Allocating and releasing regular resources
  , ResourceKey
  , allocate
  , allocateEither
  , release
  , releaseAll
  , unsafeRelease
  , unsafeReleaseAll
    -- * Threads
  , cancelThread
  , forkLinkedThread
  , forkThread
  , linkToRegistry
  , threadId
  , waitAnyThread
  , waitThread
  , withThread
    -- ** opaque
  , Thread
    -- * Temporary registry
  , TempRegistryException (..)
  , allocateTemp
  , modifyWithTempRegistry
  , runInnerWithTempRegistry
  , runWithTempRegistry
    -- ** opaque
  , WithTempRegistry
    -- * Combinators primarily for testing
  , closeRegistry
  , countResources
  , unsafeNewRegistry
    -- * opaque
  , ResourceRegistry
  ) where

import           Control.Applicative ((<|>))
import           Control.Exception (asyncExceptionFromException)
import           Control.Monad
import           Control.Monad.Reader
import           Control.Monad.State.Strict
import           Data.Bifunctor
import           Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import           Data.Either (partitionEithers)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes, listToMaybe)
import           Data.Proxy
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Word (Word64)
import           GHC.Generics (Generic)
import           NoThunks.Class (InspectHeapNamed (..), OnlyCheckWhnfNamed (..),
                     allNoThunks)

import           Ouroboros.Consensus.Util (mustBeRight, whenJust)
import           Ouroboros.Consensus.Util.CallStack
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
-- specified thread to the thread calling 'linkToRegistry', but rather to the
-- thread that created the registry. After all, the lifetime of threads spawned
-- with 'forkThread' can certainly exceed the lifetime of their parent threads,
-- but the lifetime of /all/ threads spawned using the registry will be limited
-- by the scope of that registry, and hence the lifetime of the thread that
-- created it. So, when we call 'linkToRegistry', the exception will be thrown
-- the thread that created the registry, which (if not caught) will cause that
-- that to exit the scope of 'withRegistry', thereby terminating all threads in
-- that registry.
--
-- # Combining the registry and with-style allocation
--
-- It is perfectly possible (indeed, advisable) to use 'bracket' and
-- bracket-like allocation functions alongside the registry, but note that the
-- usual caveats with 'bracket' and forking threads still applies. In
-- particular, spawning threads inside the 'bracket' that make use of the
-- bracketed resource is problematic; this is of course true whether or not a
-- registry is used.
--
-- In principle this also includes 'withAsync'; however, since 'withAsync'
-- results in a thread that is not known to the registry, such a thread will not
-- be able to use the registry (the registry would throw an unknown thread
-- exception, as described above). For this purpose we provide 'withThread';
-- 'withThread' (as opposed to 'forkThread') should be used when a parent thread
-- wants to handle exceptions in the child thread; see 'withThread' for
-- detailed discussion.
--
-- It is /also/ fine to includes nested calls to 'withRegistry'. Since the
-- lifetime of such a registry (and all resources within) is tied to the thread
-- calling 'withRegistry', which itself is tied to the "parent registry" in
-- which it was created, this creates a hierarchy of registries. It is of course
-- essential for compositionality that we should be able to create local
-- registries, but even if we do have easy access to a parent regisry, creating
-- a local one where possibly is useful as it limits the scope of the resources
-- created within, and hence their maximum lifetimes.
data ResourceRegistry m = ResourceRegistry {
      -- | Context in which the registry was created
      registryContext :: !(Context m)

      -- | Registry state
    , registryState   :: !(StrictTVar m (RegistryState m))
    }
  deriving (Generic)

deriving instance IOLike m => NoThunks (ResourceRegistry m)

{-------------------------------------------------------------------------------
  Internal: registry state
-------------------------------------------------------------------------------}

-- | The age of a resource
--
-- Age here is represented by an meaningless number. The one and only property
-- that matters is that the age of resource A that was successfully allocated
-- before resource B was (in the same registry) will be greater than the age of
-- resource B.
--
-- For the current implementation, that property will be true unless the
-- registry lives long enough to have contained 2^64 separately allocated
-- resources.
--
-- This data is not exposed by the 'ResourceRegistry' interface.
newtype Age = Age Word64
  deriving stock   (Show)
  deriving newtype (Eq, Ord)
  deriving NoThunks via InspectHeapNamed "Age" Age

-- | The age of the first resource successfully allocated in a fresh registry
ageOfFirstResource :: Age
ageOfFirstResource = Age maxBound

-- | Map the age of the latest resource to be successfully allocated to the age
-- of the next resource to be successfully allocated in the same registry
nextYoungerAge :: Age -> Age
nextYoungerAge (Age n) = Age (n - 1)

-- | Internal registry state
--
-- INVARIANT: We record exactly the ages of currently allocated resources,
-- @'Bimap.keys' . 'registryAges' = 'Map.keys' . 'registryResources'@.
data RegistryState m = RegistryState {
      -- | Forked threads
      registryThreads   :: !(KnownThreads m)

      -- | Currently allocated resources
    , registryResources :: !(Map ResourceId (Resource m))

      -- | Next available resource key
    , registryNextKey   :: !ResourceId

      -- | The age of each currently allocated resource
      --
      -- We use a 'Bimap' so we can maintain the keys in sorted order by age,
      -- which is necessary when closing the registry.
    , registryAges      :: !(Bimap ResourceId Age)

      -- | The age of the next resource
    , registryNextAge   :: !Age

      -- | Does the registry still accept new allocations?
      --
      -- See 'RegistryClosedException' for discussion.
    , registryStatus    :: !RegistryStatus
    }
  deriving (Generic, NoThunks)

-- | The currently allocated keys in youngest-to-oldest order
getYoungestToOldest :: RegistryState m -> [ResourceId]
getYoungestToOldest = map snd . Bimap.toAscListR . registryAges

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
  deriving NoThunks via InspectHeapNamed "KnownThreads" (KnownThreads m)

-- | Status of the registry (open or closed)
data RegistryStatus =
    RegistryOpen

    -- | We record the 'CallStack' to the call to 'close
  | RegistryClosed !PrettyCallStack
  deriving (Generic, NoThunks)

-- | Resource key
--
-- Resource keys are tied to a particular registry.
data ResourceKey m = ResourceKey !(ResourceRegistry m) !ResourceId
  deriving (Generic, NoThunks)

-- | Return the 'ResourceId' of a 'ResourceKey'.
resourceKeyId :: ResourceKey m -> ResourceId
resourceKeyId (ResourceKey _rr rid) = rid

-- | Resource ID
--
-- This uniquifying data is not exposed by the 'ResourceRegistry' interface.
newtype ResourceId = ResourceId Int
  deriving stock   (Show, Eq, Ord)
  deriving newtype (Enum, NoThunks)

-- | Information about a resource
data Resource m = Resource {
      -- | Context in which the resource was created
      resourceContext :: !(Context m)

      -- | Deallocate the resource
    , resourceRelease :: !(Release m)
    }
  deriving (Generic, NoThunks)

-- | Release the resource, return 'True' when the resource was actually
-- released, return 'False' when the resource was already released.
--
-- If unsure, returning 'True' is always fine.
newtype Release m = Release (m Bool)
  deriving NoThunks via OnlyCheckWhnfNamed "Release" (Release m)

releaseResource :: Resource m -> m Bool
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
      , registryAges      = Bimap.insert
                              key
                              (registryNextAge st)
                              (registryAges st)
      , registryNextAge   = nextYoungerAge (registryNextAge st)
      }

-- | Remove resource from the registry (if it exists)
removeResource :: ResourceId -> State (RegistryState m) (Maybe (Resource m))
removeResource key = state $ \st ->
    let (mbResource, resources') = Map.updateLookupWithKey
                                     (\_ _ -> Nothing)
                                     key
                                     (registryResources st)

        st' = st {
            registryResources = resources'
          , registryAges      = Bimap.delete key (registryAges st)
          }
    in  (mbResource, st')

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
-- Returns the keys currently allocated if the registry is not already closed.
--
-- POSTCONDITION: They are returned in youngest-to-oldest order.
close :: PrettyCallStack
      -> State (RegistryState m) (Either PrettyCallStack [ResourceId])
close closeCallStack = unlessClosed $ do
    modify $ \st -> st {registryStatus = RegistryClosed closeCallStack}
    gets getYoungestToOldest

-- | Convenience function for updating the registry state
updateState :: forall m a. IOLike m
            => ResourceRegistry m
            -> State (RegistryState m) a
            -> m a
updateState rr f =
    atomically $ stateTVar (registryState rr) (runState f)

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
    stateVar <- newTVarIO initState
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
        , registryAges      = Bimap.empty
        , registryNextAge   = ageOfFirstResource
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
      throwIO $ ResourceRegistryClosedFromWrongThread {
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
       void $ releaseResources rr keys release

-- | Helper for 'closeRegistry', 'releaseAll', and 'unsafeReleaseAll': release
-- the resources allocated with the given 'ResourceId's.
--
-- Returns the contexts of the resources that were actually released.
releaseResources :: IOLike m
                 => ResourceRegistry m
                 -> [ResourceId]
                    -- ^ PRECONDITION: The currently allocated keys,
                    -- youngest-to-oldest
                 -> (ResourceKey m -> m (Maybe (Context m)))
                    -- ^ How to release the resource, e.g., 'release' or
                    -- 'unsafeRelease'.
                 ->  m [Context m]
releaseResources rr sortedKeys releaser = do
    (exs, mbContexts) <- fmap partitionEithers $
      forM sortedKeys $ try . releaser . ResourceKey rr

    case prioritize exs of
      Nothing -> return (catMaybes mbContexts)
      Just e  -> throwIO e
  where
    prioritize :: [SomeException] -> Maybe SomeException
    prioritize =
          (\(asyncEx, otherEx) -> listToMaybe asyncEx <|> listToMaybe otherEx)
        . first catMaybes
        . unzip
        . map (\e -> (asyncExceptionFromException e, e))

-- | Create a new registry
--
-- See documentation of 'ResourceRegistry' for a detailed discussion.
withRegistry :: (IOLike m, HasCallStack) => (ResourceRegistry m -> m a) -> m a
withRegistry = bracket unsafeNewRegistry closeRegistry

-- | Create a new private registry for use by a bracketed resource
--
-- Use this combinator as a more specific and easier-to-maintain alternative to
-- the following.
--
-- > 'withRegistry' $ \rr ->
-- >   'bracket' (newFoo rr) closeFoo $ \foo ->
-- >     (... rr does not occur in this scope ...)
--
-- NB The scoped body can use `withRegistry` if it also needs its own, separate
-- registry.
--
-- Use this combinator to emphasize that the registry is private to (ie only
-- used by and/or via) the bracketed resource and that it thus has nearly the
-- same lifetime. This combinator ensures the following specific invariants
-- regarding lifetimes and order of releases.
--
-- o The registry itself is older than the bracketed resource.
--
-- o The only registered resources older than the bracketed resource were
--   allocated in the registry by the function that allocated the bracketed
--   resource.
--
-- o Because of the older resources, the bracketed resource is itself also
--   registered in the registry; that's the only way we can be sure to release
--   all resources in the right order.
--
-- NB Because the registry is private to the resource, the @a@ type could save
-- the handle to @registry@ and safely close the registry if the scoped body
-- calls @closeA@ before the bracket ends. Though we have not used the type
-- system to guarantee that the interface of the @a@ type cannot leak the
-- registry to the body, this combinator does its part to keep the registry
-- private to the bracketed resource.
--
-- See documentation of 'ResourceRegistry' for a more general discussion.
bracketWithPrivateRegistry :: (IOLike m, HasCallStack)
                           => (ResourceRegistry m -> m a)
                           -> (a -> m ())  -- ^ Release the resource
                           -> (a -> m r)
                           -> m r
bracketWithPrivateRegistry newA closeA body =
    withRegistry $ \registry -> do
      (_key, a) <- allocate registry (\_key -> newA registry) closeA
      body a

{-------------------------------------------------------------------------------
  Temporary registry
-------------------------------------------------------------------------------}

-- | Run an action with a temporary resource registry.
--
-- When allocating resources that are meant to end up in some final state,
-- e.g., stored in a 'TVar', after which they are guaranteed to be released
-- correctly, it is possible that an exception is thrown after allocating such
-- a resource, but before it was stored in the final state. In that case, the
-- resource would be leaked. 'runWithTempRegistry' solves that problem.
--
-- When no exception is thrown before the end of 'runWithTempRegistry', the
-- user must have transferred all the resources it allocated to their final
-- state. This means that these resources don't have to be released by the
-- temporary registry anymore, the final state is now in charge of releasing
-- them.
--
-- In case an exception is thrown before the end of 'runWithTempRegistry',
-- /all/ resources allocated in the temporary registry will be released.
--
-- Resources must be allocated using 'allocateTemp'.
--
-- To make sure that the user doesn't forget to transfer a resource to the
-- final state @st@, the user must pass a function to 'allocateTemp' that
-- checks whether a given @st@ contains the resource, i.e., whether the
-- resource was successfully transferred to its final destination.
--
-- When no exception is thrown before the end of 'runWithTempRegistry', we
-- check whether all allocated resources have been transferred to the final
-- state @st@. If there's a resource that hasn't been transferred to the final
-- state /and/ that hasn't be released or closed before (see the release
-- function passed to 'allocateTemp'), a 'TempRegistryRemainingResource'
-- exception will be thrown.
--
-- For that reason, 'WithTempRegistry' is parameterised over the final state
-- type @st@ and the given 'WithTempRegistry' action must return the final
-- state.
--
-- NOTE: we explicitly don't let 'runWithTempRegistry' return the final state,
-- because the state /must/ have been stored somewhere safely, transferring
-- the resources, before the temporary registry is closed.
runWithTempRegistry
  :: (IOLike m, HasCallStack)
  => WithTempRegistry st m (a, st)
  -> m a
runWithTempRegistry m = withRegistry $ \rr -> do
    varTransferredTo <- newTVarIO mempty
    let tempRegistry = TempRegistry {
            tempResourceRegistry = rr
          , tempTransferredTo    = varTransferredTo
          }
    (a, st) <- runReaderT (unWithTempRegistry m) tempRegistry
    -- We won't reach this point if an exception is thrown, so we won't check
    -- for remaining resources in that case.
    --
    -- No need to mask here, whether we throw the async exception or
    -- 'TempRegistryRemainingResource' doesn't matter.
    transferredTo <- atomically $ readTVar varTransferredTo
    untrackTransferredTo rr transferredTo st

    context <- captureContext
    remainingResources <- releaseAllHelper rr context release

    whenJust (listToMaybe remainingResources) $ \remainingResource ->
      throwIO $ TempRegistryRemainingResource {
          tempRegistryContext  = registryContext rr
        , tempRegistryResource = remainingResource
        }
    return a

-- | Embed a self-contained 'WithTempRegistry' computation into a larger one.
--
-- The internal 'WithTempRegistry' is effectively passed to
-- 'runWithTempRegistry'. It therefore must have no dangling resources, for
-- example. This is the meaning of /self-contained/ above.
--
-- The key difference beyond 'runWithTempRegistry' is that the resulting
-- composite resource is also guaranteed to be registered in the outer
-- 'WithTempRegistry' computation's registry once the inner registry is closed.
-- Combined with the following assumption, this establishes the invariant that
-- all resources are (transitively) in a temporary registry.
--
-- As the resource might require some implementation details to be closed, the
-- function to close it will also be provided by the inner computation.
--
-- ASSUMPTION: closing @res@ closes every resource contained in @innerSt@
--
-- NOTE: In the current implementation, there will be a brief moment where the
-- inner registry still contains the inner computation's resources and also the
-- outer registry simultaneously contains the new composite resource. If an
-- async exception is received at that time, then the inner resources will be
-- closed and then the composite resource will be closed. This means there's a
-- risk of /double freeing/, which can be harmless if anticipated.
runInnerWithTempRegistry
  :: forall innerSt st m res a. IOLike m
  => WithTempRegistry innerSt m (a, innerSt, res)
     -- ^ The embedded computation; see ASSUMPTION above
  -> (res -> m Bool)
     -- ^ How to free; same as for 'allocateTemp'
  -> (st -> res -> Bool)
     -- ^ How to check; same as for 'allocateTemp'
  -> WithTempRegistry st m a
runInnerWithTempRegistry inner free isTransferred = do
    outerTR <- WithTempRegistry ask

    lift $ runWithTempRegistry $ do
      (a, innerSt, res) <- inner

      -- Allocate in the outer layer.
      _ <-   withFixedTempRegistry outerTR
           $ allocateTemp (return res) free isTransferred

      -- TODO This point here is where an async exception could cause both the
      -- inner resources to be closed and the outer resource to be closed later.
      --
      -- If we want to do better than that, we'll need a variant of
      -- 'runWithTempRegistry' that lets us perform some action with async
      -- exceptions masked "at the same time" it closes its registry.

      -- Note that everything in `inner` allocated via `allocateTemp` must either be
      -- closed or else present in `innerSt` by this point -- `runWithTempRegistry`
      -- would have thrown if not.
      pure (a, innerSt)
  where
    withFixedTempRegistry
        :: TempRegistry     st      m
        -> WithTempRegistry st      m res
        -> WithTempRegistry innerSt m res
    withFixedTempRegistry env (WithTempRegistry (ReaderT f)) =
      WithTempRegistry $ ReaderT $ \_ -> f env

-- | When 'runWithTempRegistry' exits successfully while there are still
-- resources remaining in the temporary registry that haven't been transferred
-- to the final state.
data TempRegistryException =
    forall m. IOLike m => TempRegistryRemainingResource {
        -- | The context in which the temporary registry was created.
        tempRegistryContext  :: !(Context m)

        -- | The context in which the resource was allocated that was not
        -- transferred to the final state.
      , tempRegistryResource :: !(Context m)
      }

deriving instance Show TempRegistryException
instance Exception TempRegistryException

-- | Given a final state, return the 'ResourceId's of the resources that have
-- been /transferred to/ that state.
newtype TransferredTo st = TransferredTo {
      runTransferredTo :: st -> Set ResourceId
    }
  deriving newtype (Semigroup, Monoid)
  deriving NoThunks via OnlyCheckWhnfNamed "TransferredTo" (TransferredTo st)

-- | The environment used to run a 'WithTempRegistry' action.
data TempRegistry st m = TempRegistry {
      tempResourceRegistry :: !(ResourceRegistry m)
    , tempTransferredTo    :: !(StrictTVar m (TransferredTo st))
      -- ^ Used as a @Writer@.
    }

-- | An action with a temporary registry in scope, see 'runWithTempRegistry'
-- for more details.
--
-- The most important function to run in this monad is 'allocateTemp'.
newtype WithTempRegistry st m a = WithTempRegistry {
      unWithTempRegistry :: ReaderT (TempRegistry st m) m a
    }
  deriving newtype (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadMask, MonadSTM)

instance MonadTrans (WithTempRegistry st) where
  lift = WithTempRegistry . lift

instance MonadState s m => MonadState s (WithTempRegistry st m) where
  state = WithTempRegistry . state

-- | Untrack all the resources from the registry that have been transferred to
-- the given state.
--
-- Untracking a resource means removing it from the registry without releasing
-- it.
--
-- NOTE: does not check that it's called by the same thread that allocated the
-- resources, as it's an internal function only used in 'runWithTempRegistry'.
untrackTransferredTo
  :: IOLike m
  => ResourceRegistry m
  -> TransferredTo st
  -> st
  -> m ()
untrackTransferredTo rr transferredTo st =
    updateState rr $ mapM_ removeResource rids
  where
    rids = runTransferredTo transferredTo st

-- | Allocate a resource in a temporary registry until it has been transferred
-- to the final state @st@. See 'runWithTempRegistry' for more details.
allocateTemp
  :: (IOLike m, HasCallStack)
  => m a
     -- ^ Allocate the resource
  -> (a -> m Bool)
     -- ^ Release the resource, return 'True' when the resource was actually
     -- released, return 'False' when the resource was already released.
     --
     -- Note that it is safe to always return 'True' when unsure.
  -> (st -> a -> Bool)
     -- ^ Check whether the resource is in the given state
  -> WithTempRegistry st m a
allocateTemp alloc free isTransferred = WithTempRegistry $ do
    TempRegistry rr varTransferredTo <- ask
    (key, a) <- lift $ fmap mustBeRight $
      allocateEither rr (fmap Right . const alloc) free
    atomically $ modifyTVar varTransferredTo $ mappend $
      TransferredTo $ \st ->
        if isTransferred st a
        then Set.singleton (resourceKeyId key)
        else Set.empty
    return a

-- | Higher level API on top of 'runWithTempRegistry': modify the given @st@,
-- allocating resources in the process that will be transferred to the
-- returned @st@.
modifyWithTempRegistry
  :: forall m st a. IOLike m
  => m st                                 -- ^ Get the state
  -> (st -> ExitCase st -> m ())          -- ^ Store the new state
  -> StateT st (WithTempRegistry st m) a  -- ^ Modify the state
  -> m a
modifyWithTempRegistry getSt putSt modSt = runWithTempRegistry $
    fst <$> generalBracket (lift getSt) transfer mutate
  where
    transfer :: st -> ExitCase (a, st) -> WithTempRegistry st m ()
    transfer initSt ec = lift $ putSt initSt (snd <$> ec)

    mutate :: st -> WithTempRegistry st m (a, st)
    mutate = runStateT modSt

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
         -> (a -> m ())  -- ^ Release the resource
         -> m (ResourceKey m, a)
allocate rr alloc free = mustBeRight <$>
    allocateEither rr (fmap Right . alloc) (\a -> free a >> return True)

-- | Generalization of 'allocate' for allocation functions that may fail
allocateEither :: forall m e a. (IOLike m, HasCallStack)
               => ResourceRegistry m
               -> (ResourceId -> m (Either e a))
               -> (a -> m Bool)
                  -- ^ Release the resource, return 'True' when the resource
                  -- hasn't been released or closed before.
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
                void $ free a
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
throwRegistryClosed rr context closed = throwIO RegistryClosedException {
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
--
-- When the resource has not been released before, its context is returned.
release :: (IOLike m, HasCallStack) => ResourceKey m -> m (Maybe (Context m))
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
unsafeRelease :: IOLike m => ResourceKey m -> m (Maybe (Context m))
unsafeRelease (ResourceKey rr rid) = do
    mask_ $ do
      mResource <- updateState rr $ removeResource rid
      case mResource of
        Nothing       -> return Nothing
        Just resource -> do
          actuallyReleased <- releaseResource resource
          return $
            if actuallyReleased
            then Just (resourceContext resource)
            else Nothing

-- | Release all resources in the 'ResourceRegistry' without closing.
--
-- See 'closeRegistry' for more details.
releaseAll :: (IOLike m, HasCallStack) => ResourceRegistry m -> m ()
releaseAll rr = do
    context <- captureContext
    unless (contextThreadId context == contextThreadId (registryContext rr)) $
      throwIO $ ResourceRegistryClosedFromWrongThread {
          resourceRegistryCreatedIn = registryContext rr
        , resourceRegistryUsedIn    = context
        }
    void $ releaseAllHelper rr context release

-- | This is to 'releaseAll' what 'unsafeRelease' is to 'release': we do not
-- insist that this funciton is called from a thread that is known to the
-- registry. See 'unsafeRelease' for why this is dangerous.
unsafeReleaseAll :: (IOLike m, HasCallStack) => ResourceRegistry m -> m ()
unsafeReleaseAll rr = do
    context <- captureContext
    void $ releaseAllHelper rr context unsafeRelease

-- | Internal helper used by 'releaseAll' and 'unsafeReleaseAll'.
releaseAllHelper :: IOLike m
                 => ResourceRegistry m
                 -> Context m
                 -> (ResourceKey m -> m (Maybe (Context m)))
                    -- ^ How to release a resource
                 -> m [Context m]
releaseAllHelper rr context releaser = mask_ $ do
    mKeys <- updateState rr $ unlessClosed $ gets getYoungestToOldest
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
  deriving NoThunks via OnlyCheckWhnfNamed "Thread" (Thread m a)

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
           -> String  -- ^ Label for the thread
           -> m a
           -> m (Thread m a)
forkThread rr label body = snd <$>
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
        labelThread me label
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

-- | Bracketed version of 'forkThread'
--
-- The analogue of 'withAsync' for the registry.
--
-- Scoping thread lifetime using 'withThread' is important when a parent
-- thread wants to link to a child thread /and handle any exceptions arising
-- from the link/:
--
-- > let handleLinkException :: ExceptionInLinkedThread -> m ()
-- >     handleLinkException = ..
-- > in handle handleLinkException $
-- >      withThread registry codeInChild $ \child ->
-- >        ..
--
-- instead of
--
-- > handle handleLinkException $ do  -- PROBABLY NOT CORRECT!
-- >   child <- forkThread registry codeInChild
-- >   ..
--
-- where the parent may exit the scope of the exception handler before the child
-- terminates. If the lifetime of the child cannot be limited to the lifetime of
-- the parent, the child should probably be linked to the registry instead and
-- the thread that spawned the registry should handle any exceptions.
--
-- Note that in /principle/ there is no problem in using 'withAync' alongside a
-- registry. After all, in a pattern like
--
-- > withRegistry $ \registry ->
-- >   ..
-- >   withAsync (.. registry ..) $ \async ->
-- >     ..
--
-- the async will be cancelled when leaving the scope of 'withAsync' and so
-- that reference to the registry, or indeed any of the resources inside the
-- registry, is safe. However, the registry implements a sanity check that the
-- registry is only used from known threads. This is useful: when a thread that
-- is not known to the registry (in other words, whose lifetime is not tied to
-- the lifetime of the registry) spawns a resource in that registry, that
-- resource may well be deallocated before the thread terminates, leading to
-- undefined and hard to debug behaviour (indeed, whether or not this results in
-- problems may well depend on precise timing); an exception that is thrown when
-- /allocating/ the resource is (more) deterministic and easier to debug.
-- Unfortunately, it means that the above pattern is not applicable, as the
-- thread spawned by 'withAsync' is not known to the registry, and so if it were
-- to try to use the registry, the registry would throw an error (even though
-- this pattern is actually safe). This situation is not ideal, but for now we
-- merely provide an alternative to 'withAsync' that /does/ register the thread
-- with the registry.
--
-- NOTE: Threads that are spawned out of the user's control but that must still
-- make use of the registry can use the unsafe API. This should be used with
-- caution, however.
withThread :: IOLike m
           => ResourceRegistry m
           -> String  -- ^ Label for the thread
           -> m a
           -> (Thread m a -> m b)
           -> m b
withThread rr label body = bracket (forkThread rr label body) cancelThread

-- | Link specified 'Thread' to the (thread that created) the registry
linkToRegistry :: IOLike m => Thread m a -> m ()
linkToRegistry t = linkTo (registryThread $ threadRegistry t) (threadAsync t)

-- | Fork a thread and link to it to the registry.
--
-- This function is just a convenience.
forkLinkedThread :: (IOLike m, HasCallStack)
                 => ResourceRegistry m
                 -> String  -- ^ Label for the thread
                 -> m a
                 -> m (Thread m a)
forkLinkedThread rr label body = do
    t <- forkThread rr label body
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
      throwIO $ ResourceRegistryUsedFromUntrackedThread {
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
instance NoThunks (Context m) where
  showTypeOf _ = "Context"
  wNoThunks ctxt (Context cs tid) = allNoThunks
    [ noThunks ctxt cs
    , noThunks ctxt (InspectHeapNamed @"ThreadId" tid)
    ]

deriving instance Show (Context m)

captureContext :: IOLike m => HasCallStack => m (Context m)
captureContext = Context prettyCallStack <$> myThreadId
