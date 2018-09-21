{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}

{-# OPTIONS_GHC "-fwarn-incomplete-patterns" #-}

import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import Data.Maybe (mapMaybe)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Word (Word16)
import Data.Kind (Type)
import Data.Typeable
import Data.Void

data Header = Header
  { headerHash       :: HeaderHash
  , headerParent     :: HeaderHash
  , headerSlot       :: Slot
  , headerBlockCount :: BlockCount
  }
  deriving (Show)
newtype Body = Body
  { getBody :: ByteString
  }
  deriving (Show)
type Block = (Header, Body)
type Chain = NonEmpty Block
type HeaderHash = ByteString
type Slot = Word
type BlockCount = Word

tipHeader :: Chain -> Header
tipHeader = fst . NE.head

tipBody :: Chain -> Body
tipBody = snd . NE.head

blockCount :: Header -> Word
blockCount = headerBlockCount

-- | Comparison based on block count. As usual, the result refers to the
-- left-hand-side: LT means it is less than the right-hand-side.
compareBlockCount :: Header -> Header -> Ordering
compareBlockCount left right = blockCount left `compare` blockCount right

-- | Comparison based on slot count. As usual, the result refers to the
-- left-hand-side: LT means it is less than the right-hand-side.
compareSlot :: Header -> Header -> Ordering
compareSlot left right = headerSlot left `compare` headerSlot right

-- | Comparison based on block count, breaking ties using slot count (higher
-- is better).
compareHeaders :: Header -> Header -> Ordering
compareHeaders left right = case left `compareBlockCount` right of
  LT -> LT
  GT -> GT
  EQ -> left `compareSlot` right

-- | Does the header immediately continue the chain?
continues :: Header -> Chain -> Bool
continues header chain = headerParent header == headerHash (tipHeader chain)

-- | For resolving an intersection / read pointer.
-- In practice we'll want to limit the number of these which can be sent in
-- one message.
type Checkpoints = [Header]

-- |
-- = Using a type transition system to describe either side of a
-- request/response application.

-- | In a client/server model, one side will be awaiting when the other is
-- receiving. This type will be included in a description of either side of
-- the a protocol application. See 'Peer'.
data Status k where
  Awaiting :: k -> Status k
  Yielding :: k -> Status k

-- | Description of control flow a given instance: either hold onto it or
-- release it.
data Control where
  Hold    :: Control
  Release :: Control

-- | 'p' partitions 'k' by picking, for each 'st :: k', one of two options,
-- here called 'black' and 'white'. Church encoding of boolean.
type family Partition (p :: r) (st :: k) (black :: t) (white :: t) :: t

-- | Picks 'Hold if 'from' and 'to' lie in the same partition.
-- Picks 'Release otherwise.
type TrControl p from to =
  Partition p from (Partition p to 'Hold 'Release)
                   (Partition p to 'Release 'Hold)

-- | This family is used in the definition of 'Peer' to determine whether, given
-- a parituclar transition, a yielding side continues to yield or begins
-- awaiting, and vice verse for an awaiting side.
type family ControlNext control a b where
  ControlNext 'Hold    a b = a
  ControlNext 'Release a b = b

-- | Included in every yield, this gives not only the transition, but a
-- 'Control' code to hold or release. The constructor for yielding will
-- require that 'Hold or 'Release is equal to an application of 'TrControl'
-- on the relevant state endpoints. In this way, every transition either
-- picks out exactly one of 'Hold or 'Release, or the type family is "stuck"
-- and the program won't type check; it means you can't have a transition which
-- unifies with _both_ 'Hold and 'Release, which would allow the yielding and
-- awaiting sides to get out of sync.
data Exchange p tr from to control where
  Part :: tr from to -> Exchange p tr from to 'Hold
  -- | Radio terminology. End of transmission and response is expected.
  Over :: tr from to -> Exchange p tr from to 'Release

-- | Get the transition from an 'Exchange'.
exchangeTransition :: Exchange p tr from to control -> tr from to
exchangeTransition (Part tr) = tr
exchangeTransition (Over tr) = tr

-- | This is somewhat like a pipe or conduit, but it's one-sided. It yields
-- to and awaits from the same end. The type of thing which is produced or
-- expected depends upon the type transition system 'tr', which also controls,
-- by way of 'p' and the type family 'Partition p', at what point a yielder
-- becomes an awaiter.
data Peer p (tr :: st -> st -> Type) (from :: Status st) (to :: Status st) m t where
  PeerDone :: t -> Peer p tr end end m t
  PeerEffect :: m (Peer p tr from to m t) -> Peer p tr from to m t
  -- | When yielding a transition, there must be proof that, under 'p', the
  -- transition either holds or releases control. This is enough to allow GHC
  -- to deduce that the continuation in a complementary 'PeerAwait' (one which
  -- is awaiting on the same initial state) will either
  --   - continue to await if this one continues to yield (hold)
  --   - begin to yield if this one begins to await (release)
  -- See the defition of 'connect', where this property is used explicitly.
  --
  -- Typeable is here for the benefit of casting a transition on unknown
  -- endpoints into one on a known endpoint, to facilitate passing them
  -- through a channel. It's also in the 'PeerAwait' constructor.
  PeerYield
    :: ( Typeable from
       , TrControl p from inter ~ control
       )
    => Exchange p tr from inter control
    -> Peer p tr (ControlNext control 'Yielding 'Awaiting inter) to m t
    -> Peer p tr ('Yielding from) to m t
  PeerAwait
    :: ( Typeable from )
    => (forall inter . tr from inter -> Peer p tr (ControlNext (TrControl p from inter) 'Awaiting 'Yielding inter) to m t)
    -> Peer p tr ('Awaiting from) to m t

done :: t -> Peer p tr end end m t
done = PeerDone

effect :: Functor m => m t -> Peer p tr end end m t
effect = PeerEffect . fmap done

-- | Akin to '>>=' but the continuation carries it from the 'inter'mediate
-- state to the end state.
andThen
  :: ( Functor m )
  => Peer p tr from inter m s
  -> (s -> Peer p tr inter to m t)
  -> Peer p tr from to m t
andThen peer k = case peer of
  PeerDone t -> k t
  PeerEffect m -> PeerEffect $ (fmap (`andThen` k) m)
  PeerYield tr next -> PeerYield tr $ next `andThen` k
  PeerAwait l -> PeerAwait $ (`andThen` k) . l

feedbackLoop
  :: ( Functor m )
  => (r -> Peer p tr st st m r)
  -> (r -> Peer p tr st sx m x)
feedbackLoop k r = k r `andThen` feedbackLoop k

type Server p tr from to m = Peer p tr ('Awaiting from) ('Awaiting to) m Void
type Client p tr from to m t = Peer p tr ('Yielding from) ('Yielding to) m t

-- | Connect a server and client.
-- The output gives the client's value and the remainder of the server, which
-- by construction never finishes.
connect
  :: forall p tr from to x m t .
     ( Monad m )
  => Server p tr from to m
  -> Client p tr from to m t
  -> m (t, Server p tr to to m)
connect (PeerDone void) _                = absurd void
connect server          (PeerDone t)     = pure (t, server)
connect server          (PeerEffect m)   = m >>= connect server
connect (PeerEffect m)  client           = m >>= flip connect client
connect (PeerAwait k)  (PeerYield exchange n) = case exchange of
  Part tr -> connect (k tr) n
  Over tr -> over (k tr) n

  where

  over
    :: forall p tr from to x m t .
       ( Monad m )
    => Peer p tr ('Yielding from) ('Awaiting to) m Void
    -> Peer p tr ('Awaiting from) ('Yielding to) m t
    -> m (t, Peer p tr ('Awaiting to) ('Awaiting to) m Void)
  over server                 (PeerEffect m) = m >>= over server
  over (PeerEffect m)         client         = m >>= flip over client
  over (PeerYield exchange n) (PeerAwait k)  = case exchange of
    Part tr -> over n (k tr)
    Over tr -> connect n (k tr)

-- |
-- = Definition of the chain exchange protocol, and a factoring of it into
-- consumer and producer sides.

-- | There are 2 state forms, but the busy state is factored by 'RequestKind'
-- so that the appropriate response can be determined.
data StChainExchange where
  StIdle :: StChainExchange
  StBusy :: RequestKind -> StChainExchange

-- | Transitions on 'StChainExchange'. A request goes from idle to busy, and
-- a response may go from busy to idle, or stay on busy in case of multi-part
-- responses for headers and bodies.
data TrChainExchange stFrom stTo where
  TrRequest  :: Request  req     -> TrChainExchange 'StIdle ('StBusy req)
  TrRespond  :: Response req res -> TrChainExchange ('StBusy req) res

showTrChainExchange :: TrChainExchange from to -> String
showTrChainExchange tr = case tr of
  TrRequest (ReqSetHead hhs) -> "Req set head " ++ show hhs
  TrRequest _ -> "Other request"
  TrRespond (ResSetHead hh) -> "Res set head " ++ show hh
  TrRespond _ -> "Other response"

data RequestKind where
  SetHead :: RequestKind
  Headers :: RequestKind
  Bodies  :: RequestKind

-- | There are 3 types of requests.
data Request (req :: RequestKind) where
  ReqSetHead     :: NonEmpty HeaderHash ->    Request 'SetHead
  ReqHeaders     :: Word16       ->           Request 'Headers
  ReqBodies      :: HeaderHash   -> Word16 -> Request 'Bodies

-- | Fork and extend can be responses to any request.
-- The headers and bodies responses are multi-part: an individual data point
-- can be sent, or the response can be closed, returning the state to idle.
data Response (req :: RequestKind) (res :: StChainExchange) where
  -- | New tip (header) and rollback point (header hash).
  ResChange      :: Header -> HeaderHash -> Response anything     'StIdle
  ResSetHead     :: HeaderHash ->           Response 'SetHead     'StIdle
  ResHeadersOne  :: Header ->               Response 'Headers     ('StBusy 'Headers)
  ResHeadersDone ::                         Response 'Headers     'StIdle
  ResBodiesOne   :: HeaderHash -> Body ->   Response 'Bodies      ('StBusy 'Bodies)
  ResBodiesDone  ::                         Response 'Bodies      'StIdle

-- |
-- = Paritioning into client/server or consumer/producer
--
-- The above transition system 'TrChainExchange' makes no explicit mention
-- of a client and a server. In theory, the protocol could be run by one
-- process. In order to write separate client and server sides which are
-- complementary, we must partition the states into 2 sets.

-- | A type to identify our client/server parition.
data ChainExchange

type instance Partition ChainExchange st client server =
  ChainExchangePartition st client server

-- | Idle states are client, producer states are server.
type family ChainExchangePartition st client server where
  ChainExchangePartition 'StIdle       client server = client
  ChainExchangePartition ('StBusy res) client server = server

-- |
-- = Example producer and consumer implementations.

data Zipper t = Zipper [t] t [t]
  deriving (Show)

focus :: Zipper t -> t
focus (Zipper _ t _) = t

zipTo :: (t -> Bool) -> Zipper t -> Maybe (Zipper t)
zipTo p (Zipper before here after) = case List.break p after of
  (_, []) -> Nothing
  (pre, (x:post)) -> Just $ Zipper (before ++ [here] ++ pre) x post

zipNext :: Zipper t -> Maybe (t, Zipper t)
zipNext (Zipper _ _ []) = Nothing
zipNext (Zipper before here (next:after)) = Just (next, Zipper (before ++ [here]) next after)

-- | Produces a given chain at a given read pointer, represented by a zipper.
-- It serves one request (idle to idle) and gives the new zipper, which may
-- have a different focus (read pointer).
zipperServer
  :: ( Monad m )
  => Zipper Block
  -> Peer ChainExchange TrChainExchange ('Awaiting 'StIdle) ('Awaiting 'StIdle) m (Zipper Block)
zipperServer z = PeerAwait $ \tr -> case tr of

  TrRequest (ReqSetHead cps) ->
    let zs = mapMaybe (\cp -> zipTo ((==) cp . headerHash . fst) z) (NE.toList cps)
        sorted = List.sortOn (headerSlot . fst . focus) zs
    in  case sorted of
          [] -> PeerYield (Over (TrRespond (ResSetHead (headerHash (fst (focus z)))))) (done z)
          (z' : _) -> PeerYield (Over (TrRespond (ResSetHead (headerHash (fst (focus z')))))) (done z')

  TrRequest (ReqHeaders num)     -> respondHeaders z num

  TrRequest (ReqBodies from num) -> respondBodies z z num

  where

  respondHeaders
    :: ( Monad m )
    => Zipper Block
    -> Word16
    -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy 'Headers)) ('Awaiting 'StIdle) m (Zipper Block)
  respondHeaders z 0 = PeerYield (Over (TrRespond ResHeadersDone)) (done z)
  respondHeaders z n = case zipNext z of
    Nothing -> PeerYield (Over (TrRespond ResHeadersDone)) (done z)
    Just (b, z') -> PeerYield (Part (TrRespond (ResHeadersOne (fst b)))) (respondHeaders z' (n-1))

  respondBodies
    :: ( Monad m )
    => a
    -> Zipper Block
    -> Word16
    -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy 'Bodies)) ('Awaiting 'StIdle) m a
  respondBodies a z 0 = PeerYield (Over (TrRespond ResBodiesDone)) (done a)
  respondBodies a z n = case zipNext z of
    Nothing -> PeerYield (Over (TrRespond ResBodiesDone)) (done a)
    Just (b, z') -> PeerYield (Part (TrRespond (ResBodiesOne (headerHash (fst b)) (snd b)))) (respondBodies a z' (n-1))

data BinarySearch t where
  BstEmpty :: BinarySearch t
  BstSplit :: BinarySearch t -> t -> BinarySearch t -> BinarySearch t

-- | Finds the intersection by binary search using the client-side of the
-- protocol.
findIntersection
  :: ( Monad m )
  => BinarySearch HeaderHash
  -> HeaderHash
  -> Client ChainExchange TrChainExchange 'StIdle 'StIdle m HeaderHash
findIntersection bst bestSoFar = case bst of
  BstEmpty -> done bestSoFar
  BstSplit older t newer -> PeerYield (Over (TrRequest (ReqSetHead (pure t)))) $ PeerAwait $ \tr -> case tr of
    TrRespond (ResSetHead hh) ->
      if hh == t
      then findIntersection newer t
      else findIntersection older hh
    TrRespond (ResChange _ hh) -> findIntersection bst hh

data DownloadedHeaders where
  Forked :: [Header] -> Header -> DownloadedHeaders
  Complete :: [Header] -> DownloadedHeaders
  deriving (Show)

-- | Downloads headers using the client side of the protocol, stopping when
-- it's either finished, or the server side forks.
downloadHeaders
  :: ( Monad m )
  => Word16
  -> [Header]
  -> Client ChainExchange TrChainExchange 'StIdle 'StIdle m DownloadedHeaders
downloadHeaders howMany acc = PeerYield (Over (TrRequest (ReqHeaders howMany))) (downloadLoop howMany acc)
  where
  downloadLoop
    :: ( Monad m )
    => Word16
    -> [Header]
    -> Peer ChainExchange TrChainExchange ('Awaiting ('StBusy 'Headers)) ('Yielding 'StIdle) m DownloadedHeaders
  downloadLoop !remaining !acc = PeerAwait $ \tr -> case tr of
    TrRespond ResHeadersDone -> done $ Complete (reverse acc)
    TrRespond (ResHeadersOne h) -> downloadLoop (remaining - 1) (h : acc)
    TrRespond (ResChange tip hh) -> done $ Forked (reverse acc) tip

-- | Construct an example chain from a list of hashes for slots: 'Nothing'
-- means no block, 'Just h' means the block at this slot has hash 'h'. The
-- block count, slot numbers, and parent hash are filled in. The static
-- 'genesisBlock' is used as the root.
mkChain :: [Maybe ByteString] -> NonEmpty Block
mkChain bs = genesisBlock NE.:| genChain "genesis" 0 0 bs
  where
  genChain :: ByteString -> Slot -> BlockCount -> [Maybe ByteString] -> [Block]
  genChain parentHash sl bc [] = []
  genChain parentHash sl bc (Just bl : rest) = (Header bl parentHash sl bc, Body bl) : genChain bl (sl+1) (bc+1) rest
  genChain parentHash sl bc (Nothing : rest) = genChain parentHash (sl+1) bc rest

zipperFromChain :: NonEmpty Block -> Zipper Block
zipperFromChain (genesis NE.:| rest) = Zipper [] genesis rest

-- Requires a list of length 2^n.
bstFromChain :: [t] -> BinarySearch t
bstFromChain lst = case List.splitAt (List.length lst `div` 2) lst of
  ([], []) -> BstEmpty
  (_:_, []) -> error "bstFromChain: impossible splitAt case"
  -- If the list is length 1, we'll splitAt 0 and get an empty first list.
  ([], [t]) -> BstSplit BstEmpty t BstEmpty
  ([], _:_:_) -> error "split length 1 list"
  (older, mid : newer) -> BstSplit (bstFromChain older) mid (bstFromChain newer)

genesisBlock :: Block
genesisBlock = (Header "0" "genesis" 0 0, Body "genesis")

exampleChain1 :: NonEmpty Block
exampleChain1 = mkChain [Just "A", Nothing, Just "B", Just "C", Nothing, Just "D"]

exampleChain2 :: NonEmpty Block
exampleChain2 = mkChain [Just "A", Nothing, Just "B", Just "C", Just "E", Just "F", Nothing, Just "G"]

exampleChain3 :: NonEmpty Block
exampleChain3 = mkChain [Just "A", Just "H", Nothing, Nothing, Just "I"]

server :: Monad m => NonEmpty Block -> Server ChainExchange TrChainExchange 'StIdle anything m
server chain = feedbackLoop zipperServer (zipperFromChain chain)

client :: Monad m => NonEmpty Block -> Client ChainExchange TrChainExchange 'StIdle 'StIdle m DownloadedHeaders
client chain =
  (findIntersection
    (bstFromChain (fmap (headerHash . fst) (NE.toList chain)))
    (headerHash (fst genesisBlock)))
  `andThen`
  (\_ -> downloadHeaders 32 [])

app
  :: ( Monad m )
  => NonEmpty Block
  -> NonEmpty Block
  -> m DownloadedHeaders -- Continuation TrChainExchange 'StIdle 'StIdle m (Zipper Block) DownloadedHeaders)
app serverChain clientChain = fmap fst (connect (server serverChain) (client clientChain))


-- Next step: communication through some channel.
-- We'll factor it like this:
--
--   - There must be a serialisation of (exists from to . tr from to).
--     This may give the 'Malformed' case.
--   - There must be a type-safe cast
--       forall st . SomeTransition -> Either Unexpected (exists next . tr st next)
--     If the state type is Typeable then we can do that.
--
-- Really the care must be taken in the encode/decode, as that's where things
-- could go awry. It could decode properly, and into an expected transition,
-- putting both ends into non-complementary positions: possibility of deadlock,
-- of both sides yielding, or of an unexpected transition coming in _later on_.

data SomeTransition tr where
  -- | Must ensure Typeable here, because we can't do it at 'castTransition'.
  -- That would require stating that every type in the state kind is
  -- typeable. Quantified constraints could help, although a short attempt at
  -- that did not work.
  SomeTransition :: ( Typeable from ) => tr from to -> SomeTransition tr

data TransitionFrom tr from where
  Expected   :: tr from to -> TransitionFrom tr from
  Unexpected :: TransitionFrom tr from

showTransitionFrom :: TransitionFrom tr from -> String
showTransitionFrom (Expected _) = "Expected"
showTransitionFrom (Unexpected) = "Unexpected"

castTransition
  :: forall st (tr :: st -> st -> Type) (from :: st) .
     ( Typeable from )
  => Proxy from
  -> SomeTransition tr
  -> TransitionFrom tr from
castTransition _ (SomeTransition (it :: tr from' to)) = case eqT of
  Just (Refl :: from :~: from') -> Expected it
  Nothing -> Unexpected

data Channel m t = Channel
  { send :: t -> m ()
  , recv :: m t
  }

useChannel
  :: forall p tr status from to m t .
     ( Monad m )
  => Channel m (SomeTransition tr)
  -> Peer p tr (status from) to m t
  -> m (Either String t)
useChannel chan peer = case peer of
  PeerDone t -> pure (Right t)
  PeerEffect m -> m >>= useChannel chan
  PeerAwait k -> recv chan >>= \some -> case castTransition (Proxy :: Proxy from) some of
    Unexpected -> pure $ Left "Unexpected transition"
    Expected tr -> useChannel chan (k tr)
  PeerYield exch next -> do
    send chan (SomeTransition (exchangeTransition exch))
    useChannel chan next

withChannels :: (Channel IO t -> Channel IO t -> IO r) -> IO r
withChannels k = do
  left <- newEmptyMVar
  right <- newEmptyMVar
  let leftChan  = Channel (putMVar left) (takeMVar right)
      rightChan = Channel (putMVar right) (takeMVar left)
  k leftChan rightChan

example :: NonEmpty Block -> NonEmpty Block -> IO ()
example serverChain clientChain = withChannels $ \serverChan clientChan ->
  withAsync (useChannel serverChan (server serverChain)) $ \serverAsync -> do
    t <- useChannel clientChan (client clientChain)
    print t

-- Next steps: demonstrations of
-- - forking
-- - duplex (producer is consumer)
-- - clusters (more than 2 peers)
-- So I think what's needed is a configurable and pure demo producer, capable
-- of forking. This will help in all the items.

-- We'll leave serialisation to later. I don't anticipate any problems there.
{-
type Encode tr serial m = forall from to . tr from to -> m serial

type Decode tr serial m = serial -> m (DecodedTransition tr serial)

data DecodedTransition tr serial where
  Malformed :: String -> serial -> DecodedTransition tr serial
  WellFormed :: tr from to -> serial -> DecodedTransition tr serial
-}
