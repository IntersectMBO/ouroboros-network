{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE BangPatterns #-}

import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad (ap, forM_, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Constraint
import Data.Foldable (find)
import Data.Functor.Compose
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (isJust, mapMaybe)
import qualified Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Semigroup (Semigroup (..))
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Data.Word (Word16)
import qualified Debug.Trace as Debug
import Data.Void
import Data.Kind (Type)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import System.IO.Error (userError)

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

-- Next step: communication through an effectful channel.
-- There's a problem with receiving. Apparently we need the previous transition
-- before we receive. But then, we don't have that at the start of the server.
-- Could use 'TestEquality': pull out 'SomeTransition' then try to cast it
-- and give unexpected if no.
-- However, we also need to know when to pull the 'TrContinue' transition.
--

-- When you send, you have to get the next send/rev.
-- When you receive, you have to get the next transition along with the
-- next send/recv.
--
-- So, what we need is
--
--   send :: forall from to . tr from to -> (m (), Next tr to)
--   recv :: (m (exists from to . (tr from to, forall to' . tr from to' -> Next tr to')))
--
-- Or perhaps: when you send, you get the next receive.
-- When you receive, you get the next send.
--

{-
data Sent tr to m where
  Sent :: m () -> Recv tr to m -> Sent tr to m

type Send tr from m = forall to . tr from to -> Sent tr to m -- (m (), Recv tr to m)

data Received tr from m where
  Received :: tr from to -> Send tr to m -> Received tr from m

type Recv tr from m = m (Received tr from m)

data Channel tr from m = Channel
  { send :: Send tr from m
  , recv :: Recv tr from m
  }

data SomeTransition tr where
  SomeTransition :: tr from to -> SomeTransition tr

mvarSend :: MVar (SomeTransition TrChainExchange) -> Send TrChainExchange 'StIdle IO
mvarSend mvar = \tr -> case tr of
  TrRequest (ReqFastForward _) -> Sent (putMVar mvar (SomeTransition tr)) mvarRecvResponse
  TrRequest (ReqCheckpoints _) -> undefined
  TrRequest (ReqHeaders _)     -> undefined
  TrRequest (ReqBodies _ _)    -> undefined
-}

-- Ah no, we don't need an entire chain, we just need the next receive after
-- a send, and the next send after a receive.
-- 
--   -- Interested in (forall from . Send tr from m)
--   type Send tr from m = forall to . tr from to -> (m (), m (TransitionFrom tr to))
--   data TransitionFrom tr from where
--     Expected   :: tr from to -> Recv tr from
--     Unexpected :: String     -> Recv tr from
--
--   -- For receiving, you need to define how to get the next transition, from
--   -- this particular point, but also how to send...
--   type Recv tr from m = m (TransitionFrom tr from, 
--
--   data Received tr from m where
--     Received :: tr from inter -> (forall to . tr inter to -> m ()) -> Received tr from m
--
-- Wouldn't it be enough to have simply:
--   - for every transition, how to send it
--       forall from to . tr from to -> m ()
--   - for every transition, how to receive the next one.
--       forall from to . tr from to -> m (exists to' . tr to to')
-- That alone is enough for the client.
-- The server needs an initial one.
--   - how to receive the first transition
--       m (exists to . tr from to)
--
-- In the receive part, the transition parameter really is a nuisance. It's
-- only there so we can actually get a hold of the transition... another
-- option is to do it by typeclass.
--
--   class SendRecv tr from to m where
--     send :: tr from to -> m ()
--     recv :: m (Either Unexpected (tr from to))
--
-- then we can use quantified constraints?!
--
--   (forall from to . sendRecv tr from to m)
--
-- oooo good idea.

-- The following could be a nice solution, but it requires
-- QuantifiedConstraints, available from 8.6.
{-
class Channel (thing :: Type) (tr :: state -> state -> Type) (from :: state) (to :: state) (m :: Type -> Type) where
  send :: thing -> tr from to -> m ()
  recv :: thing -> m (Either String (tr from to))

qcons
  :: forall thing tr from to m .
     (forall x y . Channel thing tr x y m)
  => thing
  -> tr from to
  -> m ()
qcons thing tr = send thing tr
-}

-- So, back to the original idea.
--
--   data Channel tr from m = Channel
--     { send :: forall to . tr from to -> m ()
--     , recv :: m (Unexpected | exists to . tr from to)
--     , next :: forall to . tr from to -> Channel tr to m
--     }

{-
data Channel tr from m = Channel
  { send :: forall to . tr from to -> m ()
  , recv :: m (ReceivedTransition tr from)
  , next :: forall to . tr from to -> Channel tr to m
  }

-- forall tr from . (exists to . Either String (tr from to))
data ReceivedTransition tr from where
  Expected   :: tr from to -> ReceivedTransition tr from
  Unexpected :: String -> ReceivedTransition tr from
-}

-- NB we'll want one special case in which everything goes via serialisation:
-- give the encode/decode, a read/write stream, and you're set.
-- But how to account for the case of the phantom transition? Giving mempty
-- for the serialisation could work, but serialising something as mempty is
-- stupid

{-
chainExchangeChannel
  :: MVar (SomeTransition TrChainExchange)
  -> MVar (SomeTransition TrChainExchange)
  -> Channel TrChainExchange 'StIdle IO
chainExchangeChannel _ _ = Channel
  { send = \tr -> case tr of
      TrContinue -> pure ()
  , recv = 
  }
-}

-- What if we had
--
--   type Encode tr serial = forall from to . tr from to -> serial
--   type Decode tr serial = serial -> Either Malformed (exists from to . tr from to)
--
-- So then we have the notion of malformed (serialiser law violation), as well
-- as the notion of unexpected (serialised properly but an invalid transition).
--

type Encode tr serial m = forall from to . tr from to -> m serial

type Decode tr serial m = serial -> m (DecodedTransition tr serial)

data DecodedTransition tr serial where
  Malformed :: String -> serial -> DecodedTransition tr serial
  WellFormed :: tr from to -> serial -> DecodedTransition tr serial


data Status k where
  Awaiting :: k -> Status k
  Yielding :: k -> Status k

type family Complement (st :: Status k) :: Status k where
  Complement (Awaiting k) = Yielding k
  Complement (Yielding k) = Awaiting k

data Control where
  Hold    :: Control
  Release :: Control

-- | 'p' partitions 'k' by picking, for each 'st :: k', one of two options,
-- here called 'black' and 'white'.
type family Partition (p :: r) (st :: k) (black :: t) (white :: t) :: t

type TrControl p from to = Partition p from (Partition p to 'Hold 'Release) (Partition p to 'Release 'Hold)

type family ControlNext control a b where
  ControlNext 'Hold    a b = a
  ControlNext 'Release a b = b

-- | 'Next p from to current other = other'   iff 'Partition p from =  Partition p to'
--   'Next p from to current other = current' iff 'Partition p from /= Partition p to'
type Next p from to current other = Partition p from (Partition p to current other) (Partition p to other current)

data Exchange p tr from to control where
  Wait :: tr from to -> Exchange p tr from to 'Hold
  -- | Radio terminology. End of transmission and response is expected.
  Over :: tr from to -> Exchange p tr from to 'Release

data Peer p (tr :: st -> st -> Type) (from :: Status st) (to :: Status st) m t where
  PeerDone
    :: t
    -> Peer p tr end end m t
  PeerEffect
    :: m (Peer p tr from to m t)
    -> Peer p tr from to m t
  -- | When yielding a transition, there must be proof that, under 'p', the
  -- transition either holds or releases control. This is enough to allow GHC
  -- to deduce that the continuation in a complementary 'PeerAwait' (one which
  -- is awaiting on the same initial state) will either
  --   - continue to await if this one continues to yield (hold)
  --   - begin to yield if this one begins to await (release)
  -- See the defition of 'connect', where this property is used explicitly.
  PeerYield
    :: ( TrControl p from inter ~ control )
    => Exchange p tr from inter control
    -> Peer p tr (ControlNext control 'Yielding 'Awaiting inter) to m t
    -> Peer p tr ('Yielding from) to m t
  PeerAwait
    :: (forall inter . tr from inter -> Peer p tr (ControlNext (TrControl p from inter) 'Awaiting 'Yielding inter) to m t)
    -> Peer p tr ('Awaiting from) to m t

done :: t -> Peer p tr end end m t
done = PeerDone

effect :: Functor m => m t -> Peer p tr end end m t
effect = PeerEffect . fmap done

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
  Wait tr -> connect (k tr) n
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
    Wait tr -> over n (k tr)
    Over tr -> connect n (k tr)

data StChainExchange where
  StIdle :: StChainExchange
  StBusy :: RequestKind -> StChainExchange

data TrChainExchange stFrom stTo where
  TrRequest  :: Request  req     -> TrChainExchange 'StIdle ('StBusy req)
  TrRespond  :: Response req res -> TrChainExchange ('StBusy req) res

showTrChainExchange :: TrChainExchange from to -> String
showTrChainExchange tr = case tr of
  TrRequest (ReqCheckpoints hhs) -> "Req checkpoints " ++ show hhs
  TrRequest (ReqFastForward hh) -> "Req fast-forward " ++ show hh
  TrRequest _ -> "Other request"
  TrRespond (ResCheckpoints hh) -> "Res checkpoints " ++ show hh
  TrRespond (ResFastForward hh) -> "Res fast-forward " ++ show hh
  TrRespond _ -> "Other response"

data RequestKind where
  FastForward :: RequestKind
  Checkpoints :: RequestKind
  Headers     :: RequestKind
  Bodies      :: RequestKind

data Request (req :: RequestKind) where
  ReqFastForward :: HeaderHash   ->           Request 'FastForward
  ReqCheckpoints :: [HeaderHash] ->           Request 'Checkpoints
  ReqHeaders     :: Word16       ->           Request 'Headers
  ReqBodies      :: HeaderHash   -> Word16 -> Request 'Bodies

data Response (req :: RequestKind) (res :: StChainExchange) where
  ResFork        :: Header -> HeaderHash -> Response anything     'StIdle
  ResExtend      :: Header               -> Response anything     'StIdle
  ResFastForward :: HeaderHash           -> Response 'FastForward 'StIdle
  ResCheckpoints :: HeaderHash           -> Response 'Checkpoints 'StIdle
  ResHeadersOne  :: Header               -> Response 'Headers     ('StBusy 'Headers)
  ResHeadersDone ::                         Response 'Headers     'StIdle
  ResBodiesOne   :: HeaderHash -> Body   -> Response 'Bodies      ('StBusy 'Bodies)
  ResBodiesDone  ::                         Response 'Bodies      'StIdle

-- | A type to identify our client/server chain exchange.
data ChainExchange

type instance Partition ChainExchange st client server = ChainExchangePartition st client server

type family ChainExchangePartition st client server where
  ChainExchangePartition 'StIdle       client server = client
  ChainExchangePartition ('StBusy res) client server = server


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

  TrRequest (ReqFastForward hto) -> case zipTo ((==) hto . headerHash . fst) z of
    Nothing -> PeerYield (Over (TrRespond (ResFastForward (headerHash (fst (focus z)))))) (done z)
    Just it -> PeerYield (Over (TrRespond (ResFastForward (headerHash (fst (focus it)))))) (done it)

  TrRequest (ReqCheckpoints cps) ->
    let zs = mapMaybe (\cp -> zipTo ((==) cp . headerHash . fst) z) cps
        sorted = List.sortOn (headerSlot . fst . focus) zs
    in  case sorted of
          [] -> PeerYield (Over (TrRespond (ResCheckpoints (headerHash (fst (focus z)))))) (done z)
          (z' : _) -> PeerYield (Over (TrRespond (ResCheckpoints (headerHash (fst (focus z')))))) (done z')

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
    Just (b, z') -> PeerYield (Wait (TrRespond (ResHeadersOne (fst b)))) (respondHeaders z' (n-1))

  respondBodies
    :: ( Monad m )
    => a
    -> Zipper Block
    -> Word16
    -> Peer ChainExchange TrChainExchange ('Yielding ('StBusy 'Bodies)) ('Awaiting 'StIdle) m a
  respondBodies a z 0 = PeerYield (Over (TrRespond ResBodiesDone)) (done a)
  respondBodies a z n = case zipNext z of
    Nothing -> PeerYield (Over (TrRespond ResBodiesDone)) (done a)
    Just (b, z') -> PeerYield (Wait (TrRespond (ResBodiesOne (headerHash (fst b)) (snd b)))) (respondBodies a z' (n-1))

data BinarySearch t where
  BstEmpty :: BinarySearch t
  BstSplit :: BinarySearch t -> t -> BinarySearch t -> BinarySearch t

-- | Finds the intersection by binary search.
findIntersection
  :: ( Monad m )
  => BinarySearch HeaderHash
  -> HeaderHash
  -> Client ChainExchange TrChainExchange 'StIdle 'StIdle m HeaderHash
findIntersection bst bestSoFar = case bst of
  BstEmpty -> done bestSoFar
  BstSplit older t newer -> PeerYield (Over (TrRequest (ReqFastForward t))) $ PeerAwait $ \tr -> case tr of
    TrRespond (ResFastForward hh) ->
      if hh == t
      then findIntersection newer t
      else findIntersection older hh
    TrRespond (ResExtend _) -> findIntersection bst bestSoFar
    TrRespond (ResFork _ hh) -> findIntersection bst hh

data DownloadedHeaders where
  Forked :: [Header] -> Header -> DownloadedHeaders
  Complete :: [Header] -> DownloadedHeaders
  deriving (Show)

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
    TrRespond (ResExtend _) -> downloadHeaders remaining acc
    TrRespond (ResFork tip hh) -> done $ Forked (reverse acc) tip

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
