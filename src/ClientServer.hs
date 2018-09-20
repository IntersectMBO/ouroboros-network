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
{-# LANGUAGE BangPatterns #-}

--import Control.Concurrent.Async
import Control.Concurrent.MVar
import Control.Exception (throwIO)
import Control.Monad (ap, forM_, unless)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
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

data StChainExchange where
  StIdle      :: StChainExchange
  StRequested :: RequestKind -> StChainExchange
  StBusy      :: RequestKind -> StChainExchange

data TrChainExchange stFrom stTo where
  TrRequest  :: Request  req     -> TrChainExchange 'StIdle ('StRequested req)
  TrRespond  :: Response req res -> TrChainExchange ('StRequested req) res
  TrContinue :: TrChainExchange ('StBusy req) ('StRequested req)

showTrChainExchange :: TrChainExchange from to -> String
showTrChainExchange tr = case tr of
  TrRequest (ReqCheckpoints hhs) -> "Req checkpoints " ++ show hhs
  TrRequest (ReqFastForward hh) -> "Req fast-forward " ++ show hh
  TrRequest _ -> "Other request"
  TrRespond (ResCheckpoints hh) -> "Res checkpoints " ++ show hh
  TrRespond (ResFastForward hh) -> "Res fast-forward " ++ show hh
  TrRespond _ -> "Other response"
  TrContinue -> "Continue"

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

newtype Client tr begin end m t = Client
  { runClient :: m (ClientRequest tr begin end m t)
  }

data ClientRequest tr begin end m t where
  ClientYield
    :: tr begin inter
    -> (forall next . tr inter next -> Client tr next end m t)
    -> ClientRequest tr begin end m t
  ClientIncomplete :: t -> ClientRequest tr end end m t

clientPure :: Applicative m => t -> Client tr end end m t
clientPure = Client . pure . ClientIncomplete

clientSequence
  :: ( Monad m )
  => Client tr begin inter m s
  -> (s -> Client tr inter end m t)
  -> Client tr begin end m t
clientSequence client k = Client $ runClient client >>= \clientReq -> case clientReq of
  ClientIncomplete t -> runClient (k t)
  ClientYield tr l -> pure $ ClientYield tr $ \tr' -> clientSequence (l tr') k

clientFeedback
  :: ( Monad m )
  => (r -> Client tr end end m r)
  -> (r -> Client tr end x m y)
clientFeedback k r = clientSequence (k r) (clientFeedback k)

newtype Server tr begin end m t = Server
  { runServer :: m (ServerResponse tr begin end m t)
  }

data ServerResponse tr begin end m t where
  ServerAwait
    :: (forall inter . tr begin inter -> m (Respond tr inter end m t))
    -> ServerResponse tr begin end m t
  ServerIncomplete :: t -> ServerResponse tr end end m t

data Respond tr begin end m t where
  Respond
    :: tr begin inter
    -> Server tr inter end m t
    -> Respond tr begin end m t

serverPure :: Applicative m => t -> Server tr end end m t
serverPure = Server . pure . ServerIncomplete

serverSequence
  :: ( Monad m )
  => Server tr begin inter m s
  -> (s -> Server tr inter end m t)
  -> Server tr begin end m t
serverSequence server k = Server $ runServer server >>= \serverRes -> case serverRes of
  ServerIncomplete t -> runServer (k t)
  ServerAwait l -> pure $ ServerAwait $ \tr -> l tr >>= \res -> case res of
    Respond tr' server' -> pure $ Respond tr' (serverSequence server' k)

serverFeedback
  :: ( Monad m )
  => (r -> Server tr end end m r)
  -> (r -> Server tr end x m y)
serverFeedback k r = serverSequence (k r) (serverFeedback k)

data Continuation tr endS endC m s c where
  ClientServerDone :: s -> c -> Continuation tr endS endC m s c
  ServerContinuation
    :: s
    -> tr endS inter
    -> (forall next . tr inter next -> Client tr next endC m t)
    -> Continuation tr endS endC m s c
  ClientContinuation
    :: c
    -> (forall inter . tr endC inter -> m (Respond tr inter endS m t))
    -> Continuation tr endS endC m s c

showContinuation :: (s -> String) -> (c -> String) -> Continuation tr endS endC m s c -> String
showContinuation showS showC (ClientServerDone s c) = "server: (" ++ showS s ++ "), client: (" ++ showC c ++ ")"
showContinuation showS _ (ServerContinuation s _ _) = "server: (" ++ showS s ++ "), client: (starved)"
showContinuation _ showC (ClientContinuation c _) = "server: (starved), client: (" ++ showC c ++ ")"

-- | Run a client and server in some common monad.
-- Client effects go first (an arbitrary choice).
serve
  :: ( Monad m )
  => Server tr begin endS m s
  -> Client tr begin endC m c
  -> m (Continuation tr endS endC m s c)
serve server client = do
  creq <- runClient client
  sres <- runServer server
  case (sres, creq) of
    (ServerIncomplete s, ClientIncomplete c) -> pure $ ClientServerDone s c
    (ServerIncomplete s, ClientYield tr ck) -> pure $
      ServerContinuation s tr ck
    (ServerAwait sk, ClientIncomplete c) -> pure $
      ClientContinuation c sk
    (ServerAwait sk, ClientYield tr ck) -> sk tr >>= \res -> case res of
      Respond tr' server' -> serve server' (ck tr')

-- | Produces a given chain at a given read pointer, represented by a zipper.
-- It serves one request (idle to idle) and gives the new zipper, which may
-- have a different focus (read pointer).
zipperServer
  :: ( Monad m )
  => Zipper Block
  -> Server TrChainExchange 'StIdle 'StIdle m (Zipper Block)
zipperServer z = Server $ pure $ ServerAwait $ \tr -> case tr of

  TrRequest (ReqFastForward hto) -> pure $ case zipTo ((==) hto . headerHash . fst) z of
    Nothing -> Respond (TrRespond (ResFastForward (headerHash (fst (focus z))))) $ serverPure z
    Just it -> Respond (TrRespond (ResFastForward (headerHash (fst (focus it))))) $ serverPure it

  TrRequest (ReqCheckpoints cps) ->
    let zs = mapMaybe (\cp -> zipTo ((==) cp . headerHash . fst) z) cps
        sorted = List.sortOn (headerSlot . fst . focus) zs
    in  pure $ case sorted of
          [] -> Respond (TrRespond (ResCheckpoints (headerHash (fst (focus z))))) $ serverPure z
          (z' : _) -> Respond (TrRespond (ResCheckpoints (headerHash (fst (focus z'))))) $ serverPure z'

  TrRequest (ReqHeaders num)     -> pure $ respondHeaders z num

  TrRequest (ReqBodies from num) -> pure $ respondBodies z z num

  where

  respondHeaders
    :: ( Monad m )
    => Zipper Block
    -> Word16
    -> Respond TrChainExchange ('StRequested 'Headers) 'StIdle m (Zipper Block)
  respondHeaders z 0 = Respond (TrRespond ResHeadersDone) $ serverPure z
  respondHeaders z n = case zipNext z of
    Nothing -> Respond (TrRespond ResHeadersDone) $ serverPure z
    Just (b, z') -> Respond (TrRespond (ResHeadersOne (fst b))) $ Server $ pure $ ServerAwait $ \tr -> case tr of
      TrContinue -> pure $ respondHeaders z' (n-1)

  respondBodies
    :: ( Monad m )
    => a
    -> Zipper Block
    -> Word16
    -> Respond TrChainExchange ('StRequested 'Bodies) 'StIdle m a
  respondBodies a z 0 = Respond (TrRespond ResBodiesDone) $ serverPure a
  respondBodies a z n = case zipNext z of
    Nothing -> Respond (TrRespond ResBodiesDone) $ serverPure a
    Just (b, z') -> Respond (TrRespond (ResBodiesOne (headerHash (fst b)) (snd b))) $ Server $ pure $ ServerAwait $ \tr -> case tr of
      TrContinue -> pure $ respondBodies a z' (n-1)

data BinarySearch t where
  BstEmpty :: BinarySearch t
  BstSplit :: BinarySearch t -> t -> BinarySearch t -> BinarySearch t

-- | Finds the intersection by binary search.
findIntersection
  :: ( Monad m )
  => BinarySearch HeaderHash
  -> HeaderHash
  -> Client TrChainExchange 'StIdle 'StIdle m HeaderHash
findIntersection bst bestSoFar = case bst of
  BstEmpty -> clientPure bestSoFar
  BstSplit older t newer -> Client $ pure $ ClientYield (TrRequest (ReqFastForward t)) $ \tr -> case tr of
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
  -> Client TrChainExchange 'StIdle 'StIdle m DownloadedHeaders
downloadHeaders howMany acc = Client $ pure $ ClientYield (TrRequest (ReqHeaders howMany)) (downloadLoop howMany acc)
  where
  downloadLoop
    :: ( Monad m )
    => Word16
    -> [Header]
    -> TrChainExchange ('StRequested 'Headers) next
    -> Client TrChainExchange next 'StIdle m DownloadedHeaders
  downloadLoop !remaining !acc = \tr -> case tr of
    TrRespond ResHeadersDone -> clientPure $ Complete (reverse acc)
    TrRespond (ResHeadersOne h) -> Client $ pure $ ClientYield TrContinue $ downloadLoop (remaining - 1) (h : acc)
    TrRespond (ResExtend _) -> downloadHeaders remaining acc
    TrRespond (ResFork tip hh) -> clientPure $ Forked (reverse acc) tip

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

client :: Monad m => NonEmpty Block -> Client TrChainExchange 'StIdle 'StIdle m DownloadedHeaders
client chain =
  clientSequence
    (findIntersection
      (bstFromChain (fmap (headerHash . fst) (NE.toList chain)))
      (headerHash (fst genesisBlock)))
    (\_ -> downloadHeaders 32 [])

server :: Monad m => NonEmpty Block -> Server TrChainExchange 'StIdle anything m anything'
server chain = serverFeedback zipperServer (zipperFromChain chain)

app
  :: Monad m
  => NonEmpty Block
  -> NonEmpty Block
  -> m (Continuation TrChainExchange 'StIdle 'StIdle m (Zipper Block) DownloadedHeaders)
app serverChain clientChain = serve (server serverChain) (client clientChain)


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

-- So we can decode some transition. Now where does 'unexpected' come up?
-- We know the type 'from' that we're expecting. We have some arbitrary
-- transition. We want to ...
-- well no this is hopeless anyway, because we really cannot deal with the
-- placeholder transition TrContinue. If we want to continue in this way,
-- we'll have to update the Client/Server thing to not be so tightly
-- synchronous.
--
-- The question is: do we want every transition to necessarily correspond to
-- a serialised thing when running this over-the-wire?
-- If yes, then the problem is how to express yield-of-control in the
-- transitions? Deadlock (both sides awaiting) and collision (both sides
-- yielding) is possible with the current GADT transition system, because the
-- terminus of a transition can be unconstrained!
-- Somehow we need proof that every transition cosntructor picks out one and
-- only one of 'Yield' or 'Continue'.
-- Introducing a type family that picks 'Yield' or 'Continue' for each state
-- could help.
--
-- The type family seems like a good idea in fact.
-- The state transition graph given by the data kind and GADT needn't and
-- shouldn't make any stipulation about 2 distinct _sides_ of the protocol.
-- There need not be 2 sides; the protocol be run entirely locally, as in
--
--   data P tr from to m t where
--     PDone :: t -> P tr to to m t
--     PNext :: tr from inter -> m (P tr inter to m t) -> P tr from to m t
--
--   type Go tr from to m t = m (P from to m t)
--
-- That one produces a well-formed series of transitions in 'm'. On the
-- other hand, we could define something that folds over well-formed
-- transitions
--
--   data Q tr from to m t where
--     QDone
--       :: t
--       -> Q tr to to m t
--     QNext
--       :: (tr from inter -> m (Q tr inter to m t))
--       -> Q tr from to m t
--
--
-- We can define encoding/decoding as one thing, producing a 'SomeTransition'.
-- Then, when using a 'SomeTransition' channel, we can use 'Typeable' on the
-- state kind to figure out when we have an unexpected message: try to build a
-- 'Refl' on the terminus of the previous transition and the initial end of
-- the incoming 'SomeTransition'.
--

data Status where
  Awaiting :: Status
  Yielding :: Status

type family Other (st :: Status) :: Status where
  Other Awaiting = Yielding
  Other Yielding = Awaiting

{-
--type family  (from :: k) (to :: k) :: Control

type family Change (a :: k) (b :: k) :: Control where
  Change a a = Hold
  Change a b = Release

-- | Give an owner to each state in a kind 'l'.
-- Rather than giving an explicit type like A | B, the family just picks one
-- of two arguments, like a Church boolean.
type family Owner (a :: k) (b :: k) (st :: l) :: k
-}

{-
type family Control (p :: r) (from :: l) (to :: l) (a :: k) (b :: k) :: k

data Peer p status tr from to m t where
  PeerDone
    :: t
    -> Peer p status tr end end m t
  PeerEffect
    :: m (Peer p status tr from to m t)
    -> Peer p status tr from to m t
  PeerYielding
    :: tr from inter
    -> Peer p (Control p from inter 'Yielding 'Awaiting) tr inter to m t
    -> Peer p 'Yielding tr from to m t
  PeerAwaiting
    :: (forall inter . tr from inter -> Peer p (Control p from inter 'Awaiting 'Yielding) tr inter to m t)
    -> Peer p 'Awaiting tr from to m t
-}

data Control where
  Hold    :: Control
  Release :: Control

type family TransitionControl (p :: r) (from :: l) (to :: l) :: Control

type family Next (c :: Control) (a :: k) (b :: k) :: k where
  Next 'Hold    a b = a
  Next 'Release a b = b

data Peer p status tr from to m t where
  PeerDone
    :: t
    -> Peer p status tr end end m t
  PeerEffect
    :: m (Peer p status tr from to m t)
    -> Peer p status tr from to m t
  PeerYielding
    :: tr from inter
    -> Peer p (Next (TransitionControl p from inter) 'Yielding 'Awaiting) tr inter to m t
    -> Peer p 'Yielding tr from to m t
  PeerAwaiting
    :: (forall inter . tr from inter -> Peer p (Next (TransitionControl p from inter) 'Awaiting 'Yielding) tr inter to m t)
    -> Peer p 'Awaiting tr from to m t


