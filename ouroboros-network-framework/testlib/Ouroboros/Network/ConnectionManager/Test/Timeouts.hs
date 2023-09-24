{-# LANGUAGE LambdaCase     #-}
{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.ConnectionManager.Test.Timeouts where

import           Control.Monad.Class.MonadTime.SI (DiffTime, Time, diffTime)
import           Control.Monad.IOSim

import           Data.Bifoldable (bifoldMap)
import           Data.Bitraversable (bimapAccumL)
import           Data.List (dropWhileEnd, find, intercalate)
import qualified Data.List.Trace as Trace
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import           Data.Monoid (Sum (Sum))

import           Text.Printf (printf)

import           Test.QuickCheck (Arbitrary (..), Property, choose,
                     counterexample, cover, frequency, label, property, shrink,
                     tabulate, (.&&.))

import           Network.TypedProtocol.Core (PeerHasAgency (..))

import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.Driver.Limits (ProtocolTimeLimits (..))
import           Ouroboros.Network.Protocol.Handshake.Codec
                     (timeLimitsHandshake)
import           Ouroboros.Network.Protocol.Handshake.Type
import qualified Ouroboros.Network.Snocket as Snocket


verifyAllTimeouts :: Show addr
                  => Bool
                  -> Trace (SimResult ()) [(Time, AbstractTransitionTrace addr)]
                  -> AllProperty
verifyAllTimeouts inDiffusion =
  bifoldMap
   ( \ case
       MainReturn {} -> mempty
       v             -> AllProperty
                     $ counterexample (show v) (property False)
   )
   (\ tr ->
     AllProperty
     $ counterexample ("\nConnection transition trace:\n"
                     ++ intercalate "\n" (map show tr)
                     )
     $ verifyTimeouts Nothing inDiffusion tr)

-- verifyTimeouts checks that in all \tau transition states the timeout is
-- respected. It does so by checking the stream of abstract transitions
-- paired with the time they happened, for a given connection; and checking
-- that transitions from \tau states to any other happens within the correct
-- timeout bounds. One note is that for the example
-- InboundIdleState^\tau -> OutboundState^\tau -> OutboundState sequence
-- The first transition would be fine, but for the second we need the time
-- when we transitioned into InboundIdleState and not OutboundState.
--
verifyTimeouts :: Maybe (AbstractState, Time)
               -- ^ Map of first occurrence of a given \tau state
               -> Bool
               -- ^ If runnning in Diffusion or not
               -> [(Time , AbstractTransitionTrace addr)]
               -- ^ Stream of abstract transitions for a given connection
               -- paired with the time it occurred
               -> Property
verifyTimeouts state inDiffusion [] =
  counterexample
    ("This state didn't timeout:\n"
    ++ show state
    )
  $ (inDiffusion || isNothing state)
-- If we already seen a \tau transition state
verifyTimeouts st@(Just (state, t')) inDiffusion
               ((t, TransitionTrace _ tt@(Transition _ to)):xs) =
    let newState  = Just (to, t)
        idleTimeout  =
            1.1 * tProtocolIdleTimeout simTimeouts
        outboundTimeout =
            1.1 * tOutboundIdleTimeout simTimeouts
        timeWaitTimeout =
            1.1 * tTimeWaitTimeout simTimeouts
        handshakeTimeout = case timeLimitsHandshake of
          (ProtocolTimeLimits stLimit) ->
            -- Should be the same but we bias to the shorter one
            let time = min (fromMaybe 0 (stLimit (ClientAgency TokPropose)))
                           (fromMaybe 0 (stLimit (ServerAgency TokConfirm)))
             in time + (0.1 * time)

     in case state of
       UnnegotiatedSt _ -> case to of
         -- Timeout terminating states
         OutboundUniSt ->
           counterexample (errorMsg tt t' t handshakeTimeout)
           $ diffTime t t' <= handshakeTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         InboundIdleSt Unidirectional ->
           counterexample (errorMsg tt t' t handshakeTimeout)
           $ diffTime t t' <= handshakeTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         TerminatedSt ->
           counterexample (errorMsg tt t' t handshakeTimeout)
           $ diffTime t t' <= handshakeTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs

         -- These states terminate the current timeout
         -- and starts a new one
         OutboundDupSt Ticking ->
           counterexample (errorMsg tt t' t handshakeTimeout)
           $ diffTime t t' <= handshakeTimeout
           .&&. verifyTimeouts newState inDiffusion xs
         InboundIdleSt Duplex ->
           counterexample (errorMsg tt t' t handshakeTimeout)
           $ diffTime t t' <= handshakeTimeout
           .&&. verifyTimeouts newState inDiffusion xs

         _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

       InboundIdleSt Duplex         -> case to of
         -- Should preserve the timeout
         OutboundDupSt Ticking -> verifyTimeouts st inDiffusion xs
         InboundIdleSt Duplex -> verifyTimeouts st inDiffusion xs

         -- Timeout terminating states
         OutboundDupSt Expired ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         InboundSt Duplex ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         DuplexSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         TerminatedSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs

         -- This state terminates the current timeout
         -- and starts a new one
         TerminatingSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts newState inDiffusion xs

         _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

       InboundIdleSt Unidirectional -> case to of
         -- Timeout terminating states
         InboundSt Unidirectional ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         TerminatedSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs

         -- This state terminates the current timeout
         -- and starts a new one
         TerminatingSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= idleTimeout
           .&&. verifyTimeouts newState inDiffusion xs

         _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

       OutboundDupSt Ticking        -> case to of
         -- Should preserve the timeout
         InboundIdleSt Duplex -> verifyTimeouts st inDiffusion xs
         OutboundDupSt Ticking -> verifyTimeouts st inDiffusion xs

         -- Timeout terminating states
         OutboundDupSt Expired ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         DuplexSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         InboundSt Duplex ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         TerminatedSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs

         -- This state terminates the current timeout
         -- and starts a new one
         TerminatingSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts newState inDiffusion xs

         _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

       OutboundIdleSt _             -> case to of
         -- Timeout terminating states
         InboundSt Duplex ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs
         TerminatedSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs

         -- This state terminates the current timeout
         -- and starts a new one
         TerminatingSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= outboundTimeout
           .&&. verifyTimeouts newState inDiffusion xs

         _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

       TerminatingSt                -> case to of
         -- Timeout terminating states
         UnnegotiatedSt Inbound ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= timeWaitTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs

         TerminatedSt ->
           counterexample (errorMsg tt t' t idleTimeout)
           $ diffTime t t' <= timeWaitTimeout
           .&&. verifyTimeouts Nothing inDiffusion xs

         _ -> error ("Unexpected invalid transition: " ++ show (st, tt))

       _ -> error ("Should be a \tau state: " ++ show st)
  where
    errorMsg trans time' time maxDiffTime =
      "\nAt transition: " ++ show trans ++ "\n"
      ++ "First happened at: " ++ show time' ++ "\n"
      ++ "Second happened at: " ++ show time ++ "\n"
      ++ "Should only take: "
      ++ show maxDiffTime
      ++ ", but took:" ++ show (diffTime time time')
-- If we haven't seen a \tau transition state
verifyTimeouts Nothing inDiffusion ((t, TransitionTrace _ (Transition _ to)):xs) =
    let newState = Just (to, t)
     in case to of
       InboundIdleSt _       -> verifyTimeouts newState inDiffusion xs
       OutboundDupSt Ticking -> verifyTimeouts newState inDiffusion xs
       OutboundIdleSt _      -> verifyTimeouts newState inDiffusion xs
       TerminatingSt         -> verifyTimeouts newState inDiffusion xs
       _                     -> verifyTimeouts Nothing  inDiffusion xs

-- | Configurable timeouts.  We use different timeouts for 'IO' and 'IOSim' property tests.
--
data Timeouts = Timeouts {
    tProtocolIdleTimeout :: DiffTime,
    tOutboundIdleTimeout :: DiffTime,
    tTimeWaitTimeout     :: DiffTime
  }

-- | Timeouts for 'IO' tests.
--
ioTimeouts :: Timeouts
ioTimeouts = Timeouts {
    tProtocolIdleTimeout = 0.1,
    tOutboundIdleTimeout = 0.1,
    tTimeWaitTimeout     = 0.1
  }

-- | Timeouts for 'IOSim' tests.
--
simTimeouts :: Timeouts
simTimeouts = Timeouts {
    tProtocolIdleTimeout = 5,
    tOutboundIdleTimeout = 5,
    tTimeWaitTimeout     = 30
  }

-- | Groups 'TransitionTrace' to the same peerAddr.
--
groupConns :: Ord addr
           => (a -> TransitionTrace' addr st)
           -> (Transition' st -> Bool)
           -> Trace r a
           -> Trace r [a]
groupConns getTransition isFinalTransition =
    fmap fromJust
  . Trace.filter isJust
  -- there might be some connections in the state, push them onto the 'Trace'
  . (\(s, o) -> foldr (\a as -> Trace.Cons (Just (reverse a)) as) o (Map.elems s))
  . bimapAccumL
      ( \ s a -> (s, a))
      ( \ s a ->
          let TransitionTrace { ttPeerAddr, ttTransition } = getTransition a
           in if isFinalTransition ttTransition
                 then case ttPeerAddr `Map.lookup` s of
                        Nothing  -> ( Map.insert ttPeerAddr [a] s
                                    , Nothing
                                    )
                        Just trs -> ( Map.delete ttPeerAddr s
                                    , Just (reverse $ a : trs)
                                    )
                 else ( Map.alter (\case
                                     Nothing -> Just [a]
                                     Just as -> Just (a : as)
                                  )
                                  ttPeerAddr s
                      , Nothing)
      )
      Map.empty

-- | The concrete address type used by simulations.
--
type SimAddr  = Snocket.TestAddress SimAddr_
type SimAddr_ = Int

-- | We use a wrapper for test addresses since the Arbitrary instance for Snocket.TestAddress only
--   generates addresses between 1 and 4.
newtype TestAddr = TestAddr { unTestAddr :: SimAddr }
  deriving (Show, Eq, Ord)

instance Arbitrary TestAddr where
  arbitrary = TestAddr . Snocket.TestAddress <$> choose (1, 100)


-- | Test property together with classification.
--
data TestProperty = TestProperty {
    tpProperty            :: !Property,
    -- ^ 'True' if property is true

    tpNumberOfTransitions :: !(Sum Int),
    -- ^ number of all transitions

    tpNumberOfConnections :: !(Sum Int),
    -- ^ number of all connections

    tpNumberOfPrunings    :: !(Sum Int),
    -- ^ number of all connections

    --
    -- classification of connections
    --
    tpNegotiatedDataFlows :: ![NegotiatedDataFlow],
    tpEffectiveDataFlows  :: ![EffectiveDataFlow],
    tpTerminationTypes    :: ![Maybe TerminationType],
    tpActivityTypes       :: ![ActivityType],

    tpTransitions         :: ![AbstractTransition]

  }

instance Show TestProperty where
    show tp =
      concat [ "TestProperty "
             , "{ tpNumberOfTransitions = " ++ show (tpNumberOfTransitions tp)
             , ", tpNumberOfConnections = " ++ show (tpNumberOfConnections tp)
             , ", tpNumberOfPrunings = "    ++ show (tpNumberOfPrunings tp)
             , ", tpNegotiatedDataFlows = " ++ show (tpNegotiatedDataFlows tp)
             , ", tpTerminationTypes = "    ++ show (tpTerminationTypes tp)
             , ", tpActivityTypes = "       ++ show (tpActivityTypes tp)
             , ", tpTransitions = "         ++ show (tpTransitions tp)
             , "}"
             ]

instance Semigroup TestProperty where
  (<>) (TestProperty a0 a1 a2 a3 a4 a5 a6 a7 a8)
       (TestProperty b0 b1 b2 b3 b4 b5 b6 b7 b8) =
      TestProperty (a0 .&&. b0)
                   (a1 <> b1)
                   (a2 <> b2)
                   (a3 <> b3)
                   (a4 <> b4)
                   (a5 <> b5)
                   (a6 <> b6)
                   (a7 <> b7)
                   (a8 <> b8)

instance Monoid TestProperty where
    mempty = TestProperty (property True)
                          mempty mempty mempty mempty
                          mempty mempty mempty mempty

mkProperty :: TestProperty -> Property
mkProperty TestProperty { tpProperty
                        , tpNumberOfTransitions = Sum numberOfTransitions_
                        , tpNumberOfConnections = Sum numberOfConnections_
                        , tpNumberOfPrunings = Sum numberOfPrunings_
                        , tpNegotiatedDataFlows
                        , tpEffectiveDataFlows
                        , tpTerminationTypes
                        , tpActivityTypes
                        , tpTransitions
                        } =
     label ("Number of transitions: " ++ within_ 10 numberOfTransitions_
           )
   . label ("Number of connections: " ++ show numberOfConnections_
           )
   . tabulate "Pruning"             [show numberOfPrunings_]
   . tabulate "Negotiated DataFlow" (map show tpNegotiatedDataFlows)
   . tabulate "Effective DataFLow"  (map show tpEffectiveDataFlows)
   . tabulate "Termination"         (map show tpTerminationTypes)
   . tabulate "Activity Type"       (map show tpActivityTypes)
   . tabulate "Transitions"         (map ppTransition tpTransitions)
   $ tpProperty

mkPropertyPruning :: TestProperty -> Property
mkPropertyPruning tp@TestProperty { tpNumberOfPrunings = Sum numberOfPrunings_ } =
     cover 35 (numberOfPrunings_ > 0) "Prunings"
   . mkProperty
   $ tp

-- classify negotiated data flow
classifyNegotiatedDataFlow :: [AbstractTransition] -> NegotiatedDataFlow
classifyNegotiatedDataFlow as =
  case find ( \ tr
             -> case toState tr of
                  OutboundUniSt    -> True
                  OutboundDupSt {} -> True
                  InboundIdleSt {} -> True
                  _                -> False
            ) as of
     Nothing -> NotNegotiated
     Just tr ->
       case toState tr of
         OutboundUniSt      -> NegotiatedDataFlow Unidirectional
         OutboundDupSt {}   -> NegotiatedDataFlow Duplex
         (InboundIdleSt df) -> NegotiatedDataFlow df
         _                  -> error "impossible happened!"

-- classify effective data flow
classifyEffectiveDataFlow :: [AbstractTransition] -> EffectiveDataFlow
classifyEffectiveDataFlow as =
  case find ((== DuplexSt) . toState) as of
    Nothing -> EffectiveDataFlow Unidirectional
    Just _  -> EffectiveDataFlow Duplex

-- classify termination
classifyTermination :: [AbstractTransition] -> Maybe TerminationType
classifyTermination as =
    case maybeLast $ dropWhileEnd
                  (== Transition TerminatedSt TerminatedSt)
              $ dropWhileEnd
                  (== Transition TerminatedSt UnknownConnectionSt) as of
      Just Transition { fromState = TerminatingSt
                      , toState   = TerminatedSt
                      } -> Just CleanTermination
      Just _            -> Just ErroredTermination
      Nothing           -> Nothing
  where
    maybeLast :: [a] -> Maybe a
    maybeLast []         = Nothing
    maybeLast xs@(_ : _) = Just (last xs)

-- classify if a connection is active or not
classifyActivityType :: [AbstractTransition] -> ActivityType
classifyActivityType as =
  case find ( \ tr
             -> case toState tr of
                  InboundSt     {} -> True
                  OutboundUniSt    -> True
                  OutboundDupSt {} -> True
                  DuplexSt      {} -> True
                  _                -> False
            ) as of
    Nothing -> IdleConn
    Just {} -> ActiveConn

-- classify negotiated data flow
classifyPrunings :: [ConnectionManagerTrace
                      addr
                      (ConnectionHandlerTrace
                        prctl
                        dataflow)]
                 -> Sum Int
classifyPrunings =
  Sum
  . length
  . filter ( \x -> case x of
                TrPruneConnections _ _ _ -> True
                _                        -> False
           )


newtype AllProperty = AllProperty { getAllProperty :: Property }

instance Semigroup AllProperty where
    AllProperty a <> AllProperty b = AllProperty (a .&&. b)

instance Monoid AllProperty where
    mempty = AllProperty (property True)

newtype ArbDataFlow = ArbDataFlow DataFlow
  deriving Show

instance Arbitrary ArbDataFlow where
    arbitrary = ArbDataFlow <$> frequency [ (3, pure Duplex)
                                          , (1, pure Unidirectional)
                                          ]
    shrink (ArbDataFlow Duplex)         = [ArbDataFlow Unidirectional]
    shrink (ArbDataFlow Unidirectional) = []

data ActivityType
    = IdleConn

    -- | Active connections are once that reach any of the state:
    --
    -- - 'InboundSt'
    -- - 'OutobundUniSt'
    -- - 'OutboundDupSt'
    -- - 'DuplexSt'
    --
    | ActiveConn
    deriving (Eq, Show)

data TerminationType
    = ErroredTermination
    | CleanTermination
    deriving (Eq, Show)

data NegotiatedDataFlow
    = NotNegotiated

    -- | Negotiated value of 'DataFlow'
    | NegotiatedDataFlow DataFlow
    deriving (Eq, Show)

data EffectiveDataFlow
    -- | Unlike the negotiated 'DataFlow' this indicates if the connection has
    -- ever been in 'DuplexSt'
    --
    = EffectiveDataFlow DataFlow
    deriving (Eq, Show)

within_ :: Int -> Int -> String
within_ _ 0 = "0"
within_ a b = let x = b `div` a in
              concat [ if b < a
                         then "1"
                         else show $ x * a
                     , " - "
                     , show $ x * a + a - 1
                     ]

ppTransition :: AbstractTransition -> String
ppTransition Transition {fromState, toState} =
    printf "%-30s → %s" (show fromState) (show toState)


