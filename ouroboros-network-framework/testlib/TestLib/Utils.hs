{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}

module TestLib.Utils where

import           Control.Monad.IOSim

import           Data.Bitraversable (bimapAccumL)
import           Data.List (find, dropWhileEnd)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust, isJust)
import           Data.Monoid (Sum (Sum))
import qualified Data.List.Trace as Trace

import           Text.Printf (printf)

import           Test.QuickCheck
                     ((.&&.), Property, property, Arbitrary(..), shrink,
                     frequency, cover, label, tabulate, choose)

import           Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import           Ouroboros.Network.ConnectionManager.Types
import qualified Ouroboros.Network.Snocket as Snocket


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
    tpTerminationTypes    :: ![TerminationType],
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
classifyTermination :: [AbstractTransition] -> TerminationType
classifyTermination as =
  case last $ dropWhileEnd
                (== Transition TerminatedSt TerminatedSt)
            $ dropWhileEnd
                (== Transition TerminatedSt UnknownConnectionSt) as of
    Transition { fromState = TerminatingSt
               , toState   = TerminatedSt
               } -> CleanTermination
    _            -> ErroredTermination

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


