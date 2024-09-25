{-# LANGUAGE NamedFieldPuns #-}

module Ouroboros.Network.ConnectionManager.Test.Utils where

import Prelude hiding (read)

import Ouroboros.Network.ConnectionHandler (ConnectionHandlerTrace)
import Ouroboros.Network.ConnectionManager.Types

import Test.QuickCheck (counterexample, property)
import Test.QuickCheck.Monoids (All (..))


verifyAbstractTransition :: AbstractTransition
                         -> Bool
verifyAbstractTransition Transition { fromState, toState } =
    case (fromState, toState) of
      --
      -- Outbound
      --

      -- @Reserve@
      (TerminatedSt, ReservedOutboundSt) -> True
      (UnknownConnectionSt, ReservedOutboundSt) -> True
      -- @Connected@
      (ReservedOutboundSt, UnnegotiatedSt Outbound) -> True
      -- @Negotiated^{Unidirectional}_{Outbound}@
      (UnnegotiatedSt Outbound, OutboundUniSt)  -> True
      -- @Negotiated^{Duplex}_{Outbound}@
      (UnnegotiatedSt Outbound, OutboundDupSt Ticking) -> True
      (UnnegotiatedSt _,        TerminatingSt) -> True

      -- @DemotedToCold^{Unidirectional}_{Local}@
      (OutboundUniSt, OutboundIdleSt Unidirectional) -> True
      -- @TimeoutExpired@
      (OutboundDupSt Ticking, OutboundDupSt Expired) -> True
      -- @DemotedToCold^{Duplex}_{Local}@
      (OutboundDupSt Expired, OutboundIdleSt Duplex) -> True
      -- identity transition executed by 'demotedToColdRemote'
      (OutboundIdleSt dataFlow, OutboundIdleSt dataFlow') -> dataFlow == dataFlow'

      --
      -- Outbound ↔ Inbound
      --

      -- @DemotedToCold^{Duplex}_{Local}@
      (OutboundDupSt Ticking, InboundIdleSt Duplex) -> True
      -- @Awake^{Duplex}_{Local}
      -- or
      -- 'SelfConn⁻¹'
      (InboundIdleSt Duplex, OutboundDupSt Ticking) -> True
      -- @PromotedToWarm^{Duplex}_{Remote}@
      (OutboundDupSt Ticking, DuplexSt) -> True
      (OutboundDupSt Expired, DuplexSt) -> True
      -- can be executed by 'demotedToColdRemote'
      (OutboundDupSt expired, OutboundDupSt expired')
                                        -> expired == expired'
      -- @PromotedToWarm^{Duplex}_{Local}@
      (InboundSt Duplex, DuplexSt) -> True
      -- @DemotedToCold^{Duplex}_{Remote}@
      (DuplexSt, OutboundDupSt Ticking) -> True
      -- @DemotedToCold^{Duplex}_{Local}@
      (DuplexSt, InboundSt Duplex) -> True

      --
      -- Inbound
      --

      -- @Accepted@
      (TerminatedSt, UnnegotiatedSt Inbound) -> True
      (UnknownConnectionSt, UnnegotiatedSt Inbound) -> True
      -- @Overwritten@
      (ReservedOutboundSt, UnnegotiatedSt Inbound) -> True
      -- @Negotiated^{Duplex}_{Inbound}
      (UnnegotiatedSt Inbound, InboundIdleSt Duplex) -> True
      -- @Negotiated^{Unidirectional}_{Inbound}
      (UnnegotiatedSt Inbound, InboundIdleSt Unidirectional) -> True

      -- @SelfConnNegotiated^{Duplex}_{Inbound}
      (UnnegotiatedSt Outbound, InboundIdleSt Duplex) -> True
      -- @SelfConnNegotiated^{Unidirectional}_{Inbound}
      (UnnegotiatedSt Outbound, InboundIdleSt Unidirectional) -> True

      -- 'unregisterOutboundConnection' and 'demotedToColdRemote' might perform
      (InboundIdleSt Duplex, InboundIdleSt Duplex) -> True
      -- @Awake^{Duplex}_{Remote}
      (InboundIdleSt Duplex, InboundSt Duplex) -> True
      -- @Commit^{Duplex}
      (InboundIdleSt Duplex, TerminatingSt) -> True
      -- @DemotedToCold^{Duplex}_{Local}
      (InboundSt Duplex, InboundIdleSt Duplex) -> True

      -- @Awake^{Unidirectional}_{Remote}
      (InboundIdleSt Unidirectional, InboundSt Unidirectional) -> True
      -- @Commit^{Unidirectional}
      (InboundIdleSt Unidirectional, TerminatingSt) -> True
      -- @DemotedToCold^{Unidirectional}_{Local}
      (InboundSt Unidirectional, InboundIdleSt Unidirectional) -> True

      --
      -- OutboundIdleSt
      --

      (OutboundIdleSt Duplex, InboundSt Duplex) -> True
      (OutboundIdleSt _dataFlow, TerminatingSt) -> True

      --
      -- Connecting to oneself
      --

      -- 'SelfConn'
      (UnnegotiatedSt Outbound, UnnegotiatedSt Inbound) -> True
      -- 'SelfConn⁻¹'
      (UnnegotiatedSt Inbound,  UnnegotiatedSt Outbound) -> True
      -- 'SelfConn⁻¹'
      (InboundIdleSt Unidirectional, OutboundUniSt) -> True

      --
      -- Terminate
      --

      -- @Terminate@
      (TerminatingSt, TerminatedSt) -> True

      -- explicit prohibition of reflexive terminate transition
      (TerminatedSt, TerminatedSt) -> False
      -- implicit terminate transition
      (_, TerminatedSt) -> True

      -- explicit prohibition of reflexive unknown transition
      (UnknownConnectionSt, UnknownConnectionSt) -> False
      (_, UnknownConnectionSt) -> True

      -- We accept connection in 'TerminatingSt'
      (TerminatingSt, UnnegotiatedSt Inbound) -> True

      _ -> False

-- | Maps each valid transition into one number. Collapses all invalid transition into a
-- single number.
--
-- NOTE: Should be in sync with 'verifyAbstractTransition'
--
validTransitionMap :: AbstractTransition
                   -> (Int, String)
validTransitionMap t@Transition { fromState, toState } =
    case (fromState, toState) of
      (TerminatedSt            , ReservedOutboundSt)                -> (01, show t)
      (UnknownConnectionSt     , ReservedOutboundSt)                -> (02, show t)
      (ReservedOutboundSt      , UnnegotiatedSt Outbound)           -> (03, show t)
      (UnnegotiatedSt Outbound , OutboundUniSt)                     -> (04, show t)
      (UnnegotiatedSt Outbound , OutboundDupSt Ticking)             -> (05, show t)
      (OutboundUniSt           , OutboundIdleSt Unidirectional)     -> (06, show t)
      (OutboundDupSt Ticking   , OutboundDupSt Expired)             -> (07, show t)
      (OutboundDupSt Expired   , OutboundIdleSt Duplex)             -> (08, show t)
      (OutboundIdleSt dataFlow , OutboundIdleSt dataFlow')
        | dataFlow == dataFlow'                                     -> (09, show t)
      (OutboundDupSt Ticking   , InboundIdleSt Duplex)              -> (10, show t)
      (InboundIdleSt Duplex    , OutboundDupSt Ticking)             -> (11, show t)
      (OutboundDupSt Ticking   , DuplexSt)                          -> (12, show t)
      (OutboundDupSt Expired   , DuplexSt)                          -> (13, show t)
      (OutboundDupSt expired   , OutboundDupSt expired')
        | expired == expired'                                       -> (14, show t)
      (InboundSt Duplex             , DuplexSt)                     -> (15, show t)
      (DuplexSt                     , OutboundDupSt Ticking)        -> (16, show t)
      (DuplexSt                     , InboundSt Duplex)             -> (17, show t)
      (TerminatedSt                 , UnnegotiatedSt Inbound)       -> (18, show t)
      (UnknownConnectionSt          , UnnegotiatedSt Inbound)       -> (19, show t)
      (ReservedOutboundSt           , UnnegotiatedSt Inbound)       -> (20, show t)
      (UnnegotiatedSt Inbound       , InboundIdleSt Duplex)         -> (21, show t)
      (UnnegotiatedSt Inbound       , InboundIdleSt Unidirectional) -> (22, show t)
      (InboundIdleSt Duplex         , InboundIdleSt Duplex)         -> (23, show t)
      (InboundIdleSt Duplex         , InboundSt Duplex)             -> (24, show t)
      (InboundIdleSt Duplex         , TerminatingSt)                -> (25, show t)
      (InboundSt Duplex             , InboundIdleSt Duplex)         -> (26, show t)
      (InboundIdleSt Unidirectional , InboundSt Unidirectional)     -> (27, show t)
      (InboundIdleSt Unidirectional , TerminatingSt)                -> (28, show t)
      (InboundSt Unidirectional     , InboundIdleSt Unidirectional) -> (29, show t)
      (OutboundIdleSt Duplex        , InboundSt Duplex)             -> (30, show t)
      (UnnegotiatedSt Outbound      , UnnegotiatedSt Inbound)       -> (36, show t)
      (UnnegotiatedSt Inbound       , UnnegotiatedSt Outbound)      -> (37, show t) -- max valid transition
      (OutboundIdleSt _dataFlow , TerminatingSt)                    -> (38, show t)
      (TerminatingSt            , TerminatedSt)                     -> (39, show t)
      (_                        , TerminatedSt)                     -> (40, show t)
      (_                        , UnknownConnectionSt)              -> (41, show t)
      (TerminatingSt            , UnnegotiatedSt Inbound)           -> (42, show t)
      (UnnegotiatedSt Outbound      , InboundIdleSt Duplex)         -> (43, show t)
      (UnnegotiatedSt Outbound      , InboundIdleSt Unidirectional) -> (44, show t)
      (InboundIdleSt Unidirectional , OutboundUniSt)                -> (45, show t)
      _                                                             -> (99, show t)

-- Assuming all transitions in the transition list are valid, we only need to
-- look at the 'toState' of the current transition and the 'fromState' of the
-- next transition.
verifyAbstractTransitionOrder :: Bool -- ^ Check last transition: useful for
                                      --    distinguish Diffusion layer tests
                                      --    vs non-Diffusion ones.
                              -> [AbstractTransition]
                              -> All
verifyAbstractTransitionOrder _ [] = mempty
verifyAbstractTransitionOrder checkLast (h:t) = go t h
  where
    go :: [AbstractTransition] -> AbstractTransition -> All
    -- All transitions must end in the 'UnknownConnectionSt', and since we
    -- assume that all transitions are valid we do not have to check the
    -- 'fromState'.
    go [] (Transition _ UnknownConnectionSt) = mempty
    go [] tr@(Transition _ _)          =
      All
        $ counterexample
            ("\nUnexpected last transition: " ++ show tr)
            (property (not checkLast))
    -- All transitions have to be in a correct order, which means that the
    -- current state we are looking at (current toState) needs to be equal to
    -- the next 'fromState', in order for the transition chain to be correct.
    go (next@(Transition nextFromState _) : ts)
        curr@(Transition _ currToState) =
         All
           (counterexample
              ("\nUnexpected transition order!\nWent from: "
              ++ show curr ++ "\nto: " ++ show next)
              (property (currToState == nextFromState)))
         <> go ts next


-- | List of all valid transition's names.
--
-- NOTE: Should be in sync with 'verifyAbstractTransition', but due to #3516
-- abrupt terminating transitions and identity transitions are trimmed for now,
-- until we tweak the generators to include more connection errors.
--
allValidTransitionsNames :: [String]
allValidTransitionsNames =
  map show
  [ Transition UnknownConnectionSt             ReservedOutboundSt
  -- , Transition TerminatedSt                    ReservedOutboundSt
  , Transition ReservedOutboundSt              (UnnegotiatedSt Outbound)
  , Transition (UnnegotiatedSt Outbound)       (UnnegotiatedSt Inbound)
  , Transition (UnnegotiatedSt Inbound)        (UnnegotiatedSt Outbound)
  , Transition (UnnegotiatedSt Outbound)       OutboundUniSt
  , Transition (UnnegotiatedSt Outbound)       (OutboundDupSt Ticking)
  , Transition OutboundUniSt                   (OutboundIdleSt Unidirectional)
  , Transition (OutboundDupSt Ticking)         (OutboundDupSt Expired)
  -- , Transition (OutboundDupSt Expired)         (OutboundIdleSt Duplex)
  -- , Transition (OutboundIdleSt Unidirectional) (OutboundIdleSt Unidirectional)
  -- , Transition (OutboundIdleSt Duplex)         (OutboundIdleSt Duplex)
  , Transition (OutboundDupSt Ticking)         (InboundIdleSt Duplex)
  , Transition (InboundIdleSt Duplex)          (OutboundDupSt Ticking)
  , Transition (OutboundDupSt Ticking)         DuplexSt
  -- , Transition (OutboundDupSt Expired)         DuplexSt
  -- , Transition (OutboundDupSt Ticking)         (OutboundDupSt Ticking)
  -- , Transition (OutboundDupSt Expired)         (OutboundDupSt Expired)
  , Transition (InboundSt Duplex)              DuplexSt
  , Transition DuplexSt                        (OutboundDupSt Ticking)
  , Transition DuplexSt                        (InboundSt Duplex)
  -- , Transition TerminatedSt                    (UnnegotiatedSt Inbound)
  , Transition UnknownConnectionSt             (UnnegotiatedSt Inbound)
  , Transition ReservedOutboundSt              (UnnegotiatedSt Inbound)
  , Transition (UnnegotiatedSt Inbound)        (InboundIdleSt Duplex)
  , Transition (UnnegotiatedSt Inbound)        (InboundIdleSt Unidirectional)
  -- , Transition (InboundIdleSt Duplex)          (InboundIdleSt Duplex)
  , Transition (InboundIdleSt Duplex)          (InboundSt Duplex)
  -- , Transition (InboundIdleSt Duplex)          TerminatingSt
  -- , Transition (InboundSt Duplex)              (InboundIdleSt Duplex)
  -- , Transition (InboundIdleSt Unidirectional)  (InboundSt Unidirectional)
  -- , Transition (InboundIdleSt Unidirectional)  TerminatingSt
  -- , Transition (InboundSt Unidirectional)      (InboundIdleSt Unidirectional)
  -- , Transition (OutboundIdleSt Duplex)         (InboundSt Duplex)
  , Transition (UnnegotiatedSt Outbound)       (UnnegotiatedSt Inbound)
  , Transition (UnnegotiatedSt Inbound)        (UnnegotiatedSt Outbound)
  -- , Transition (OutboundIdleSt Unidirectional) TerminatingSt
  -- , Transition (OutboundIdleSt Duplex)         TerminatingSt
  , Transition TerminatingSt                   TerminatedSt
  -- , Transition TerminatedSt                    UnknownConnectionSt
  -- , Transition TerminatingSt                   (UnnegotiatedSt Inbound)
  -- , Transition (_)                             (TerminatedSt)
  -- , Transition (_)                             (UnknownConnectionSt)
  , Transition (UnnegotiatedSt Outbound)       (InboundIdleSt Unidirectional)
  , Transition (UnnegotiatedSt Outbound)       (InboundIdleSt Duplex)
  , Transition (InboundIdleSt Unidirectional)  OutboundUniSt
  ]

abstractStateIsFinalTransition :: Transition' AbstractState -> Bool
abstractStateIsFinalTransition (Transition _ UnknownConnectionSt) = True
abstractStateIsFinalTransition _                                  = False

-- | This function is necessary to check tests that only read from the
-- `traceTVar` logging output. These `traceTVar` logs do not transition to
-- 'UnknownConnectionSt'.
--
abstractStateIsFinalTransitionTVarTracing :: Transition' AbstractState -> Bool
abstractStateIsFinalTransitionTVarTracing (Transition _ UnknownConnectionSt) = True
abstractStateIsFinalTransitionTVarTracing _                                  = False

connectionManagerTraceMap
  :: ConnectionManagerTrace
      ntnAddr
      (ConnectionHandlerTrace ntnVersion ntnVersionData)
  -> String
connectionManagerTraceMap (TrIncludeConnection p _)        =
  "TrIncludeConnection " ++ show p
connectionManagerTraceMap (TrUnregisterConnection p _)     =
  "TrUnregisterConnection " ++ show p
connectionManagerTraceMap (TrConnect _ _)                  =
  "TrConnect"
connectionManagerTraceMap (TrConnectError _ _ _)          =
  "TrConnectError"
connectionManagerTraceMap (TrTerminatingConnection p _)    =
  "TrTerminatingConnection " ++ show p
connectionManagerTraceMap (TrTerminatedConnection p _)     =
  "TrTerminatedConnection " ++ show p
connectionManagerTraceMap (TrConnectionHandler _ _)        =
  "TrConnectionHandler"
connectionManagerTraceMap TrShutdown                       =
  "TrShutdown"
connectionManagerTraceMap (TrConnectionExists p _ as)      =
  "TrConnectionExists " ++ show p ++ " " ++ show as
connectionManagerTraceMap (TrForbiddenConnection _)        =
  "TrForbiddenConnection"
connectionManagerTraceMap (TrConnectionFailure _)          =
  "TrConnectionFailure"
connectionManagerTraceMap (TrConnectionNotFound p _)       =
  "TrConnectionNotFound " ++ show p
connectionManagerTraceMap (TrForbiddenOperation _ as)      =
  "TrForbiddenOperation" ++ show as
connectionManagerTraceMap (TrPruneConnections _ _ _)       =
  "TrPruneConnections"
connectionManagerTraceMap (TrConnectionCleanup _)          =
  "TrConnectionCleanup"
connectionManagerTraceMap (TrConnectionTimeWait _)         =
  "TrConnectionTimeWait"
connectionManagerTraceMap (TrConnectionTimeWaitDone _)     =
  "TrConnectionTimeWaitDone"
connectionManagerTraceMap (TrConnectionManagerCounters _)  =
  "TrConnectionManagerCounters"
connectionManagerTraceMap (TrState _)                      =
  "TrState"
connectionManagerTraceMap (TrUnexpectedlyFalseAssertion _) =
  "TrUnexpectedlyFalseAssertion"
