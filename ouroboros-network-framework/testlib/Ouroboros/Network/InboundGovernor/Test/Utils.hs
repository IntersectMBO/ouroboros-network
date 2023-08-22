{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Ouroboros.Network.InboundGovernor.Test.Utils where

import           Test.QuickCheck

import           Ouroboros.Network.ConnectionManager.Test.Timeouts
import           Ouroboros.Network.ConnectionManager.Types
import           Ouroboros.Network.InboundGovernor (InboundGovernorTrace (..),
                     RemoteSt (..))
import qualified Ouroboros.Network.InboundGovernor as IG
import           Ouroboros.Network.Server2 (RemoteTransition, ServerTrace (..))


-- | Pattern synonym which matches either 'RemoteHotEst' or 'RemoteWarmSt'.
--
pattern RemoteEstSt :: RemoteSt
pattern RemoteEstSt <- (\ case
                            RemoteHotSt  -> True
                            RemoteWarmSt -> True
                            _            -> False -> True
                        )

{-# COMPLETE RemoteEstSt, RemoteIdleSt, RemoteColdSt #-}


-- | Specification of the transition table of the inbound governor.
--
verifyRemoteTransition :: RemoteTransition -> Bool
verifyRemoteTransition Transition {fromState, toState} =
    case (fromState, toState) of
      -- The initial state must be 'RemoteIdleSt'.
      (Nothing,           Just RemoteIdleSt) -> True

      --
      -- Promotions
      --

      (Just RemoteIdleSt, Just RemoteEstSt)  -> True
      (Just RemoteColdSt, Just RemoteEstSt)  -> True
      (Just RemoteWarmSt, Just RemoteHotSt)  -> True

      --
      -- Demotions
      --

      (Just RemoteHotSt,  Just RemoteWarmSt) -> True
      -- demotion to idle state can happen from any established state
      (Just RemoteEstSt,  Just RemoteIdleSt) -> True
      -- demotion to cold can only be done from idle state; We explicitly rule
      -- out demotions to cold from warm or hot states.
      (Just RemoteEstSt,  Just RemoteColdSt) -> False
      (Just RemoteIdleSt, Just RemoteColdSt) -> True
      -- normal termination (if outbound side is not using that connection)
      (Just RemoteIdleSt, Nothing)           -> True
      -- This transition corresponds to connection manager's:
      -- @
      --   Commit^{Duplex}_{Local} : OutboundIdleState Duplex
      --                           → TerminatingState
      -- @
      (Just RemoteColdSt, Nothing)           -> True
      -- any of the mini-protocols errored
      (Just RemoteEstSt, Nothing)            -> True

      --
      -- We are conservative to name all the identity transitions.
      --

      -- This might happen if starting any of the responders errored.
      (Nothing,           Nothing)           -> True
      -- @RemoteWarmSt → RemoteWarmSt@, @RemoteIdleSt → RemoteIdleSt@ and
      -- @RemoteColdSt → RemoteColdSt@ transition are observed if a hot or
      -- warm protocol terminates (which triggers @RemoteEstSt -> RemoteWarmSt@)
      (Just RemoteWarmSt, Just RemoteWarmSt) -> True
      (Just RemoteIdleSt, Just RemoteIdleSt) -> True
      (Just RemoteColdSt, Just RemoteColdSt) -> True

      (_,                 _)                 -> False



-- | Maps each valid remote transition into one number. Collapses all invalid
-- transition into a single number.
--
-- NOTE: Should be in sync with 'verifyRemoteTransition'
--
validRemoteTransitionMap :: RemoteTransition -> (Int, String)
validRemoteTransitionMap t@Transition { fromState, toState } =
    case (fromState, toState) of
      (Nothing          , Just RemoteIdleSt) -> (00, show t)
      (Just RemoteIdleSt, Just RemoteEstSt)  -> (01, show t)
      (Just RemoteColdSt, Just RemoteEstSt)  -> (02, show t)
      (Just RemoteWarmSt, Just RemoteHotSt)  -> (03, show t)
      (Just RemoteHotSt , Just RemoteWarmSt) -> (04, show t)
      (Just RemoteEstSt , Just RemoteIdleSt) -> (05, show t)
      (Just RemoteIdleSt, Just RemoteColdSt) -> (06, show t)
      (Just RemoteIdleSt, Nothing)           -> (07, show t)
      (Just RemoteColdSt, Nothing)           -> (08, show t)
      (Just RemoteEstSt , Nothing)           -> (09, show t)
      (Nothing          , Nothing)           -> (10, show t)
      (Just RemoteWarmSt, Just RemoteWarmSt) -> (11, show t)
      (Just RemoteIdleSt, Just RemoteIdleSt) -> (12, show t)
      (Just RemoteColdSt, Just RemoteColdSt) -> (13, show t)
      (_                , _)                 -> (99, show t)

-- | List of all valid transition's names.
--
-- NOTE: Should be in sync with 'verifyAbstractTransition'.
--
allValidRemoteTransitionsNames :: [String]
allValidRemoteTransitionsNames =
  map show
  [ Transition Nothing             (Just RemoteIdleSt)
  , Transition (Just RemoteIdleSt) (Just RemoteWarmSt)
  -- , Transition (Just RemoteIdleSt) (Just RemoteHotSt)
  -- , Transition (Just RemoteColdSt) (Just RemoteWarmSt)
  -- , Transition (Just RemoteColdSt) (Just RemoteHotSt)
  , Transition (Just RemoteWarmSt) (Just RemoteHotSt)
  , Transition (Just RemoteHotSt ) (Just RemoteWarmSt)
  , Transition (Just RemoteWarmSt) (Just RemoteIdleSt)
  -- , Transition (Just RemoteHotSt)  (Just RemoteIdleSt)
  , Transition (Just RemoteIdleSt) (Just RemoteColdSt)
  , Transition (Just RemoteIdleSt) Nothing
  , Transition (Just RemoteColdSt) Nothing
  , Transition (Just RemoteWarmSt) Nothing
  , Transition (Just RemoteHotSt)  Nothing
  , Transition Nothing             Nothing
  -- , Transition (Just RemoteWarmSt) (Just RemoteWarmSt)
  -- , Transition (Just RemoteIdleSt) (Just RemoteIdleSt)
  -- , Transition (Just RemoteColdSt) (Just RemoteColdSt)
  ]

-- Assuming all transitions in the transition list are valid, we only need to
-- look at the 'toState' of the current transition and the 'fromState' of the
-- next transition.
verifyRemoteTransitionOrder :: Bool -- ^ Check last transition: useful for
                                    --    distinguish Diffusion layer tests
                                    --    vs non-Diffusion ones.
                            -> [RemoteTransition]
                            -> AllProperty
verifyRemoteTransitionOrder _ [] = mempty
verifyRemoteTransitionOrder checkLast (h:t) = go t h
  where
    go :: [RemoteTransition] -> RemoteTransition -> AllProperty
    -- All transitions must end in the 'Nothing' (final) state, and since
    -- we assume all transitions are valid we do not have to check the
    -- 'fromState' .
    go [] (Transition _ Nothing) = mempty
    go [] tr@(Transition _ _)          =
      AllProperty
        $ counterexample
            ("\nUnexpected last transition: " ++ show tr)
            (property (not checkLast))
    -- All transitions have to be in a correct order, which means that the
    -- current state we are looking at (current toState) needs to be equal to
    -- the next 'fromState', in order for the transition chain to be correct.
    go (next@(Transition nextFromState _) : ts)
        curr@(Transition _ currToState) =
         AllProperty
           (counterexample
              ("\nUnexpected transition order!\nWent from: "
              ++ show curr ++ "\nto: " ++ show next)
              (property (currToState == nextFromState)))
         <> go ts next

remoteStrIsFinalTransition :: Transition' (Maybe RemoteSt) -> Bool
remoteStrIsFinalTransition (Transition _ Nothing) = True
remoteStrIsFinalTransition _                      = False

inboundGovernorTraceMap :: InboundGovernorTrace ntnAddr -> String
inboundGovernorTraceMap (TrNewConnection p _)            =
  "TrNewConnection " ++ show p
inboundGovernorTraceMap (TrResponderRestarted _ mpn)         =
  "TrResponderRestarted " ++ show mpn
inboundGovernorTraceMap (TrResponderStartFailure _ mpn se)   =
  "TrResponderStartFailure " ++ show mpn ++ " " ++ show se
inboundGovernorTraceMap (TrResponderErrored _ mpn se)        =
  "TrResponderErrored " ++ show mpn ++ " " ++ show se
inboundGovernorTraceMap (TrResponderStarted _ mpn)           =
  "TrResponderStarted " ++ show mpn
inboundGovernorTraceMap (TrResponderTerminated _ mpn)        =
  "TrResponderTerminated " ++ show mpn
inboundGovernorTraceMap (TrPromotedToWarmRemote _ ora)        =
  "TrPromotedToWarmRemote " ++ show ora
inboundGovernorTraceMap (TrPromotedToHotRemote _)            =
  "TrPromotedToHotRemote"
inboundGovernorTraceMap (TrDemotedToWarmRemote _)            =
  "TrDemotedToWarmRemote"
inboundGovernorTraceMap (TrDemotedToColdRemote _ ora)         =
  "TrDemotedToColdRemote " ++ show ora
inboundGovernorTraceMap (TrWaitIdleRemote _ ora)              =
  "TrWaitIdleRemote " ++ show ora
inboundGovernorTraceMap (TrMuxCleanExit _)                   =
  "TrMuxCleanExit"
inboundGovernorTraceMap (TrMuxErrored _ se)                  =
  "TrMuxErrored " ++ show se
inboundGovernorTraceMap (TrInboundGovernorCounters _)       =
  "TrInboundGovernorCounters"
inboundGovernorTraceMap (TrRemoteState _)                   =
  "TrRemoteState"
inboundGovernorTraceMap (IG.TrUnexpectedlyFalseAssertion _) =
  "TrUnexpectedlyFalseAssertion"
inboundGovernorTraceMap (TrInboundGovernorError se)           =
  "TrInboundGovernorError " ++ show se


serverTraceMap :: Show ntnAddr => ServerTrace ntnAddr -> String
serverTraceMap (TrAcceptConnection _)     = "TrAcceptConnection"
serverTraceMap st@(TrAcceptError _)       = show st
serverTraceMap st@(TrAcceptPolicyTrace _) = show st
serverTraceMap (TrServerStarted _)        = "TrServerStarted"
serverTraceMap st@TrServerStopped         = show st
serverTraceMap st@(TrServerError _)       = show st

