module Cardano.KESAgent.Pretty
where

import Cardano.KESAgent.Agent
import Cardano.KESAgent.ControlClient
import Cardano.KESAgent.ServiceClient
import Cardano.KESAgent.Driver
import Cardano.KESAgent.RefCounting

class Pretty a where
  pretty :: a -> String

strLength :: String -> Int
strLength = length

instance Pretty AgentTrace where
  pretty (AgentServiceDriverTrace d) = "Agent: ServiceDriver: " ++ pretty d
  pretty (AgentControlDriverTrace d) = "Agent: ControlDriver: " ++ pretty d
  pretty (AgentServiceClientConnected a) = "Agent: ServiceClientConnected: " ++ a
  pretty (AgentServiceClientDisconnected a) = "Agent: ServiceClientDisconnected: " ++ a
  pretty (AgentServiceSocketError e) = "Agent: ServiceSocketError: " ++ e
  pretty (AgentControlClientConnected a) = "Agent: ControlClientConnected: " ++ a
  pretty (AgentControlClientDisconnected a) = "Agent: ControlClientDisconnected: " ++ a
  pretty (AgentControlSocketError e) = "Agent: ControlSocketError: " ++ e
  pretty x = "Agent: " ++ drop (strLength "Agent") (show x)

instance Pretty ControlClientTrace where
  pretty (ControlClientDriverTrace d) = "Control: Driver: " ++ pretty d
  pretty ControlClientConnected = "Control: Connected"
  pretty x = "Control: " ++ drop (strLength "ControlClient") (show x)

instance Pretty ServiceClientTrace where
  pretty (ServiceClientDriverTrace d) = "Service: Driver: " ++ pretty d
  pretty ServiceClientConnected = "Service: Connected"
  pretty x = "Service: " ++ drop (strLength "ServiceClient") (show x)

instance Pretty DriverTrace where
  pretty x = drop (strLength "Driver") (show x)

instance Pretty CRefEvent where
  pretty = show
