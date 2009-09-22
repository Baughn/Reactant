module FRP.Reactant.Types(
  Action, Sink
  ) where

-- | Limits the number of parantheses
type Action = IO ()

-- | A generic IO sink
type Sink a = a -> Action
