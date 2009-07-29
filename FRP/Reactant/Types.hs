module FRP.Reactant.Types where

type Action = IO ()

type Sink a = a -> Action
