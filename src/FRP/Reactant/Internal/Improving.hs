{-# LANGUAGE RecordWildCards #-}
module FRP.Reactant.Internal.Future (
--   -- * Types
--   Improving, Clock(..),
--   -- * Creating futures
--   fixedImproving, newImproving, systemClock,
--   -- * Reading the value
--   getImproving
  ) where

import Control.Exception
import Control.Concurrent
import Control.Concurrent.RPC
import Control.Parallel.Strategies
import System.Time
import System.IO.Unsafe
import Data.AddBounds
import Data.Word
import Data.Function
import Data.Heap

import Control.Applicative
import Control.Monad
import Control.Monad.State

import qualified Data.Set as S

import FRP.Reactant.Types



