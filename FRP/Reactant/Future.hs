module FRP.Reactant.Future where

import Control.Applicative
import Data.Monoid

import FRP.Reactant.Time


-- | A Future is a value that becomes defined ("arrives") only at some point in the future.
-- The associated time defines when that is, exactly. Though it isn't enforced here, the time
-- should be defined at the same time as the value, or earlier.
--
-- Monoid: mempty is a future that never arrives (infinite time, undefined value),
-- a `mappend` b is the earlier of a and b, preferring a when simultaneous.
--
-- Functor: apply a function to a future argument. The (future) result
-- arrives simultaneously with the argument.
--
-- Applicative: pure gives the value arriving at mempty, <*> applies a future function to
-- a future argument, yielding a future result whose arrival time is constructed by mappend.
newtype Future t a = Future (t,a)

unFuture :: Future t a -> a
unFuture (Future (_,a)) = a

futureT :: Future t a -> t
futureT (Future (t,_)) = t

isNeverF :: (Bounded t, Eq t) => Future t a -> Bool
isNeverF (Future (t,_)) = t == maxBound

instance (Bounded t, Ord t) => Monoid (Future t a) where
  mempty = Future (maxBound, error "Future: mempty")
  mappend a@(Future (t,_)) b@(Future (t2,_)) = if t <= t2 then a else b

instance Functor (Future t) where
  fmap f (Future (t,a)) = Future (t, f a)

instance (Monoid t) => Applicative (Future t) where
  pure a = Future (mempty, a)
  (Future (t,f)) <*> (Future (t2,a)) = Future (mappend t t2, f a)
