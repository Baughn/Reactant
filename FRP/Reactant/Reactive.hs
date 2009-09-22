-- | Representation for Reactive and Event types. Combined here, because they're mutually recursive.
--
-- The representation used in this module is based on a close connection between these two types.
-- A reactive value is defined by an initial value and an event that yields future values; 
-- while an event is given as a future reactive value. 
module FRP.Reactant.Reactive where

import Control.Applicative
import Data.Monoid

import FRP.Reactant.Time
import FRP.Reactant.Future

-- | A reactive value is, a value that changes over time in discrete steps.
-- It is defined at all times.
--
-- Given a reactive value r and a time t, r `at` t = the value at time t.
-- The various instances are given using the same semantics as for a
-- value that doesn't change.
--
-- Monoid: a typical lifted monoid. If o is a monoid, then Reactive o is a monoid,
-- with mempty == pure mempty, and mappend == liftA2 mappend. That is, mempty at
-- t == mempty, and (r mappend s) at t == (r at t) mappend (s at t).
--
-- Functor: (fmap f r) `at` t == f (r `at` t)
--
-- Applicative: (pure v) `at` t == v
--
-- <*> creates a reactive value that steps whenever either of its parameter
-- values step.
--
-- Eq and Ord are implemented using the Applicative instance, so act similarly
-- except that they use compressR to remove extra steps.
data Reactive t a = a `Stepper` (Event t a)

-- | Events. Semantically, a time-ordered list of future values. Instances:
--
-- Monoid: mempty is the event that never occurs, and e `mappend` e' is the
-- event that combines occurrences from e and e'.
--
-- Functor: fmap f e is the event that occurs whenever e occurs, and whose
-- occurence value comes from applying f to the values from e.
--
-- Applicative, Monad: I'm open for suggestions. What would be useful?
data Event t a = Event { eFuture :: Future t (Reactive t a) }


isNeverE :: (Bounded t, Eq t) => Event t a -> Bool
isNeverE = isNeverF . eFuture

-- | Apply an unary function inside an Event representation
inEvent :: (Future s (Reactive s a) -> Future t (Reactive t b)) -> Event s a -> Event t b
inEvent f = Event . f . eFuture

-- | Apply a binary function inside an Event representation
inEvent2 :: (Future r (Reactive r a) -> Future s (Reactive s b) -> Future t (Reactive t c))
            -> Event r a
            -> Event s b
            -> Event t c
inEvent2 f ex ey = Event (f (eFuture ex) (eFuture ey))

-- | Make the event into a list of futures
eFutures :: (Bounded t, Eq t) => Event t a -> [Future t a]
eFutures e | isNeverE e = []
eFutures (Event (Future (t,a `Stepper` e))) = Future (t,a) : eFutures e

-- | Apply a unary function inside the Event part of a Reactive representation
-- (Thus skipping the initial value)
inREvent :: (Event s a -> Event t a) -> Reactive s a -> Reactive t a
inREvent f (a `Stepper` e) = a `Stepper` f e

-- | Apply a unary function inside the future reactive inside a Reactive representation
inFutR :: (Future s (Reactive s b) -> Future t (Reactive t b)) -> Reactive s b -> Reactive t b
inFutR = inREvent . inEvent
