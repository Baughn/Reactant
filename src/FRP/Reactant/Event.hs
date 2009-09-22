module FRP.Reactant.Event(
  Event, Time,
  -- * Constructing events
  listE, atTime, atTimes
  ) where

import Control.Applicative
import Data.Monoid
import Data.Function
import Data.Int(Int64)
import Data.AddBounds
import Data.List
-- import Data.AffineSpace
import Control.Arrow
import Control.Monad
-- import Debug.Trace


-- import FRP.Reactant.Internal.Time

-- | An event is semantically a time-ordered, possibly finite list of
-- ('Time',value) pairs, each known as an event occurence.
--
-- Most of an Event's functionality is provided through type classes.
--
-- * Eq: Compares only time values
--
-- * Ord: Compares only time values, earlier occurences have higher significance
--
-- * Functor: 'fmap' /f ev/ is an event that has occurences simultaneously
-- with /ev/, but with the value of each occurence being f applied to the value
-- of the occurence in /ev/.
-- 
-- * Applicative: 'pure _' is an event with a single occurence at time=-inf.
-- /ev1/ '<*>' /ev2/ is an event with an occurence at every occurence in ev1 or
-- ev2, starting at the time of whichever event has the latest first occurence.
-- Simultaneous occurences produce only one new one. The value of each occurence
-- is the function-typed value of the most recent occurence in ev1 applied to the
-- most recent occurence in /ev2/.
--
-- * Monad: 'join' /ev/ first maps the time of every occurence of every event-valued
-- occurence of /ev/ to the maximum of its own time and the time of the outer occurence.
-- It then merges every occurence in the two-layer event into a single event,
-- using 'mappend'.
--
-- * Monoid: 'mempty' is the event with no occurences. /ev1/ 'mappend' /ev2'
-- interleaves the events according to left-biased temporal order.
newtype Event v = Ev [Occurence v]
                deriving(Show,Eq,Ord)
-- Implementor's note: The list is terminated by []; the last element may
-- or may not have MaxBound for time.

data Occurence v = Occ { occT :: Time, occV :: v }

instance Functor Occurence where
  fmap f (Occ t v) = Occ t (f v)

instance Eq (Occurence v) where
  (Occ t _) == (Occ u _) = t == u

instance Ord (Occurence v) where
  compare (Occ t _) (Occ u _) = compare t u

-- Requirements for Time:
--
-- * It's an instance of Bounded, with infinite bounds
--
-- * Its Ord instance evaluates <= and compare with minimal information

-- FIXME: Implement properly
type Time = AddBounds Int64

instance Num a => Num (AddBounds a) where
  fromInteger = NoBound . fromInteger
  (+) = undefined
  (*) = undefined
  abs MinBound = MaxBound
  abs (NoBound a) = NoBound (abs a)
  abs MaxBound = MaxBound
  signum MinBound = NoBound (-1)
  signum (NoBound a) = NoBound (signum a)
  signum MaxBound = NoBound 1

-- * Event instances

instance Show v => Show (Occurence v) where
  showList occs = (++) $ concat $ "[" : intersperse "," (map show occs) ++ ["]"]
  show (Occ MaxBound v) = "MaxBound: " ++ show v
  show (Occ MinBound v) = "MinBound: " ++ show v
  show (Occ (NoBound t) v) = show t ++ ": " ++ show v

instance Functor Event where
  fmap f (Ev occs) = Ev $ map (fmap f) occs

instance Monoid (Event v) where
  mempty = Ev []
  mappend (Ev occs1) (Ev occs2) = Ev (merge occs1 occs2)
    where merge [] p = p
          merge o [] = o
          merge o p = if head o <= head p 
                      then head o : merge (tail o) p
                      else head p : merge o (tail p)

instance Applicative Event where
  pure f = Ev [Occ MinBound f]
  (Ev []) <*> _ = Ev []
  _ <*> (Ev []) = Ev []
  (Ev occfs) <*> (Ev occvs) = Ev $ unfoldr apply (trim occfs, trim occvs)
    where startTime = max (occT $ head occfs) (occT $ head occvs)
          trim occs = uncurry (:) $ first (\a -> (last a) {occT = startTime}) $ partition ((<= startTime) . occT) occs
          apply ([],_) = Nothing
          apply (_,[]) = Nothing
          apply (fs@(Occ t f:fs'),vs@(Occ t2 v:vs')) = case compare (tNext fs') (tNext vs') of
            LT -> Just (Occ t (f v) , (fs', vs))
            EQ -> Just (Occ t (f v) , (fs', vs'))
            GT -> Just (Occ t2 (f v), (fs,  vs'))
          tNext [] = MaxBound
          tNext (Occ t _ : _) = t


-- (Ev []) <> _ = Ev []
-- _ <> (Ev []) = Ev []
-- (Ev occfs) <> (Ev occvs) = Ev $ unfoldr apply (trim occfs, trim occvs)
--   where startTime = max (occT $ head occfs) (occT $ head occvs)
--         trim occs = uncurry (:) $ first last $ partition ((<= startTime) . occT) occs
--         apply ([],_) = Nothing
--         apply (_,[]) = Nothing
--         apply (fs@(Occ t f:fs'),vs@(Occ t2 v:vs')) = case compare t t2 of
--           LT -> Just (Occ t (f v) , (fs', vs))
--           EQ -> Just (Occ t (f v) , (fs', vs'))
--           GT -> Just (Occ t2 (f v), (fs,  vs'))


-- ts :: Show a => a -> a
-- ts = join traceShow


-- * Introducing events

atTime :: Time -> Event ()
atTime t = listE [(t,())]

atTimes :: [Time] -> Event ()
atTimes ts = listE [(t,()) | t <- ts]

-- | Construct an event from a pre-existing list of times and values.
listE :: [(Time,v)] -> Event v
listE occs = Ev $ uncurry Occ <$> occs
