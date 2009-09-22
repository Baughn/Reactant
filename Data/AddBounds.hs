module Data.AddBounds where

-- | Wrap a type into one having new least and greatest elements, preserving the existing ordering.
data AddBounds a = MinBound | NoBound a | MaxBound
                 deriving(Eq,Ord,Read,Show)

unAB :: AddBounds a -> a
unAB (NoBound a) = a

instance Bounded (AddBounds a) where { minBound = MinBound; maxBound = MaxBound }
