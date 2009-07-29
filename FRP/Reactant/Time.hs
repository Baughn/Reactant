{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module FRP.Reactant.Time(
  -- * Types
  Time, ITime,
  module FRP.Reactant.Types,
  -- * Making time
  newTime, newTime', deriveTime,
  -- * Unravelling time
  unITime,
  -- * For internal use in Reactant: DO NOT TOUCH
  getTime, withTime, laterThan
  ) where

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception

import Data.AffineSpace
import Data.Int(Int64)
import FRP.Reactant.Types
import System.Time

import qualified System.IO.Unsafe as IOU


-- | We'll be using this a lot. Abandon all hope ye who enter here.
io :: IO a -> a
io = IOU.unsafePerformIO


-- | Nanoseconds since 1970
type Time = Int64


lastTimeV :: MVar Int64
lastTimeV = io $ newMVar 0

lockTime :: IO a -> IO a
lockTime = withMVar lastTimeV . const

-- | Returns the number of nanoseconds since 1970.
getTime :: IO Time
getTime = do
  TOD s ps <- getClockTime
  return $ fromInteger $ s * 1000000000 + div ps 1000

-- | Passes the approximate number of nanoseconds since 1970.
--
-- The purpose for using a lock here is to ensure that, while the lock is held,
-- no ITimes can be completed. Furthermore, if they later are, their value will be
-- greater than that passed by withTime.
withTime :: (Time -> IO a) -> IO a
withTime act = modifyMVar lastTimeV $ \lastTime -> do
  newTime <- max (lastTime + 1) <$> getTime
  ret <- act newTime
  return (newTime,ret)

-- | Runs an action at a point in time strictly later than the time passed.
-- This function returns immediately, forking off a thread if it has to wait.
laterThan :: Time -> Action -> IO ()
laterThan time act = do
  now <- getTime
  if now > time
    then act
    else forkIO (sleepPast now time >> act) >> return ()
  where
    sleepPast now time = do
      threadDelay (((fromIntegral $ time - now) `div` 1000) + 1)
      now' <- getTime
      when (now' <= time) (putStrLn "Underslept!" >> sleepPast now' time)

-- | Most functionality is exported through the instances
data ITime = ITime { 
  offset :: Time -- ^ If the time is based on the clock, and is as yet unknown, then this is an offset to add to the current time when it becomes known.
  ,completeV :: MVar Bool -- ^ True once the time is known
  ,timeV :: MVar Time    -- ^ Empty until the time is known
  ,qsemsV :: MVar [QSem] } -- ^ A list of semaphores to signal when the time becomes complete, empty thereafter.


-- | Called to complete an ITime, if it isn't created complete. Uses the clock, adds the offset, signals QSems.
complete :: ITime -> IO ()
complete itime@ITime{offset} = withTime $ \now -> complete' itime (now+offset)

-- | Called to complete an ITime. Does not add the offset - careful!
complete' :: ITime -> Time -> IO ()
complete' ITime{..} time = do
  modifyMVar_ completeV $ const $ do
    putMVar timeV time
    return True
  mapM_ signalQSem =<< takeMVar qsemsV


-- | Sets up a semaphore to be signalled once the ITime is complete, or immediately if it already is.
addSem :: ITime -> QSem -> IO ()
addSem ITime{..} sem =
  withMVar completeV $ \complete -> do
    if complete
      then signalQSem sem
      else modifyMVar_ qsemsV (return . (sem :))

-- | Creates a new ITime with an already-known value.
newTime :: Time -> ITime
newTime time = io $ ITime undefined <$> newMVar True <*> newMVar time <*> newEmptyMVar

-- | Creates a new ITime, whose value is the current time of day when the corresponding Action is executed.
newTime' :: IO (ITime, Action)
newTime' = do
  itime <- ITime 0 <$> newMVar False <*> newEmptyMVar <*> newMVar []
  return (itime, complete itime)

-- | Creates a new ITime with a specified offset from the old one.
-- Performance warning: Forks a thread.
deriveTime :: ITime -> Int64 -> ITime
deriveTime time@ITime{offset = oldOffset} extraOffset = io $ do
  sem <- newQSem 0
  addSem time sem
  let offset = oldOffset + extraOffset
  derived <- ITime offset <$> newMVar False <*> newEmptyMVar <*> newMVar []
  -- FIXME: It should be possible to avoid this forkIO; store Actions instead of QSems in ITime.
  forkIO $ waitQSem sem >> complete' derived (unITime time + offset)
  return derived
  

-- | Reads the time from an ITime, waiting for it to be complete if necessary
unITime :: ITime -> Time
unITime ITime{timeV} = io $ readMVar timeV

-- | Checks whether an ITime is already complete. Be careful, it might complete just after
-- calling this function.
isComplete :: ITime -> IO Bool
isComplete ITime{completeV} = readMVar completeV

-- | Waits until at least one ITime is complete and the other is either complete or known to
-- be larger than the first, then passes the complete ITime as the first parameter
-- to the passed action. If this flips them around, then the return value is returned in a Right,
-- otherwise in a Left. If they are both known to be complete, the Bool is True, otherwise False.
-- Furthermore, the function is called with time updates locked.
wait :: ITime -> ITime -> (ITime -> Bool -> ITime -> IO a) -> IO (Either a a)
wait it1 it2 act = do
  -- Wait for at least one to be complete
  sem <- newQSem 0
  addSem it1 sem
  addSem it2 sem
  waitQSem sem
  -- Check which is complete. At least one is. Note: Branch for one-incomplete must work even if it's complete (race condition).
  c1 <- isComplete it1
  c2 <- isComplete it2
  case (c1,c2) of
    (True,True) -> Left <$> call it1 True it2
    (True,False) -> do it1 `wait4` it2; Left <$> call it1 False it2
    (False,True) -> do it2 `wait4` it1; Right <$> call it2 False it1
  where
    -- | a `wait4` b: wait for b to be knowably later than a, or complete
    a `wait4` b@ITime{offset} = do
      let threshold = unITime a - offset
      sem <- newQSem 0
      laterThan (threshold - offset) (signalQSem sem)
      addSem it2 sem
      waitQSem sem
    call a both b = block $ lockTime $ act a both b

-- To compare for equality, it suffices that one ITime is defined and the other's
-- clock is past the complete one's value.
instance Eq ITime where
  it1 == it2 = io $ either id id <$> do
    wait it1 it2 $ \complete bothComplete possible -> do
      if bothComplete
        then return (unITime complete == unITime possible)
        else return False
    
-- For more complex orderings, the same goes. One ITime must be defined, the other
-- must be past any possibility of being equal or lesser than the first.
instance Ord ITime where
  compare it1 it2 = io $ either id reverseOrdering <$> do
    wait it1 it2 $ \complete bothComplete possible -> do
      if bothComplete
        then return (unITime complete `compare` unITime possible)
        else return LT        
    where
      reverseOrdering LT = GT
      reverseOrdering EQ = EQ
      reverseOrdering GT = LT

-- FIXME: Orphaned instance. For adoption by Conal!
-- instance AdditiveGroup Int64 where
--   zeroV = 0
--   (^+^) = (+)
--   negateV = negate

-- instance AffineSpace ITime where
--   type Diff ITime = Int64
--   (.-.) = undefined -- FIXME: Not quite sure about this one yet.
--   itime .+^ diff =  -- Just use deriveTime for now.

instance Show ITime where
  show time = "ITime " ++ show (unITime time)

instance Read ITime where
  readsPrec 11 _ = error "No clue how to deal with this!"
  readsPrec _ ('I':'T':'i':'m':'e':' ':numstr) = map (\(num,str) -> (newTime num,str)) $ readsPrec 10 numstr