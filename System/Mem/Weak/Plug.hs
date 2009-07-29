module System.Mem.Weak.Plug(Plug, newPlug, touchPlug, addPlugFinalizer) where

import Control.Concurrent.MVar

-- | A handy key for use in System.Mem.Weak
type Plug = MVar ()

-- | Creates a plug.
newPlug :: IO Plug
newPlug = newMVar ()

-- | The plug will not be garbage-collected before the last call to touchPlug.
touchPlug :: Plug -> IO ()
touchPlug plug = modifyMVar_ plug return

-- | Add a finalizer to a plug
addPlugFinalizer :: Plug -> IO () -> IO ()
addPlugFinalizer = addMVarFinalizer
