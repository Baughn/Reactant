module Control.Concurrent.RPC(
  RPC,
  query, query_,
  receive,
  newRPC
  ) where

import FRP.Reactant.Types
import Control.Concurrent.MVar
import Control.Applicative

-- * RPC Implementation

data RPC q r = RPC (MVar (q, Sink r))


-- | Create an RPC channel
newRPC :: IO (RPC q r)
newRPC = RPC <$> newEmptyMVar


-- | Ask an RPC server something
query :: RPC q r -> q -> IO r
query (RPC comm) q = do
  retComm <- newEmptyMVar
  putMVar comm (q, putMVar retComm)
  takeMVar retComm

-- | Ask an RPC server something and discard the result.
-- Still waits for the server to complete the query, however.
query_ :: RPC q r -> q -> IO ()
query_ rpc q = query rpc q >> return ()

-- | Receive a query. Reply via the sink.
receive :: RPC q r -> IO (q, Sink r)
receive (RPC comm) = takeMVar comm

