module Network.Server where

import Structure.Lens

import Network
import Network.Socket hiding (accept)

import System.IO
import Control.Concurrent
import Control.Exception hiding (catch)

import Data.Word

server ::
  ([ThreadId] -> Accept -> IO ())
  -> IO a
server f =
  let hand i s = do u <- accept' s
                    _ <- forkIO (f i u)
                    hand i s
  in do s  <- listenOn (PortNumber 6060)
        hand [] s `finally` sClose s

data Accept =
  Accept
    Handle
    HostName
    PortNumber
  deriving (Eq, Show)

handleL ::
  Lens Accept Handle
handleL =
  Lens
    (\(Accept _ nam num) hd -> Accept hd nam num)
    (\(Accept hd _ _) -> hd)

hostNameL ::
  Lens Accept HostName
hostNameL =
  Lens
    (\(Accept hd _ num) nam -> Accept hd nam num)
    (\(Accept _ nam _) -> nam)

portNumberL ::
  Lens Accept PortNumber
portNumberL =
  Lens
    (\(Accept hd nam _) num -> Accept hd nam num)
    (\(Accept _ _ num) -> num)

accept' ::
  Socket
  -> IO Accept
accept' =
  fmap (\(hd, nam, num) -> Accept hd nam num) . accept

