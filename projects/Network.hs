{-# LANGUAGE FlexibleInstances #-}

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

acceptHandleL ::
  Lens Accept Handle
acceptHandleL =
  Lens
    (\(Accept _ nam num) hd -> Accept hd nam num)
    (\(Accept hd _ _) -> hd)

acceptHostNameL ::
  Lens Accept HostName
acceptHostNameL =
  Lens
    (\(Accept hd _ num) nam -> Accept hd nam num)
    (\(Accept _ nam _) -> nam)

acceptPortNumberL ::
  Lens Accept PortNumber
acceptPortNumberL =
  Lens
    (\(Accept hd nam _) num -> Accept hd nam num)
    (\(Accept _ _ num) -> num)

accept' ::
  Socket
  -> IO Accept
accept' =
  fmap (\(hd, nam, num) -> Accept hd nam num) . accept

