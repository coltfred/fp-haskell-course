module Network.Server where

import Structure.Lens
import Data.TicTacToe

import Network
import Network.Socket hiding (accept)

import System.IO
import Control.Concurrent
import Control.Exception hiding (catch)

import Data.Word

server ::
  Game ()
  -> IO a
server (Game g) =
  let hand s = do p1 <- accept' s
                  hPutStrLn (handleL `getL` p1) "PLAYER 1"
                  p2 <- accept' s
                  hPutStrLn (handleL `getL` p2) "PLAYER 2"
                  _ <- forkIO (g p1 p2)
                  hand s
  in do s <- listenOn (PortNumber 6060)
        hand s `finally` sClose s

newtype Game a =
  Game {
    play ::
      Accept
      -> Accept
      -> IO a
  }

game ::
  Game ()
game =
  Game undefined

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

