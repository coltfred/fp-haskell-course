module Network.Server where

import Structure.Lens
import Data.TicTacToe

import Network
import Network.Socket hiding (accept)

import Prelude hiding (mapM_, catch)
import System.IO
import Control.Concurrent
import Control.Monad(forever, when)
import Control.Exception(catch, finally, IOException)
import Data.Foldable
import Data.Function
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S
import System.Posix

server ::
  ClientThread IO ()
  -> IO a
server (ClientThread g) =
  let hand s c = forever $
                   do q <- accept' s
                      lSetBuffering q NoBuffering
                      _ <- modifyMVar_ c (return . S.insert q)
                      forkIO (g q c)
  in do withSocketsDo $ do
          s <- listenOn (PortNumber 6060)
          c <- newMVar S.empty
          hand s c `finally` sClose s

newtype ClientThread f a =
  ClientThread {
    play ::
      Accept
      -> MVar (Set Accept)
      -> f a
  }

game ::
  ClientThread  IO ()
game =
  ClientThread $ \a c ->
    let x = do l <- lGetLine a
               e <- readMVar c
               mapM_ (\y -> lPutStrLn y l) (S.delete a e)
    in forever x

newtype Ref =
  Ref Handle
  deriving (Eq, Show)

instance Ord Ref where
  compare =
    compare `on` show

data Accept =
  Accept
    Ref
    HostName
    PortNumber
  deriving (Eq, Ord, Show)

refL ::
  Lens Accept Ref
refL =
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
  fmap (\(hd, nam, num) -> Accept (Ref hd) nam num) . accept

class HandleLens a where
  handleL ::
    Lens a Handle

instance HandleLens Handle where
  handleL =
    identityL

instance HandleLens Ref where
  handleL =
    iso (\(Ref h) -> h) Ref

instance HandleLens Accept where
  handleL =
    refL .@ handleL

lGetLine ::
  HandleLens h =>
  h
  -> IO String
lGetLine h =
  hGetLine (handleL `getL` h)

lPutStrLn ::
  HandleLens h =>
  h
  -> String
  -> IO ()
lPutStrLn h =
  hPutStrLn (handleL `getL` h)

lClose ::
  HandleLens h =>
  h
  -> IO ()
lClose h =
  hClose (handleL `getL` h)

lSetBuffering ::
  HandleLens h =>
  h
  -> BufferMode
  -> IO ()
lSetBuffering h =
  hSetBuffering (handleL `getL` h)

lIsWritable ::
  HandleLens a =>
  a
  -> IO Bool
lIsWritable h =
  hIsWritable (handleL `getL` h)

lIsReadable ::
  HandleLens a =>
  a
  -> IO Bool
lIsReadable h =
  hIsWritable (handleL `getL` h)
