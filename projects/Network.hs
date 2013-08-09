{-# LANGUAGE RankNTypes #-}

module Network.Server where

import Monad.Fuunctor
import Monad.Moonad
import Structure.Lens
import Data.TicTacToe

import Network
import Network.Socket hiding (accept, bind)

import Prelude hiding (mapM_, catch)
import System.IO
import Control.Concurrent
import Control.Monad(forever, when)
import Control.Applicative
import Control.Exception(catch, finally, try, Exception, IOException)
import Data.Foldable
import Data.Function
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S
import System.Posix

eprint ::
  IOException
  -> IO ()
eprint =
  print

server ::
  ClientThread IO ()
  -> IO ()
server (ClientThread g) =
  let hand s c = forever $
                   do q <- accept' s
                      lSetBuffering q NoBuffering
                      _ <- modifyMVar_ c (return . S.insert q)
                      forkIO (g q c)
  in withSocketsDo $ do
       s <- listenOn (PortNumber 6060)
       c <- newMVar S.empty
       hand s c `finally` sClose s

game ::
  ClientThread IO ()
game =
  ClientThread $ \a c ->
    fix $ \loop ->
      do k <- try (lGetLine a)
         case k of Left e -> eprint e
                   Right [] -> loop
                   Right l -> do e <- readMVar c
                                 mapM_ (\y ->
                                          catch (lPutStrLn y l) (\x -> do _ <- modifyMVar_ c (return . S.delete y)
                                                                          eprint x)
                                       ) (S.delete a e)
                                 loop

newtype ClientThread f a =
  ClientThread {
    play ::
      Accept
      -> MVar (Set Accept)
      -> f a
  }

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
  fmaap (\(hd, nam, num) -> Accept (Ref hd) nam num) . accept

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
-- EitherT
newtype ThisOrThat f a b =
  ThisOrThat (
    f (Either a b)
  )

thisOrThat ::
  Moonad f =>
  (a -> f x)
  -> (b -> f x)
  -> ThisOrThat f a b
  -> f x
thisOrThat f g (ThisOrThat x) =
  bind (either f g) x

handleThis ::
  Moonad f =>
  (b -> f a)
  -> ThisOrThat f a b
  -> f a
handleThis =
  thisOrThat reeturn

handleThat ::
  Moonad f =>
  (a -> f b)
  -> ThisOrThat f a b
  -> f b
handleThat f =
  thisOrThat f reeturn

isThis ::
  Fuunctor f =>
  ThisOrThat f a b
  -> f Bool
isThis (ThisOrThat x) =
  fmaap (either (const True) (const False)) x

isThat ::
  Fuunctor f =>
  ThisOrThat f a b
  -> f Bool
isThat =
  fmaap not . isThis

this ::
  Fuunctor f =>
  f a
  -> ThisOrThat f a b
this =
  ThisOrThat . fmaap Left

that ::
  Fuunctor f =>
  f b
  -> ThisOrThat f a b
that =
  ThisOrThat . fmaap Right

swap ::
  Fuunctor f =>
  ThisOrThat f a b
  -> ThisOrThat f b a
swap (ThisOrThat x) =
  ThisOrThat (fmaap (either Right Left) x)

(~.) ::
  Fuunctor f =>
  (ThisOrThat f b a -> ThisOrThat f b a)
  -> ThisOrThat f a b
  -> ThisOrThat f a b
(~.) f =
  swap . f . swap

instance Fuunctor f => Fuunctor (ThisOrThat f a) where
  fmaap f (ThisOrThat x) =
    ThisOrThat (fmaap (fmap f) x)

instance Moonad f => Moonad (ThisOrThat f a) where
  reeturn =
    ThisOrThat . reeturn . Right
  bind f (ThisOrThat x) =
    ThisOrThat (bind (either (reeturn . Left) (\r -> let ThisOrThat q = f r in q)) x)

instance Monad f => Monad (ThisOrThat f a) where
  return =
    ThisOrThat . return . Right
  ThisOrThat x >>= f =
    ThisOrThat (x >>= either (return . Left) (\r -> let ThisOrThat q = f r in q))

tcatch ::
  Exception e =>
  IO a
  -> ThisOrThat IO e a
tcatch x =
  ThisOrThat (catch (fmaap Right x) (\e -> return (Left e)))

