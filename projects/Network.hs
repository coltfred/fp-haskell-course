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
import Control.Exception(catch, finally, Exception, IOException)
import Data.Foldable
import Data.Function
import Data.Word
import Data.Set(Set)
import qualified Data.Set as S
import System.Posix

server ::
  ClientThread IO ()
  -> IO ()
server (ClientThread g) =
  let hand s c = printIOException . forever $
                   do q <- accept' s
                      lSetBuffering q NoBuffering
                      _ <- that (modifyMVar_ c (return . S.insert q))
                      that (forkIO (g q c))
  in withSocketsDo $ do
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

(>>-) ::
  ThisOrThat IO IOException a
  -> (a -> IO ())
  -> IO ()
(>>-) =
  flip (thisOrThat print)

game2 =
  ClientThread $ \a c ->
    fix $ \loop -> lGetLine a >>- \l ->
                     do e <- readMVar c
                        mapM_ (\y -> lPutStrLn y l >>- return) (S.delete a e)
                        loop

game ::
  ClientThread IO ()
game =
  ClientThread $ \a c ->
    let x = do l <- lGetLine a
               e <- that (readMVar c)
               mapM_ (\y -> lPutStrLn y l) (S.delete a e)
    in printIOException (forever x)


printIOException ::
  ThisOrThat IO IOException ()
  -> IO ()
printIOException =
  handleThat print

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
  Exception e =>
  Socket
  -> ThisOrThat IO e Accept
accept' =
  fmaap (\(hd, nam, num) -> Accept (Ref hd) nam num) . tcatch . accept

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
  (Exception e, HandleLens h) =>
  h
  -> ThisOrThat IO e String
lGetLine h =
  tcatch (hGetLine (handleL `getL` h))

lPutStrLn ::
  (Exception e, HandleLens h) =>
  h
  -> String
  -> ThisOrThat IO e ()
lPutStrLn h =
  tcatch . hPutStrLn (handleL `getL` h)

lClose ::
  (Exception e, HandleLens h) =>
  h
  -> ThisOrThat IO e ()
lClose h =
  tcatch (hClose (handleL `getL` h))

lSetBuffering ::
  (Exception e, HandleLens h) =>
  h
  -> BufferMode
  -> ThisOrThat IO e ()
lSetBuffering h =
  tcatch . hSetBuffering (handleL `getL` h)

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
