{-# LANGUAGE DeriveFunctor #-}

module Seminar7 where

toKleisli :: Monad m => (a -> b) -> a -> m b
toKleisli f = return . f

cosM :: (Monad m, Floating b) => b -> m b
cosM = toKleisli cos

newtype Identity a = Identity { runIdentity :: a }
    deriving (Show, Functor)

instance Applicative Identity where
  pure = Identity
  Identity f <*> Identity x = Identity (f x)

instance Monad Identity where
  Identity x >>= k = k x

cosId, acosId, sinM
  :: Double -> Identity Double
cosId = Identity . cos
acosId = Identity . acos
sinM = Identity . sin


go = cosId (pi/2) >>= acosId >>= sinM

go2 = cosId (pi/2) >>= (\x ->
      acosId x     >>= (\y ->
      sinM y       >>= \z ->
      return z))

go2' = cosId (pi/2) >>= (\x ->
       acosId x     >>= (\y ->
       sinM y       >>= \z ->
       return (x, y, z)))

go2'' = let alpha = pi/2 in
            cosId alpha  >>= (\x ->
            acosId x     >>= (\y ->
            sinM y       >>
            return (alpha, x, y)))

go2''' = do
  let alpha = pi/2
  x <- cosId alpha
  y <- acosId x
  z <- sinM y
  return (alpha, x, y)

prodM :: Monad m => (a -> m b) -> (c -> m d)
      -> m (a, c) -> m (b, d)
prodM f g mp =
  mp >>= \(a,b) -> f a >>= \c -> g b >>= \d -> return (c, d)

prodM' :: Monad m => (a -> m b) -> (c -> m d)
       -> m (a, c) -> m (b, d)
prodM' f g mp = do
  (a, b) <- mp
  c <- f a
  d <- g b
  return (c, d)

type Author = String

type Book = String

type Library = [(Author, Book)]

books :: [Book]
books = ["Faust", "Alice in Wonderland", "The Idiot"]

authors :: [Author]
authors = ["Goethe", "Carroll", "Dostoevsky"]

library :: Library
library = zip authors books

library' :: Library
library' = ("Dostoevsky", "Demons") :
  ("Dostoevsky", "White Nights") : library

getBook :: Author -> Library -> Maybe Book
getBook author library = lookup author library

getFirstbook, getLastBook :: Author -> Maybe Book
getFirstbook author = do
  let lib' = filter (\p -> fst p == author) library'
  book <- getBook author lib'
  return book
getLastBook author = do
  let lib' = filter (\p -> fst p == author) library'
  book <- getBook author (reverse lib')
  return book

cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys =
  xs >>= \x -> ys >>= \y -> return (x, y)

cartesianProduct' :: [a] -> [b] -> [(a, b)]
cartesianProduct' xs ys = do
  x <- xs
  y <- ys
  return (x, y)

cartesianProduct'' :: [a] -> [b] -> [(a, b)]
cartesianProduct'' xs ys = [(x, y) | x <- xs, y <- ys]
