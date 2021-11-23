{-# LANGUAGE DeriveFunctor #-}

module Seminar7 where

import Control.Monad
import Control.Applicative

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

{-
(>>=) :: [a] -> (a -> [b]) -> [b]
xs >>= k = concat (map k xs)

xs :: [a]
k :: a -> [b]

concat :: forall a. [[a]] -> [a]

map k xs :: [[b]]
concat (map k xs) :: [b]
-}

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

main :: IO ()
main = do
  putStrLn "Hello, what is your name?"
  name <- getLine
  putStrLn $ "Hi, " ++ name
  putStrLn $
    "Gotta go, " ++ name ++
    ", have a nice day"

getLine' :: IO String
getLine' = do
  c <- getChar
  case c == '\n' of
    True -> return []
    False -> do
      cs <- getLine'
      return (c : cs)

putStr' :: String -> IO ()
putStr' [] = return ()
putStr' (x : xs) = putChar x >> putStr' xs

putStr'' :: String -> IO ()
putStr'' = sequence_ . map putChar

putStr''' :: String -> IO ()
putStr''' = mapM_ putChar

{-
foo =
  do { e1 ; e2 }

foo = do
  e1
  e2

foo' = e1 >>= \p -> e2

foo'' =
  do { p <- e1; e2 }

foo''' = do
  p <- e1
  e2     -- p might occur in e2 somehow

foo = let v = e1 in do e2

foo' = do { let v = e1; e2 }

foo'' = do
  let v = e1
  e2      -- v might occur in e2 somehow
-}
{-
instance Monad IO where
  return :: a -> IO a
  return x = IO $ \w -> (w, x)

  (>>=) :: IO a -> (a -> IO b) -> IO b
  -- m :: IO a <-> RealWorld -> (RealWorld, a)
  -- k :: a -> IO b <-> a -> (RealWorld -> (RealWorld, b))

  -- we have to return IO b, that is,
  -- RealWorld -> (RealWorld, b)

  -- w :: RealWorld
  -- m w :: (RealWorld, a)
  -- (w', x) :: (RealWorld, a)
  m >>= k = IO $ \w ->
    case m w of
      (w', x) -> k x w'
-}

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap = undefined

instance Applicative (Reader r) where
  pure = undefined
  (<*>) = undefined

instance Monad (Reader r) where -- return :: a -> Reader r a return x = Reader $ const x
      -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  f >>= k = Reader $ \e -> let v = runReader f e in runReader (k v) e

ask :: Reader e e
ask = Reader id

asks ::(e -> a) -> Reader e a
asks f = Reader f

local :: (e -> b) -> Reader b a -> Reader e a
local f m = Reader $ runReader m . f

data Environment = Environment { ids  :: [Int]
                               , name :: Int -> String
                               , near :: Int -> (Int, Int) }

inEnv :: Int -> Reader Environment Bool
inEnv i = asks (elem i . ids)

anyInEnv :: (Int, Int) -> Reader Environment Bool
anyInEnv (i, j) = inEnv i ||^ inEnv j
  where
    (||^) = liftA2 (||)

env = Environment [1..10] (\x -> show x) (\x -> (x - 1, x + 1))

checkNeighbours :: Int -> Reader Environment (Maybe String)
checkNeighbours i =
  asks (`near` i) >>= \pair ->
  anyInEnv pair   >>= \res  ->
  if res
  then Just <$> asks (`name` i)
  else pure Nothing

type Bindings = [(String,Int)]

greeter :: Reader String String
greeter = do
  name <- ask
  return ("hello, " ++ name ++ "!")

 -- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

 -- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
  count <- asks (lookupVar "count")
  bindings <- ask
  return (count == length bindings)

lookupVar :: String -> Bindings -> Int
lookupVar name bindings = maybe 0 id (lookup name bindings)

sampleBindings = [("count",3), ("1",1), ("b",2)]

action = do
    putStr $ "Count is correct for bindings " ++ (show sampleBindings) ++ ": "
    putStrLn $ show (isCountCorrect sampleBindings)

calculateContentLen :: Reader String Int
calculateContentLen = do
  content <- ask
  return (length content)

-- Calls calculateContentLen after adding a prefix to the Reader content.
calculateModifiedContentLen :: Reader String Int
calculateModifiedContentLen = local ("Prefix " ++) calculateContentLen

action' = do
  let s = "12345";
  let modifiedLen = runReader calculateModifiedContentLen s
  let len = runReader calculateContentLen s
  putStrLn $ "Modified 's' length: " ++ (show modifiedLen)
  putStrLn $ "Original 's' length: " ++ (show len)

newtype Writer w a = Writer { runWriter :: (a, w) }

instance Functor (Writer w) where
  fmap = undefined

instance (Monoid w) => Applicative (Writer w) where
  pure = undefined
  (<*>) = undefined

instance Monoid w => Monad (Writer w) where
  Writer (x,v) >>= f = let (Writer (y, v')) = f x in Writer (y, v `mappend` v')

tell :: Monoid w => w -> Writer w ()
tell w = Writer ((), w)

listen :: Monoid w => Writer w a -> Writer w (w, a)
listen (Writer (a,w)) = Writer ((w,a),w)

pass :: Monoid w => Writer w (a, w -> w) -> Writer w a
pass (Writer ((a, f), w)) = Writer (a, f w)

execWriter :: Writer w a -> w
execWriter (Writer p) = snd p

writer :: (a, w) -> Writer w a
writer = Writer

binPow :: Int -> Int -> Writer String Int
binPow 0 _      = return 1
binPow n a
  | even n    = binPow (n `div` 2) a >>= \b ->
                Writer (b * b, "Square " ++ show b ++ "\n")
  | otherwise =
      binPow (n - 1) a >>= \b ->
      Writer
        (a * b, "Mul " ++ show a ++ " and " ++ show b ++ "\n")

type MyWriter = Writer [Int] String

half :: Int -> Writer String Int
half x = do
        tell ("I just halved " ++ (show x) ++ "!")
        return (x `div` 2)

example :: MyWriter
example  = do
  tell [1..3]
  tell [3..5]
  return "foo"

output :: (String, [Int])
output = runWriter example

deleteOn :: (Monoid w) => (w -> Bool) -> Writer w a -> Writer w a
deleteOn p m = pass $ do
  (w, a) <- listen m
  case p w of
    True -> return (a, id)
    False -> return (a, const mempty)

  -- Or pass alone
deleteOn' :: (Monoid w) => (w -> Bool) -> Writer w a -> Writer w a
deleteOn' p m = pass $ do
  a <- m
  return (a, (\w -> if p w then mempty else w))

logTwo :: Writer [String] ()
logTwo = do
  deleteOn ((> 5) . length . head) $ tell ["foo"]
  deleteOn ((> 5) . length . head) $ tell ["foobar"]

logNumber :: Int -> Writer [String] Int
logNumber x = writer (x, ["Got number: " ++ show x])

logNumber2 :: Int -> Writer [String] Int
logNumber2 x = do
  tell ["Got number: " ++ show x]
  return x

multWithLog :: Writer [String] Int
multWithLog = do
  a <- logNumber 3
  b <- logNumber 5
  tell ["multiplying " ++ show a ++ " and " ++ show b ]
  return (a*b)
