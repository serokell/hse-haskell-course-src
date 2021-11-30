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
  -- fmap :: (a -> b) -> Reader r a -> Reader r b
  -- f :: a -> b, g :: r -> a
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
  -- pure :: a -> Reader r a
  pure x = Reader $ const x

  -- (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  -- f :: r -> a -> b
  -- g :: r -> a
  -- smth :: r -> b
  Reader f <*> Reader g = Reader $ (\x -> f x (g x))

instance Monad (Reader r) where
  -- (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  -- runReader f :: r -> a
  -- e :: r
  -- v = runReader f e :: a

  -- k :: a -> Reader r b
  -- k v :: Reader r b
  -- runReader (k v) :: r -> b
  -- runReader (k v) e :: b
  -- Reader r b
  f >>= k =
    Reader $ \e -> let v = runReader f e in runReader (k v) e

ask :: Reader e e
ask = Reader id

asks :: (e -> a) -> Reader e a
asks f = Reader f

local :: (e -> b) -> Reader b a -> Reader e a
local f m = Reader $ runReader m . f

type Id = Int

data Environment = Environment { ids  :: [Id]
                               , name :: Id -> String
                               , near :: Id -> (Id, Id) }

inEnv :: Id -> Reader Environment Bool
inEnv i = asks (elem i . ids)

anyInEnv :: (Id, Id) -> Reader Environment Bool
anyInEnv (i, j) = inEnv i ||^ inEnv j
  where
    -- Bool -> Bool -> Bool
    -- Reader Env Bool -> Reader Env Bool -> Reader Env Bool
    (||^) = liftA2 (||)

env = Environment [1..10] (\x -> "This is " ++ show x) (\x -> (x - 1, x + 1))

checkNeighbours :: Id -> Reader Environment (Maybe String)
checkNeighbours i =
  asks (`near` i) >>= \pair ->
  anyInEnv pair   >>= \res  ->
  if res
  then Just <$> asks (`name` i)
  else pure Nothing

greeter :: Reader String String
greeter = do
  name <- ask
  return ("hello, " ++ name ++ "!")

type Bindings = [(String,Int)]
 -- Returns True if the "count" variable contains correct bindings size.
isCountCorrect :: Bindings -> Bool
isCountCorrect bindings = runReader calc_isCountCorrect bindings

 -- The Reader monad, which implements this complicated check.
calc_isCountCorrect :: Reader Bindings Bool
calc_isCountCorrect = do
  count <- asks (lookupVar "count")
  bindings <- ask
  return (count == length bindings)

-- lookup :: Eq a => a -> [(a,b)] -> Maybe b

lookupVar :: String -> Bindings -> Int
lookupVar name bindings = maybe 0 id (lookup name bindings)

sampleBindings = [("1",1), ("b",2)]

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
  let s = "12345"
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

-- foo x y
-- x `foo` y

-- foo x y z
-- y `foo x` z

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

deleteOn :: (Monoid w) => (w -> Bool) -> Writer w a -> Writer w a
deleteOn p m = pass $ do
  (w, a) <- listen m
  case p w of
    True -> return (a, id)
    False -> return (a, const mempty)

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

-------- State examples ---------------------------

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  -- fmap :: (a -> b) -> State s a -> State s b
  fmap f (State g) = State $ \s -> let (x, y) = g s in (f x, y)

instance Applicative (State s) where
  -- pure :: a -> State s a
  pure x =  State $ \s -> (x, s)

  -- (<*>) :: State s (a -> b) -> State s a -> State s b
  (State sa) <*> (State sb) = State $
                                \s -> let (fn, s1) = sa s
                                          (a, s2)  = sb s1
                                      in (fn a, s2)

instance Monad (State s) where
  -- (>>=) :: State s a -> (a -> State s b) -> State s b
  State act >>= f = State $ \s ->
    let (a, s') = act s
    in runState (f a) s'

get :: State s s
get = State $ \s -> (s, s)

put :: s -> State s ()
put s = State $ \_ -> ((), s)

modify :: (s -> s) -> State s ()
modify f = do
  s <- get
  put (f s)

gets :: (s -> a) -> State s a
gets f = do
  s <- get
  return (f s)

withState :: (s -> s) -> State s a -> State s a
withState f s = modify f >> s

evalState :: State s a -> s -> a
evalState (State f) s = fst $ f s

execState :: State s a -> s -> s
execState (State f) s = snd $ f s

type Stack = [Int]

emptyStack :: Stack
emptyStack = []

pop :: State Stack Int
pop = State $ \(x:xs) -> (x,xs)

push :: Int -> State Stack ()
push a = State $ \xs -> ((),a:xs)

tos :: State Stack Int
tos = State $ \(x:xs) -> (x,x:xs)

stackManip :: State Stack Int
stackManip = do
    push 10
    push 20
    a <- pop
    b <- pop
    push (a+b)
    tos

actionIO :: IO ()
actionIO = do
  let res = evalState stackManip emptyStack
  print res


data Post = Post { pTitle :: String, pBody :: String }

instance Show Post where
  show (Post title body) = "Title:" ++ title ++ "\n" ++ "Text: " ++ body

data Blog = Blog
  { bPosts   :: [Post]
  , bCounter :: Int
  }

mkBlog :: [Post] -> Blog
mkBlog posts = Blog posts (length posts)

type BlogS = State Blog

readPostS :: Int -> BlogS Post
readPostS i = do
  modify (\b -> b { bCounter = bCounter b + 1 })
  gets ((!! i) . bPosts)

newPostS :: Post -> BlogS ()
newPostS p = modify $ \b ->
  b { bPosts = p : bPosts b }

counterS :: BlogS Int
counterS = gets bCounter

read12AndNewS :: State Blog (Post, Post)
read12AndNewS =
  readPostS 1
    >>= \post1 ->
    newPostS (Post "Bla" "<text>") >>
      readPostS 2 >>= \post2 ->
      return (post1, post2)

evalRead12AndNewS
  :: Blog -> (Post, Post)
evalRead12AndNewS =
  evalState read12AndNewS

myPost :: Post
myPost = Post "Spam Title" "Whooo"

spamWithPosts :: Int -> State Blog ()
spamWithPosts n =
    replicateM_ n (newPostS myPost)

posts :: [Post]
posts = (\(x, y) -> Post x y) <$> [("title 1", "lorem ipsum"), ("title 2", "dolor sit amet"), ("title 3", "consectetur adipiscing elit")]

blog :: Blog
blog = Blog posts (length posts)

multiNewPost :: [Post] -> State Blog ()
multiNewPost = mapM_ newPostS
