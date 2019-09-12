module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = next 0 1
  where next x y = x : next y (x+y)

data Stream a = Cons a (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons h t) = h : streamToList t

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat x = Cons x . streamRepeat $ x

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t) = Cons (f h) (streamMap f t)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f s = Cons s (streamFromSeed f (f s))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

ruler :: Stream Integer
ruler = streamMap (maxPower 0) . streamMap (+1) $ nats
  where maxPower r n
          | n `rem` (2^(r+1)) /= 0 = r
          | otherwise = maxPower (r+1) n

