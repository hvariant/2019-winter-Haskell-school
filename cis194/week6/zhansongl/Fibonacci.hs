{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

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

x :: Stream Integer
x = Cons 0 . Cons 1 $ streamRepeat 0

instance Num (Stream Integer) where
  fromInteger n = Cons n . streamRepeat $ 0
  negate = streamMap negate -- this is not recursion!!!
  (+) (Cons a0 a') (Cons b0 b') = Cons (a0 + b0) (a' + b')
  (*) (Cons a0 a') b@(Cons b0 b') = Cons (a0 * b0) (fromIntegral a0 * b' + a' * b)

instance Fractional (Stream Integer) where
  (/) (Cons a0 a') (Cons b0 b') = q
    where q = Cons (a0 `div` b0) (fromIntegral (1 `div` b0) * (a' - q * b'))

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x * x)

