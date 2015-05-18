module Main where

import Debug.Trace

-- main = trace "ohai lol!"

fib :: Number -> Number
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

-- ident :: Number -> Number
-- ident a = a

ident :: forall a. a -> a
ident a = a

-- map :: forall a b. (a -> b) -> [a] -> [b]
-- map _ [] = []
-- map f (head:tail) = (f head) : (map f tail)

inc :: Number -> Number
inc a = a + 1

class MyFunctor f where
  map :: forall a b. (a -> b) -> f a -> f b

instance functorArray :: MyFunctor [] where
  map _ [] = []
  map f (head:tail) = (f head) : (map f tail)

class MySemigroup a where
  append :: a -> a -> a

instance semigroupArray :: MySemigroup [a] where
  append [] b = b
  append (head:tail) b = head : append tail b

class (MySemigroup a) <= Monoid a where
  empty :: a

instance monoidArray :: Monoid [a] where
  empty = []
