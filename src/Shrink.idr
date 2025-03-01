module Shrink

import Data.List

public export
Shrinker : (a : Type) -> Type
Shrinker a = a -> List a

||| Shrink a list by removing elements.
export
shrinkList : Shrinker (List a)
shrinkList [] = []
shrinkList (x :: xs) = xs :: shrinkList xs

||| Shrink an integer by reducing its absolute value.
export
shrinkInt : Shrinker Int
shrinkInt n =
  if n == 0
    then []
    else [n `div` 2, -n `div` 2]

||| Shrink a pair by shrinking each component.
export
shrinkPair : Shrinker a -> Shrinker b -> Shrinker (a, b)
shrinkPair shrinkA shrinkB (a, b) =
  [(a', b) | a' <- shrinkA a] ++ [(a, b') | b' <- shrinkB b]

