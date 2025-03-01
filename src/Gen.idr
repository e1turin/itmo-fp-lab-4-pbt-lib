module Gen

import System.Random
import Data.List

||| A generator for random values of type `a`.
public export
Gen : Type -> Type
Gen a = IO a

||| Generate a random Int32 within a range.
export
genInt32 : Gen Int32
genInt32 = randomRIO {a=Int32} (-1000, 1000)

||| Generate a random Int by casting from Int32.
export
genInt : Gen Int
genInt = map cast genInt32

||| Generate a random natural number (Nat) within a range.
export
genNat : Gen Nat
genNat = map cast (randomRIO {a=Int32} (0, 100))

||| Generate a random boolean.
export
genBool : Gen Bool
genBool = do
  n <- randomRIO {a=Int32} (0, 1)
  pure (n == 0)

||| Generate a random list of values using a generator.
export
genList : (maxSize : Nat) -> Gen a -> Gen (List a)
genList maxSize gen = do
  len <- randomRIO {a=Int32} (0, cast maxSize)
  sequence (replicate (cast len) gen)

||| Generate a pair of random values.
export
genPair : Gen a -> Gen b -> Gen (a, b)
genPair genA genB = do
  a <- genA
  b <- genB
  pure (a, b)

