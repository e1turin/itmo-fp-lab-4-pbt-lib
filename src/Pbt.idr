module Pbt

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
genList : Gen a -> Gen (List a)
genList gen = do
  len <- genNat
  sequence (replicate len gen)

||| Generate a pair of random values.
export
genPair : Gen a -> Gen b -> Gen (a, b)
genPair genA genB = do
  a <- genA
  b <- genB
  pure (a, b)


||| Shrink a list by removing elements.
export
shrinkList : List a -> List (List a)
shrinkList lst with (length lst)
  shrinkList lst | 0 = []
  shrinkList lst | (S k) = for [0..k] $ \i =>
    case (inBounds i lst) of
      (Yes _) => deleteAt i lst
      (No  _) => []

||| Shrink an integer by reducing its absolute value.
export
shrinkInt : Int -> List Int
shrinkInt n =
  if n == 0
    then []
    else [n `div` 2, -n `div` 2]

||| Shrink a pair by shrinking each component.
export
shrinkPair : (a -> List a) -> (b -> List b) -> (a, b) -> List (a, b)
shrinkPair shrinkA shrinkB (a, b) =
  [(a', b) | a' <- shrinkA a] ++ [(a, b') | b' <- shrinkB b]

||| A property is a function that takes an input and returns a Bool.
public export
Property : Type
Property = Bool

||| Test a property with random inputs, shrinking, and repetition.
|||
||| @gen The generator for random inputs.
||| @prop The property to test.
||| @numTests The number of tests to run.
||| @shrink A function to shrink failing inputs.
export
forAll : {auto _ : Show a}
      -> (gen : Gen a)
      -> (numTests : Nat)
      -> (shrink : a -> List a)
      -> (prop : a -> Property)
      -> IO ()
forAll gen numTests shrink prop = do
  putStrLn $ "Running \{show numTests} tests..."
  go numTests
  where
    go : Nat -> IO ()
    go 0 = putStrLn "All tests passed."
    go (S k) = do
      input <- gen
      let result := prop input
      if result
        then do
          -- putStrLn $ "Test passed with input: \{show input}"
          go k
        else do
          putStrLn $ "Test failed with input: \{show input}"
          putStrLn "Shrinking..."
          shrinkInput 100 input
          where
            shrinkInput : Nat -> a -> IO ()
            shrinkInput 0 input = do
              putStrLn "Reached maximum depth of shrinking"
            shrinkInput (S depth) input = do
              let candidates := shrink input
              case find (not . prop) candidates of
                Nothing => putStrLn $ "Minimal failing input: \{show input}"
                Just smaller => do
                  putStrLn $ "Shrunk to: \{show smaller}"
                  shrinkInput depth smaller

testReverse : IO ()
testReverse = do
  forAll (genList genInt) 100 shrinkList $ \lst =>
    lst == reverse (reverse lst)

testAppendLength : IO ()
testAppendLength = do
  forAll (genPair (genList genInt) (genList genInt)) 100 (shrinkPair shrinkList shrinkList) $
    \(lst1, lst2) =>
      length (lst1 ++ lst2) == length lst1 + length lst2

testAddCommutative : IO ()
testAddCommutative = do
  forAll (genPair genInt genInt) 100 (shrinkPair shrinkInt shrinkInt) $
    \(x, y) =>
      x + y == y + x

main : IO ()
main = do
  testReverse
  testAppendLength
  testAddCommutative

