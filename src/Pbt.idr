module Pbt

import Data.List

import public Gen
import public Shrink


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
      -> (shrink : Shrinker a)
      -> (prop : a -> Property)
      -> IO ()
forAll gen numTests shrink prop = do
  putStr "  Running \{show numTests} tests... "
  go numTests
  where
    maxShrinkDepth : Nat
    maxShrinkDepth = 10

    go : Nat -> IO ()
    go 0 = putStrLn "All tests passed."
    go (S k) = do
      input <- gen
      let result := prop input
      if result
        then go k
        else do
          putStrLn "Test failed with input: \{show input}"
          putStrLn "! Shrinking..."
          shrinkInput maxShrinkDepth input
          where
            shrinkInput : Nat -> a -> IO ()
            shrinkInput 0 input = do
              putStrLn "! Reached maximum depth of \{show maxShrinkDepth} shrinks"
            shrinkInput (S depth) input = do
              let candidates := shrink input
              case find (not . prop) candidates of
                Nothing => putStrLn $ "Minimal failing input: \{show input}"
                Just smaller => do
                  putStrLn "  Shrunk to: \{show smaller}"
                  shrinkInput depth smaller


public export
record Group where
  constructor MkGroup
  name : String
  tests : List (String, IO ())


runTest : (String, IO ()) -> IO ()
runTest (name, prop) = do
  putStrLn "  > Check '\{name}':"
  prop


export
test : Group -> IO ()
test (MkGroup name tests) = do
  putStrLn "=== begin \{name} ==="
  runAll tests
  where
    runAll : List (String, IO ()) -> IO ()
    runAll [] = putStrLn "=== end \{name} ==="
    runAll (cur :: rem) = do
      runTest cur
      runAll rem


export
testAll : List (Group) -> IO ()
testAll [] = putStrLn "Tests finished"
testAll (cur :: rem) = do
  test cur
  testAll rem

