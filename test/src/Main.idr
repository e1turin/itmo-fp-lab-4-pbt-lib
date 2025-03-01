module Main

import Pbt


testReverse : IO ()
testReverse = do
  forAll (genList 10 genInt)
     100 shrinkList $
    \lst =>
      lst == reverse (reverse lst)


testFailReverse : IO ()
testFailReverse = do
  forAll (genList 10 genInt) 100 shrinkList $
    \lst =>
      lst == reverse lst


testAppendLength : IO ()
testAppendLength = do
  forAll (genPair (genList 10 genInt) (genList 10 genInt))
     100 (shrinkPair shrinkList shrinkList) $
    \(lst1, lst2) => do
      let s1 = length (lst1 ++ lst2)
      let s2 = length lst1 + length lst2
      s1 == s2


testFailAppendLength : IO ()
testFailAppendLength = do
  forAll (genPair (genList 10 genInt) (genList 10 genInt))
     100 (shrinkPair shrinkList shrinkList) $
    \(lst1, lst2) => do
      let s1 = length (lst1 ++ lst2)
      let s2 = length lst1 + length lst2
      s1 == s2 + 1


testAddCommutative : IO ()
testAddCommutative = do
  forAll (genPair genInt genInt)
     100 (shrinkPair shrinkInt shrinkInt) $
    \(x, y) =>
      x + y == y + x


testFailAddCommutative : IO ()
testFailAddCommutative = do
  forAll (genPair genInt genInt)
    100 (shrinkPair shrinkInt shrinkInt) $
    \(x, y) =>
      x + y == x


main : IO ()
main = testAll
  [ MkGroup "Smoke tests"
    [ ("reverse list", testReverse)
    , ("append length", testAppendLength)
    , ("commutative addition", testAddCommutative)
    ]
  , MkGroup "Fire tests"
    [ ("fail commutative add", testFailAddCommutative)
    , ("fail reverse list", testFailReverse)
    , ("fail append length", testFailAppendLength)
    ]
  ]
