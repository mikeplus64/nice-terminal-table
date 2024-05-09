{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedLists #-}

module Main where

import Data.Char.BoxDrawing
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Test.Falsify.Generator qualified as Gen
import Test.Falsify.Predicate ((.$))
import Test.Falsify.Predicate qualified as Predicate
import Test.Tasty
import Test.Tasty.Falsify

main :: IO ()
main = defaultMain (testGroup "char-boxdrawing" tests)

modifiers :: NonEmpty Drawing
modifiers =
  [ heavy
  , heavyHoriz
  , heavyVert
  , dashed2
  , dashed3
  , double
  , doubleHoriz
  , doubleVert
  , rounded
  ]

directions :: NonEmpty Drawing
directions = NonEmpty.fromList do
  u <- [mempty, up]
  d <- [mempty, down]
  l <- [mempty, left]
  r <- [mempty, right]
  [u <> d <> l <> r]

tests :: [TestTree]
tests =
  [ testProperty "direction unaffected by style" do
      dir <- gen (Gen.elem directions)
      m1 <- gen (Gen.elem modifiers)
      m2 <- gen (Gen.elem modifiers)
      let d1 = dir <> m1
      let d2 = dir <> m2
      assert (Predicate.satisfies ("hasSameDirections", uncurry hasSameDirections) .$ ("(d1,d2)", (d1, d2)))
  , testProperty "style unaffected by direction" do
      m <- gen (Gen.elem modifiers)
      a <- gen (Gen.elem directions)
      b <- gen (Gen.elem directions)
      let d1 = a <> m
      let d2 = b <> m
      assert (Predicate.satisfies ("hasSameStyle", uncurry hasSameStyle) .$ ("(d1,d2)", (d1, d2)))
  ]
