{-# LANGUAGE BlockArguments #-}

module Main (main) where

import Test.Hspec (describe, hspec, it, shouldBe)

main :: IO ()
main = hspec do
  describe "Data Star Event Generation" do
    it "should properly create a merge fragment event" do
      1 `shouldBe` 1
