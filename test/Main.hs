{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.ByteString (isInfixOf)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Foldable (find, traverse_)
import Data.Maybe (isJust)
import DataStar.Events (MergeMode (..), defaultMergeFragmentConfig, mergeFragmentConfigMergeMode, mergeFragments')
import DataStar.Events qualified as Morph
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = hspec do
  describe "Data Star Event Generation" do
    describe "Merge Fragments (default config)" do
      let fragments =
            [ "fragment1"
            , "fragment2"
            ]
      let evt =
            mergeFragments'
              defaultMergeFragmentConfig
              fragments
      let evtLines = BS.lines evt
      it "should have the correct number of lines (including empty line at end)" do
        length evtLines `shouldBe` 6
      it "should have the correct event descriptor" do
        take 1 evtLines `shouldBe` ["event: datastar-merge-fragments"]
      it "should have an empty line at the end" do
        drop 5 evtLines `shouldBe` [""]
      it "should have a fragments line with all fragments" do
        let mMatchedLine = find (\line -> "data: fragments " `BS.isPrefixOf` line) (drop 1 evtLines)

        case mMatchedLine of
          Nothing -> fail "Couldn't find a fragment line"
          Just matchedLine -> do
            let allFragmentsContained =
                  all
                    ( \frag ->
                        (BS.toStrict frag)
                          `isInfixOf` (BS.toStrict matchedLine)
                    )
                    fragments
            allFragmentsContained `shouldBe` True
      it "should have a merge morph line" do
        let mMatchedLine = find (\line -> line == "data: mergeMode morph") (drop 1 evtLines)
        mMatchedLine `shouldSatisfy` isJust
      it "should have a default of 300ms settle time" do
        let mMatchedLine = find (\line -> line == "data: settleDuration 300") (drop 1 evtLines)
        mMatchedLine `shouldSatisfy` isJust
      it "should have a default of false for use view transition" do
        let mMatchedLine = find (\line -> line == "data: useViewTransition false") (drop 1 evtLines)
        mMatchedLine `shouldSatisfy` isJust
    describe "Merge Fragments (custom config)" do
      it "should have the proper merge mode (testing all modes)" do
        let verifyMergeMode mergeMode = do
              let evt =
                    mergeFragments'
                      defaultMergeFragmentConfig{mergeFragmentConfigMergeMode = mergeMode}
                      ["fragment1"]
              let evtLines = BS.lines evt
              let mMatchedLine = find (\line -> "data: mergeMode " `BS.isPrefixOf` line) (drop 1 evtLines)
              case mMatchedLine of
                Nothing -> fail "Couldn't find a mergemode line"
                Just matchedLine -> do
                  matchedLine `shouldSatisfy` \l ->
                    ( BS.pack (show mergeMode) `BS.isSuffixOf` l
                    )
        traverse_ verifyMergeMode [Morph .. UpsertAttributes]