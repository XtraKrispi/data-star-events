{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Data.Aeson (encode)
import Data.ByteString (isInfixOf)
import Data.ByteString.Lazy.Char8 qualified as BS
import Data.Foldable (find, traverse_)
import Data.List (intersperse)
import Data.Maybe (isJust)
import DataStar.Events (MergeFragmentConfig (mergeFragmentConfigSelector, mergeFragmentConfigSettleDurationInMs, mergeFragmentConfigUseViewTransition), MergeMode (..), defaultMergeFragmentConfig, executeScripts, mergeFragmentConfigMergeMode, mergeFragments', mergeSignals, redirectScript, removeFragments, removeSignals)
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)

main :: IO ()
main = hspec do
  describe "Data Star Event Generation" do
    describe "Fragments" do
      describe "Merge Fragments" do
        describe "Default Config" do
          let fragments =
                [ "fragment1"
                , "fragment2"
                ]
          let evt =
                mergeFragments'
                  defaultMergeFragmentConfig
                  fragments
          let evtLines = BS.lines evt
          it "should have the correct event descriptor" do
            take 1 evtLines `shouldBe` ["event: datastar-merge-fragments"]
          it "should have the correct number of lines (including empty line at end)" do
            length evtLines `shouldBe` 6
          it "should have an empty line at the end" do
            drop 5 evtLines `shouldBe` [""]
          it "should have a fragments line with all fragments" do
            let mMatchedLine = find (\line -> "data: fragments " `BS.isPrefixOf` line) evtLines

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
            let mMatchedLine = find (\line -> line == "data: mergeMode morph") evtLines
            mMatchedLine `shouldSatisfy` isJust
          it "should have a default of 300ms settle time" do
            let mMatchedLine = find (\line -> line == "data: settleDuration 300") evtLines
            mMatchedLine `shouldSatisfy` isJust
          it "should have a default of false for use view transition" do
            let mMatchedLine = find (\line -> line == "data: useViewTransition false") evtLines
            mMatchedLine `shouldSatisfy` isJust
        describe "Custom Config" do
          it "should have the proper merge mode (testing all modes)" do
            let verifyMergeMode mergeMode = do
                  let evt =
                        mergeFragments'
                          defaultMergeFragmentConfig{mergeFragmentConfigMergeMode = mergeMode}
                          ["fragment1"]
                  let evtLines = BS.lines evt
                  let mMatchedLine = find (\line -> "data: mergeMode " `BS.isPrefixOf` line) evtLines
                  case mMatchedLine of
                    Nothing -> fail "Couldn't find a mergemode line"
                    Just matchedLine -> do
                      matchedLine `shouldSatisfy` \l ->
                        ( BS.pack (show mergeMode) `BS.isSuffixOf` l
                        )
            traverse_ verifyMergeMode [Morph .. UpsertAttributes]
          it "should correctly add a selector if specified" do
            let id_ = "#id"
            let evt =
                  mergeFragments'
                    defaultMergeFragmentConfig{mergeFragmentConfigSelector = Just id_}
                    ["fragment1"]
            let evtLines = BS.lines evt
            let mMatchedLine = find (\line -> line == ("data: selector " <> id_)) evtLines
            mMatchedLine `shouldSatisfy` isJust
          it "should correctly set the settle duration" do
            let evt =
                  mergeFragments'
                    defaultMergeFragmentConfig{mergeFragmentConfigSettleDurationInMs = 1000}
                    ["fragment1"]
            let evtLines = BS.lines evt
            let mMatchedLine = find (\line -> line == ("data: settleDuration 1000")) evtLines
            mMatchedLine `shouldSatisfy` isJust
          it "should correctly set the view transition" do
            let evt =
                  mergeFragments'
                    defaultMergeFragmentConfig{mergeFragmentConfigUseViewTransition = True}
                    ["fragment1"]
            let evtLines = BS.lines evt
            let mMatchedLine = find (\line -> line == "data: useViewTransition true") evtLines
            mMatchedLine `shouldSatisfy` isJust
      describe "Remove Fragments" do
        let selector = "#id"
        let evt = removeFragments selector
        let evtLines = BS.lines evt
        it "should have the correct event descriptor" do
          take 1 evtLines `shouldBe` ["event: datastar-remove-fragments"]
        it "should have the correct number of lines (including empty line at end)" do
          length evtLines `shouldBe` 3
        it "should have the correct selector" do
          let matchedLine = find (\line -> line == ("data: selector " <> selector)) evtLines
          matchedLine `shouldSatisfy` isJust
    describe "Signals" do
      describe "Merge Signals" do
        let obj :: (Int, Int, Int) = (1, 2, 3)
        let evt = mergeSignals True obj
        let evtLines = BS.lines evt
        it "should have the correct event descriptor" do
          take 1 evtLines `shouldBe` ["event: datastar-merge-signals"]
        it "should have the proper amount of lines" do
          length evtLines `shouldBe` 4
        it "should contain the serialized object" do
          let matchedLine = find (\line -> line == ("data: signals " <> encode obj)) evtLines
          matchedLine `shouldSatisfy` isJust
        it "should contain the onlyIfMissing flag" do
          let evt' = mergeSignals False obj
          let evtLines' = BS.lines evt'

          let matchedLine = find (\line -> line == "data: onlyIfMissing true") evtLines
          matchedLine `shouldSatisfy` isJust

          let matchedLine' = find (\line -> line == "data: onlyIfMissing false") evtLines'
          matchedLine' `shouldSatisfy` isJust
      describe "Remove Signals" do
        let paths = ["path1", "path2"]
        let evt = removeSignals paths
        let evtLines = BS.lines evt
        it "should have the correct event descriptor" do
          take 1 evtLines `shouldBe` ["event: datastar-remove-signals"]
        it "should have the proper amount of lines" do
          length evtLines `shouldBe` 3
        it "should contain the paths in the final event" do
          let matchedLine = find (\line -> line == ("data: paths " <> mconcat (intersperse " " paths))) evtLines
          matchedLine `shouldSatisfy` isJust
    describe "Scripts" do
      describe "executeScripts" do
        let script1 = "console.log('Hello')"
        let script2 = "alert(\"This is a test\")"
        let evt = executeScripts True [] [script1, script2]
        let evtLines = BS.lines evt
        it "should have the correct event descriptor" do
          take 1 evtLines `shouldBe` ["event: datastar-execute-script"]
        it "should have the correct number of lines (including empty line at end)" do
          length evtLines `shouldBe` 5
        it "should add attributes" do
          let evt' = executeScripts True [("defer", "true"), ("type", "module")] [script1, script2]
          let evtLines' = BS.lines evt'
          let matchedLineDefer = find (\line -> line == "data: attributes defer true") evtLines'
          let matchedLineType = find (\line -> line == "data: attributes type module") evtLines'
          matchedLineDefer `shouldSatisfy` isJust
          matchedLineType `shouldSatisfy` isJust
        it "should have a line per script" do
          let matchedLineScript1 = find (\line -> line == ("data: script " <> script1)) evtLines
          let matchedLineScript2 = find (\line -> line == ("data: script " <> script2)) evtLines
          matchedLineScript1 `shouldSatisfy` isJust
          matchedLineScript2 `shouldSatisfy` isJust
      describe "redirectScript" do
        it "should have the correct script" do
          let url = "http://www.google.com"
          let evt = redirectScript url
          evt `shouldBe` "window.location = \"" <> url <> "\""
