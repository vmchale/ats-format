{-# LANGUAGE OverloadedStrings #-}

import           Language.ATS.Lexer       (lexATS)
import           Language.ATS.Parser      (parseATS)
import           Language.ATS.PrettyPrint (printATS)
import           Test.Hspec

main :: IO ()
main = hspec $
    describe "pretty" $ do
        parallel $ it "works on fib.dats" $ do
            sample <- readFile "test/data/fib.dats"
            expected <- readFile "test/data/fib.out"
            (fmap printATS . parseATS . lexATS) sample `shouldBe` Right expected
        parallel $ it "works on filecount.dats" $ do
            sample <- readFile "test/data/filecount.dats"
            expected <- readFile "test/data/filecount.out"
            (fmap ((++ "\n") . printATS) . parseATS . lexATS) sample `shouldBe` Right expected
        parallel $ it "works on filetype.sats" $ do
            sample <- readFile "test/data/filetype.sats"
            expected <- readFile "test/data/filetype.out"
            (fmap ((++ "\n") . printATS) . parseATS . lexATS) sample `shouldBe` Right expected
        parallel $ it "works on polyglot.dats" $ do
            sample <- readFile "test/data/polyglot.dats"
            expected <- readFile "test/data/polyglot.out"
            (fmap ((++ "\n") . printATS) . parseATS . lexATS) sample `shouldBe` Right expected
        parallel $ it "works on left-pad.dats" $ do
            sample <- readFile "test/data/left-pad.dats"
            expected <- readFile "test/data/left-pad.out"
            (fmap ((++ "\n") . printATS) . parseATS . lexATS) sample `shouldBe` Right expected
