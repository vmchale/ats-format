{-# LANGUAGE OverloadedStrings #-}

import qualified Filesystem.Path.CurrentOS as F
import           Language.ATS
import           Test.Hspec
import           Test.Hspec.Dirstream

isATS :: F.FilePath -> Bool
isATS x = (extension x `elem`) (pure <$> ["ats", "dats", "sats", "hats", "cats"])

main :: IO ()
main = hspec $
    describe "pretty-print" $ parallel $
        testFiles "test/data" isATS (fmap printATS . parseATS . lexATS)
