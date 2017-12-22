{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}

import           Data.DirStream
import qualified Filesystem.Path.CurrentOS as F
import           Language.ATS
import           Pipes
import qualified Pipes.Prelude             as P
import           Pipes.Safe
import           System.FilePath
import           Test.Hspec
import           Test.Hspec.Core.Spec

deriving instance MonadCatch (SpecM a)
deriving instance MonadThrow (SpecM a)
deriving instance MonadMask (SpecM a)
deriving instance MonadIO (SpecM a)

isATS :: F.FilePath -> Bool
isATS x = (F.extension x `elem`) (pure <$> ["ats", "dats", "sats", "hats", "cats"])

testFile :: String -> SpecWith ()
testFile f = it f $ do
    sample <- readFile f
    expected <- readFile (replaceExtension f ".out")
    (fmap ((++ "\n") . printATS) . parseATS . lexATS) sample `shouldBe` Right expected

main :: IO ()
main = hspec $
    describe "pretty-print" $ parallel $
    runSafeT $ runEffect $ atsPath >-> mapS testFile

mapS :: (a -> SpecM () ()) -> Proxy () a y' y (SafeT (SpecM ())) r
mapS = P.mapM_ . (lift .)

atsPath :: MonadSafe m => Producer String m ()
atsPath = every (childOf p) >-> P.filter isATS >-> P.map F.encodeString
    where p = F.decodeString "test/data"
