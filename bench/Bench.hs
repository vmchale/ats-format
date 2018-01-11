module Main where

import           Criterion.Main
import           Language.ATS

main :: IO ()
main =
    defaultMain [ env envFiles $ \ ~(l, m) ->
                  bgroup "format"
                      [ bench "lexATS (large)" $ nf lexATS l
                      , bench "parseATS . lexATS (large)" $ nf (parseATS . lexATS) l
                      , bench "printATS . parseATS . lexATS (large)" $ nf (fmap printATS . parseATS . lexATS) l
                      , bench "lexATS (medium)" $ nf lexATS m
                      , bench "parseATS . lexATS (medium)" $ nf (parseATS . lexATS) m
                      , bench "printATS . parseATS . lexATS (medium)" $ nf (fmap printATS . parseATS . lexATS) m
                      ]
                ]
    where large = readFile "test/data/polyglot.dats"
          medium = readFile "test/data/toml-parse.dats"
          envFiles = (,) <$> large <*> medium
