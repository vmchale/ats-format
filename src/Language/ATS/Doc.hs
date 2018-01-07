{-# LANGUAGE OverloadedStrings #-}

module Language.ATS.Doc ( generateATSDocs
                        ) where

import           Language.ATS.Types
import           Text.Blaze.Html5

generateATSDocs :: ATS -> Html
generateATSDocs = toHtml . markupATS

markupATS :: ATS -> Markup
markupATS _ = html $
    body $ do
        p "First paragraph"
        p "Second paragraph"
