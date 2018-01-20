-- | Main module for the library
module Language.ATS ( -- * Functions for working with syntax
                      lexATS
                    , parseATS
                    , printATS
                    , printATSCustom
                    , printATSFast
                    -- * Library functions
                    , getDependencies
                    -- * Syntax Tree
                    , ATS (..)
                    , Declaration (..)
                    , Expression (..)
                    , Type (..)
                    , Function (..)
                    , Implementation (..)
                    , Pattern (..)
                    , Name (..)
                    , UnOp (..)
                    , BinOp (..)
                    , DataPropLeaf (..)
                    , Leaf (..)
                    , Arg (..)
                    , Addendum (..)
                    , LambdaType (..)
                    , Universal (..)
                    , Existential (..)
                    , PreFunction (..)
                    , StaticExpression (..)
                    , Paired (..)
                    , Fixity (..)
                    -- * Lexical types
                    , Token (..)
                    , AlexPosn (..)
                    , Keyword (..)
                    -- * Error types
                    , ATSError
                    -- * Lenses
                    , leaves
                    , constructorUniversals
                    -- * Library functions
                    -- * Executable
                    , exec
                    ) where

import           Data.Maybe               (catMaybes)
import           Language.ATS.Exec
import           Language.ATS.Lexer
import           Language.ATS.Parser
import           Language.ATS.PrettyPrint
import           Language.ATS.Types

getDependencies :: ATS -> [FilePath]
getDependencies (ATS ds) = catMaybes (g <$> ds)
    where g (Staload _ s) = Just s
          g (Include s)   = Just s
          g _             = Nothing
