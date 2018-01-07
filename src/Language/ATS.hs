-- | Main module for the library
module Language.ATS ( -- * Functions
                      lexATS
                    , parseATS
                    , printATS
                    -- * Documentation generation
                    , generateATSDocs
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
                    -- * Lexical types
                    , Token (..)
                    , AlexPosn (..)
                    , Keyword (..)
                    -- * Error types
                    , ATSError
                    -- * Executable
                    , exec
                    ) where

import           Language.ATS.Doc
import           Language.ATS.Exec
import           Language.ATS.Lexer
import           Language.ATS.Parser
import           Language.ATS.PrettyPrint
import           Language.ATS.Types
