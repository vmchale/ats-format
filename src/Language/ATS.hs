-- | Main module for the library
module Language.ATS ( -- * Functions
                      lexATS
                    , parseATS
                    , printATS
                    , printATSCustom
                    , printATSFast
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
                    -- * Executable
                    , exec
                    ) where

import           Language.ATS.Exec
import           Language.ATS.Lexer
import           Language.ATS.Parser
import           Language.ATS.PrettyPrint
import           Language.ATS.Types
