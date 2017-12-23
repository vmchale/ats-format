-- | Main module for the library
module Language.ATS ( -- * Functions
                      lexATS
                    , parseATS
                    , printATS
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
                    , Arg (..)
                    , Addendum (..)
                    , LambdaType (..)
                    , Universal (..)
                    , Existential (..)
                    , PreFunction (..)
                    -- * Lexical types
                    , Token (..)
                    , AlexPosn (..)
                    , Keyword (..)
                    -- * Error types
                    , ATSError
                    -- * Executable
                    , exec
                    ) where

import           Language.ATS.Exec
import           Language.ATS.Lexer
import           Language.ATS.Parser
import           Language.ATS.PrettyPrint
import           Language.ATS.Types
