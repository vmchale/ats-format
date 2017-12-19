-- | Main module for the library
module Language.ATS ( -- * Functions
                      lexATS
                    , parseATS
                    , printATS
                    -- * Types
                    , ATS (..)
                    , Declaration (..)
                    , Expression (..)
                    , Type (..)
                    -- * Executable
                    , exec
                    ) where

import           Language.ATS.Exec
import           Language.ATS.Lexer
import           Language.ATS.Parser
import           Language.ATS.PrettyPrint
import           Language.ATS.Types
