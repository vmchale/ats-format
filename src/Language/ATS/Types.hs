{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- | This is a module containing types to model the ATS syntax tree. As it is
-- collapsed by the pretty printer, you may see that in some places it is
-- focused on the lexical side of things.
module Language.ATS.Types
    ( ATS (..)
    , Declaration (..)
    , Type (..)
    , Name (..)
    , Pattern (..)
    , PatternF (..)
    , Arg (..)
    , Universal (..)
    , Function (..)
    , Expression (..)
    , ExpressionF (..)
    , Implementation (..)
    , BinOp (..)
    , UnOp (..)
    , TypeF (..)
    , Existential (..)
    , LambdaType (..)
    , Addendum (..)
    , DataPropLeaf (..)
    , PreFunction (..)
    ) where

import           Control.DeepSeq          (NFData)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           GHC.Generics             (Generic)
import           Language.ATS.Lexer       (Addendum (..), AlexPosn)

-- | Newtype wrapper containing a list of declarations
newtype ATS = ATS { unATS :: [Declaration] }
    deriving (Show, Eq, Generic, NFData)

-- | Declare something in a scope (a function, value, action, etc.)
data Declaration = Func AlexPosn Function
                 | Impl Implementation
                 | ProofImpl Implementation -- primplmnt
                 | Val Addendum (Maybe Type) Pattern Expression
                 | PrVal Pattern Expression
                 | Var (Maybe Type) Pattern Expression -- TODO can a `var` really appear anywhere in top-level declarations
                 | AndDecl (Maybe Type) Pattern Expression
                 | Include String
                 | Staload (Maybe String) String
                 | Stadef String Name
                 | CBlock String
                 | RecordType String [(String, Type)]
                 | RecordViewType String Type
                 | SumType String [(String, Maybe Type)]
                 | SumViewType String [(String, Maybe Type)]
                 | AbsViewType AlexPosn String [Arg] Type
                 | OverloadOp AlexPosn BinOp Name
                 | Comment String
                 | DataProp AlexPosn String [Arg] [DataPropLeaf]
                 | Extern AlexPosn Declaration
                 | Define String
                 deriving (Show, Eq, Generic, NFData)

data DataPropLeaf = DataPropLeaf [Universal] Expression
                  deriving (Show, Eq, Generic, NFData)

-- | A type for parsed ATS types
data Type = Bool
          | Void
          | String
          | Char
          | Int
          | Nat
          | DependentInt Expression
          | DependentBool Expression
          | DepString Expression
          | Double
          | Float
          | Tuple AlexPosn [Type]
          | Named String
          | Ex Existential Type
          | ForA Universal Type
          | Dependent Name [Type]
          | Unconsumed Type -- !a
          | AsProof Type (Maybe Type) -- a >> b
          | FromVT Type -- For a viewtype VT, we can prove there exist a view V and type T such that `VT` is equivalent to `(V | T)` - that T is `VT?!`
          | MaybeVal Type -- This is just `a?` or the like
          | T0p Addendum -- t@ype
          | Vt0p Addendum -- vt@ype
          | At AlexPosn Type Type
          | ProofType AlexPosn Type Type -- Aka (prf | val)
          | ConcreteType Expression
          | RefType Type
          | AbsProp AlexPosn String [Arg]
          deriving (Show, Eq, Generic, NFData)

-- | A type for the various lambda arrows (`=>`, `=<cloref1>`, etc.)
data LambdaType = Plain AlexPosn
                | Full AlexPosn String
                deriving (Show, Eq, Generic, NFData)

-- | A name can be qualified (`$UN.unsafefn`) or not
data Name = Unqualified String
          | Qualified AlexPosn String String
          deriving (Show, Eq, Generic, NFData)

-- | A data type for patterns.
data Pattern = Wildcard AlexPosn
             | PName String [Pattern]
             | PSum String Pattern
             | PLiteral Expression
             | Guarded AlexPosn Expression Pattern
             | Free Pattern
             | Proof AlexPosn Pattern Pattern
             | NullPattern AlexPosn
             | TuplePattern [Pattern]
             deriving (Show, Eq, Generic, NFData)

-- | An argument to a function.
data Arg = Arg String (Maybe Type)
    deriving (Show, Eq, Generic, NFData)

-- | Wrapper for universal quantifiers (refinement types)
data Universal = Universal { bound :: [Arg], typeU :: Maybe Type, prop :: Maybe Expression }
    deriving (Show, Eq, Generic, NFData)

-- | Wrapper for existential quantifiers/types
data Existential = Existential { boundE :: [Arg], typeE :: Maybe Type, propE :: Maybe Expression } -- TODO #[id:int] existentials
    deriving (Show, Eq, Generic, NFData)

-- | `~` is used to negate numbers in ATS
data UnOp = Negate
    deriving (Show, Eq, Generic, NFData)

-- | Binary operators on expressions
data BinOp = Add
           | Mult
           | Div
           | Sub
           | GreaterThan
           | GreaterThanEq
           | LessThan
           | LessThanEq
           | Equal
           | NotEqual
           | LogicalAnd
           | LogicalOr
           deriving (Show, Eq, Generic, NFData)

-- | A (possibly effectful) expression.
data Expression = Let AlexPosn ATS Expression
                | Begin ATS
                | Local ATS
                | VoidLiteral -- The '()' literal representing inaction.
                    AlexPosn
                | Call Name [Expression] [Type] [Expression]
                | NamedVal Name
                | If { cond     :: Expression -- ^ Expression evaluating to a boolean value
                     , whenTrue :: Expression -- ^ Expression to be returned when true
                     , elseExpr :: Expression -- ^ Expression to be returned when false
                     }
                | Sif { cond :: Expression, whenTrue :: Expression, elseExpr :: Expression }
                | BoolLit Bool
                | TimeLit String
                | FloatLit Float
                | IntLit Int
                | Lambda AlexPosn LambdaType Pattern Expression
                | LinearLambda AlexPosn LambdaType Pattern Expression
                | Index AlexPosn Name Expression
                | Access AlexPosn Expression Name
                | StringLit String
                | CharLit Char
                | Binary BinOp Expression Expression
                | Unary UnOp Expression
                | Case { posE :: AlexPosn
                       , kind :: Addendum
                       , val  :: Expression
                       , arms :: [(Pattern, Expression)] -- ^ Each `(Pattern, Expression)` pair corresponds to a branch of the 'case' statement
                       }
                | RecordValue AlexPosn [(String, Expression)] (Maybe Type)
                | Precede Expression Expression
                | FieldMutate { posE  :: AlexPosn
                              , old   :: Expression -- ^ Record to modify
                              , field :: String -- ^ Field being modified
                              , new   :: Expression -- ^ New value of the field
                              }
                | Deref AlexPosn Expression
                | Ref AlexPosn Type Expression
                | ProofExpr AlexPosn Expression Expression
                | TypeSignature Expression Type
                | WhereExp Expression Declaration
                | TupleEx AlexPosn [Expression]
                | While AlexPosn Expression Expression
                | Actions ATS
                deriving (Show, Eq, Generic, NFData)

-- | An 'implement' declaration
data Implementation = Implement { pos            :: AlexPosn
                                , preUniversalsI :: [Universal]
                                , universalsI    :: [Universal] -- ^ Universal quantifiers
                                , nameI          :: Name -- ^ Name of the template being implemented
                                , iArgs          :: [Arg] -- ^ Arguments
                                , iExpression    :: Expression -- ^ Expression holding the function body.
                                }
    deriving (Show, Eq, Generic, NFData)

-- | A function declaration accounting for all three keywords (???) ATS uses to
-- define them.
data Function = Fun PreFunction
              | Fnx PreFunction
              | And PreFunction
              | PrFun PreFunction
              | PrFn PreFunction
              | Praxi PreFunction
              deriving (Show, Eq, Generic, NFData)

data PreFunction = PreF { fname         :: String -- ^ Function name
                        , preUniversals :: [Universal] -- ^ Universal quantifiers making a function generic
                        , universals    :: [Universal] -- ^ Universal quantifiers/refinement type
                        , args          :: [Arg] -- ^ Actual function arguments
                        , returnType    :: Type -- ^ Return type
                        , termetric     :: Maybe Expression -- ^ Optional termination metric
                        , expression    :: Maybe Expression -- ^ Expression holding the actual function body (not present in static templates)
                        }
                        deriving (Show, Eq, Generic, NFData)

makeBaseFunctor ''Pattern
makeBaseFunctor ''Expression
makeBaseFunctor ''Type
