{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable     #-}
{-# LANGUAGE DeriveFunctor      #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveTraversable  #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE TypeFamilies       #-}

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
    , Paired (..)
    , Bifurcated (..)
    , StaticExpression (..)
    , StaticExpressionF (..)
    , rewriteATS
    ) where

import           Control.DeepSeq          (NFData)
import           Data.Functor.Foldable    (cata, embed)
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           GHC.Generics             (Generic)
import           Language.ATS.Lexer       (Addendum (..), AlexPosn)

data Bifurcated a = Nil
                  | Comma a (Bifurcated a)
                  | Bar a (Bifurcated a)

-- | Newtype wrapper containing a list of declarations
newtype ATS = ATS { unATS :: [Declaration] }
    deriving (Show, Eq, Generic, NFData)

-- | Declare something in a scope (a function, value, action, etc.)
data Declaration = Func AlexPosn Function
                 | Impl [Arg] Implementation
                 | ProofImpl Implementation -- primplmnt -- TODO add args
                 | Val Addendum (Maybe Type) Pattern Expression -- TODO expression should be optional?
                 | PrVal Pattern Expression
                 | Var (Maybe Type) Pattern (Maybe Expression) (Maybe Expression) -- TODO AlexPosn
                 | AndDecl (Maybe Type) Pattern Expression
                 | Include String
                 | Staload (Maybe String) String
                 | Stadef String Name [Type]
                 | CBlock String
                 | RecordType String [Arg] [Universal] [(String, Type)]
                 | RecordViewType String [Arg] [Universal] [(String, Type)]
                 | TypeDef AlexPosn String [Arg] Type
                 | ViewTypeDef AlexPosn String [Arg] Type
                 | SumType String [Arg] [(String, Maybe Type)] -- TODO [Arg] for here
                 | SumViewType String [Arg] [(String, Maybe Type)]
                 | AbsType AlexPosn String [Arg] Type
                 | AbsViewType AlexPosn String [Arg] Type
                 | OverloadOp AlexPosn BinOp Name
                 | Comment String
                 | DataProp AlexPosn String [Arg] [DataPropLeaf]
                 | Extern AlexPosn Declaration
                 | Define String
                 | SortDef AlexPosn String Type
                 | AndD Declaration Declaration
                 | Local AlexPosn [Declaration] [Declaration]
                 | AbsProp AlexPosn String [Arg]
                 | Assume Name [Arg] Expression
                 | TKind AlexPosn Name String
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
          | DependentInt StaticExpression
          | DependentBool StaticExpression
          | DepString StaticExpression
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
          | ViewType AlexPosn Type
          | FunctionType String Type Type
          | NoneType AlexPosn
          | ImplicitType AlexPosn
          deriving (Show, Eq, Generic, NFData)

-- | A type for the various lambda arrows (`=>`, `=<cloref1>`, etc.)
data LambdaType = Plain AlexPosn
                | Full AlexPosn String
                | Spear AlexPosn
                deriving (Show, Eq, Generic, NFData)

-- | A name can be qualified (`$UN.unsafefn`) or not
data Name = Unqualified String
          | Qualified AlexPosn String String
          | SpecialName AlexPosn String
          | Functorial String String
          | Unnamed AlexPosn
          deriving (Show, Eq, Generic, NFData)

-- | A data type for patterns.
data Pattern = Wildcard AlexPosn
             | PName String [Pattern]
             | PSum String Pattern
             | PLiteral Expression
             | Guarded AlexPosn Expression Pattern
             | Free Pattern
             | Proof AlexPosn [Pattern] [Pattern]
             | TuplePattern [Pattern]
             deriving (Show, Eq, Generic, NFData)

data Paired a b = Both a b
                | First a
                | Second b
                deriving (Show, Eq, Generic, NFData)

-- | An argument to a function.
data Arg = Arg (Paired String Type)
         | PrfArg Arg Arg
         | NoArgs
    deriving (Show, Eq, Generic, NFData)

-- | Wrapper for universal quantifiers (refinement types)
data Universal = Universal { bound :: [Arg], typeU :: Maybe Type, prop :: Maybe StaticExpression }
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
           | StaticEq
           | Mod
           deriving (Show, Eq, Generic, NFData)

data StaticExpression = StaticVal Name
                      | StaticBinary BinOp StaticExpression StaticExpression
                      | StaticInt Int
                      | StaticBool Bool
                      | Sif { scond :: StaticExpression, wwhenTrue :: StaticExpression, selseExpr :: StaticExpression } -- Static if (for proofs)
                      | SCall Name [StaticExpression]
                      deriving (Show, Eq, Generic, NFData)

-- | A (possibly effectful) expression.
data Expression = Let AlexPosn ATS (Maybe Expression)
                | VoidLiteral -- The '()' literal representing inaction.
                    AlexPosn
                -- function call: <a>, then {n}
                | Call Name [Type] [Type] (Maybe Expression) [Expression]
                | NamedVal Name
                | If { cond     :: Expression -- ^ Expression evaluating to a boolean value
                     , whenTrue :: Expression -- ^ Expression to be returned when true
                     , elseExpr :: Maybe Expression -- ^ Expression to be returned when false
                     }
                | BoolLit Bool
                | TimeLit String
                | FloatLit Float
                | IntLit Int
                | UnderscoreLit AlexPosn
                | Lambda AlexPosn LambdaType Pattern Expression
                | LinearLambda AlexPosn LambdaType Pattern Expression
                | Index AlexPosn Name Expression
                | Access AlexPosn Expression Name
                | StringLit String
                | CharLit Char
                | AtExpr Expression Expression
                | AddrAt AlexPosn Expression
                | ViewAt AlexPosn Expression
                | Binary BinOp Expression Expression
                | Unary UnOp Expression
                | Case { posE :: AlexPosn
                       , kind :: Addendum
                       , val  :: Expression
                       , arms :: [(Pattern, LambdaType, Expression)] -- ^ Each `(Pattern, Expression)` pair corresponds to a branch of the 'case' statement
                       }
                | RecordValue AlexPosn [(String, Expression)] (Maybe Type)
                | Precede Expression Expression
                | FieldMutate { posE  :: AlexPosn
                              , old   :: Expression -- ^ Record to modify
                              , field :: String -- ^ Field being modified
                              , new   :: Expression -- ^ New value of the field
                              }
                | Mutate Expression Expression
                | Deref AlexPosn Expression
                | ProofExpr AlexPosn Expression Expression
                | TypeSignature Expression Type
                | WhereExp Expression [Declaration]
                | TupleEx AlexPosn [Expression]
                | While AlexPosn Expression Expression
                | Actions ATS
                | Begin AlexPosn Expression
                | BinList { _op :: BinOp, _exprs :: [Expression] }
                | PrecedeList { _exprs :: [Expression] }
                | FixAt PreFunction
                | LambdaAt PreFunction
                | ParenExpr AlexPosn Expression
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
              | Fn PreFunction
              | Fnx PreFunction
              | And PreFunction
              | PrFun PreFunction
              | PrFn PreFunction
              | Praxi PreFunction
              deriving (Show, Eq, Generic, NFData)

data PreFunction = PreF { fname         :: Name -- ^ Function name
                        , sig           :: String -- ^ e.g. <> or <!wrt>
                        , preUniversals :: [Universal] -- ^ Universal quantifiers making a function generic
                        , universals    :: [Universal] -- ^ Universal quantifiers/refinement type
                        , args          :: [Arg] -- ^ Actual function arguments
                        , returnType    :: Type -- ^ Return type
                        , termetric     :: Maybe StaticExpression -- ^ Optional termination metric
                        , expression    :: Maybe Expression -- ^ Expression holding the actual function body (not present in static templates)
                        }
                        deriving (Show, Eq, Generic, NFData)

makeBaseFunctor ''Pattern
makeBaseFunctor ''Expression
makeBaseFunctor ''StaticExpression
makeBaseFunctor ''Type

rewriteATS :: Expression -> Expression
rewriteATS = cata a where
    a (PrecedeF e e'@PrecedeList{}) = PrecedeList (e : _exprs e')
    a (PrecedeF e e')               = PrecedeList [e, e']
    a x                             = embed x
    {- a (BinaryF Add e e'@BinList{})  = BinList Add (e : _exprs e') -}
    {- a (BinaryF Add e e')            = BinList Add [e, e'] -}
