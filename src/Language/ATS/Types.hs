{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase        #-}
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
    , rewriteATS
    , binList
    ) where

import           Control.DeepSeq          (NFData)
import           Control.Lens.Plated
import           Data.Functor.Foldable.TH (makeBaseFunctor)
import           GHC.Generics             (Generic)
import           Language.ATS.Lexer       (Addendum (..), AlexPosn)

-- | Newtype wrapper containing a list of declarations
newtype ATS = ATS { unATS :: [Declaration] }
    deriving (Show, Eq, Generic, NFData)

-- | Declare something in a scope (a function, value, action, etc.)
data Declaration = Func AlexPosn Function
                 | Impl [Arg] Implementation
                 | ProofImpl Implementation -- primplmnt -- TODO add args
                 | Val Addendum (Maybe Type) Pattern Expression -- TODO expression should be optional?
                 | PrVal Pattern Expression
                 | Var (Maybe Type) Pattern (Maybe Expression) -- TODO AlexPosn
                 | AndDecl (Maybe Type) Pattern Expression
                 | Include String
                 | Staload (Maybe String) String
                 | Stadef String Name
                 | CBlock String
                 | RecordType String [Arg] [(String, Type)]
                 | RecordViewType String [Arg] [(String, Type)]
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
          | ViewType AlexPosn Type
          | FunctionType String Type Type
          | NoneType AlexPosn
          deriving (Show, Eq, Generic, NFData)

-- | A type for the various lambda arrows (`=>`, `=<cloref1>`, etc.)
data LambdaType = Plain AlexPosn
                | Full AlexPosn String
                deriving (Show, Eq, Generic, NFData)

-- | A name can be qualified (`$UN.unsafefn`) or not
data Name = Unqualified String
          | Qualified AlexPosn String String
          | SpecialName AlexPosn String
          | Functorial String String
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
         | PrfArg Arg [Arg]
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
data Expression = Let AlexPosn ATS (Maybe Expression)
                | VoidLiteral -- The '()' literal representing inaction.
                    AlexPosn
                -- The first list is implicit arguments such as `<a>`, the second is implicits such as `{list_vt(string)}`, the third is an optional proof, and the last is the actual arguments
                | Call Name [Expression] [Type] (Maybe Expression) [Expression]
                | NamedVal Name
                | If { cond     :: Expression -- ^ Expression evaluating to a boolean value
                     , whenTrue :: Expression -- ^ Expression to be returned when true
                     , elseExpr :: Maybe Expression -- ^ Expression to be returned when false
                     }
                | Sif { cond :: Expression, whenTrue :: Expression, selseExpr :: Expression } -- Static if (for proofs)
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
                | Mutate Expression Expression
                | Deref AlexPosn Expression
                | Ref AlexPosn Type Expression
                | ProofExpr AlexPosn Expression Expression
                | TypeSignature Expression Type
                | WhereExp Expression [Declaration]
                | TupleEx AlexPosn [Expression]
                | While AlexPosn Expression Expression
                | Actions ATS
                | TKind AlexPosn Name String
                | ViewExpr AlexPosn Type -- TODO separate type for proof values?
                | Begin AlexPosn Expression
                | BinList { _op :: BinOp, _exprs :: [Expression] }
                | PrecedeList { _exprs :: [Expression] }
                deriving (Show, Eq, Generic, NFData)

instance Plated Expression where
    plate f (Binary op e e') = Binary op <$> f e <*> f e'
    plate f (Precede e e')   = Precede <$> f e <*> f e'
    plate _ x                = pure x

rewriteATS :: Expression -> Expression
rewriteATS = precedeList

precedeList :: Expression -> Expression
precedeList = transform $ \case
    (Precede e@PrecedeList{} e') -> PrecedeList (e' : _exprs e)
    (Precede e e') -> PrecedeList [e, e']
    a -> a

binList :: Expression -> Expression
binList = transform $ \case
    (Binary Add e e'@Binary{})  -> BinList Add [e, e'] -- FIXME both have to be 'Add'
    (Binary Add e@BinList{} e') -> BinList Add (e' : _exprs e)
    a                          -> a

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

data PreFunction = PreF { fname         :: Name -- ^ Function name
                        , sig           :: String -- ^ e.g. <> or <!wrt>
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
