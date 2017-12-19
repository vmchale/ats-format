{
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DeriveGeneric     #-}
    {-# LANGUAGE DeriveAnyClass    #-}

    -- | This module contains the parser.
    module Language.ATS.Parser ( parseATS
                               , ATSError (..)
                               ) where

import Language.ATS.Types

import Language.ATS.Lexer ( Token (..)
                          , AlexPosn (..)
                          , Keyword (..)
                          , Addendum (..)
                          , token_posn
                          )

import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

import Prelude

import Control.DeepSeq (NFData)

import GHC.Generics (Generic)

}

%name parseATS
%tokentype { Token }
%error { parseError }
%monad { Either (ATSError String) } { (>>=) } { pure }

%token
    fun { Keyword $$ KwFun }
    fnx { Keyword $$ KwFnx }
    and { Keyword $$ KwAnd }
    lambda { Keyword $$ KwLambda }
    llambda { Keyword $$ KwLinearLambda }
    if { Keyword $$ KwIf }
    stadef { Keyword $$ KwStadef }
    val { Keyword _ (KwVal $$) }
    prval { Keyword $$ KwPrval }
    var { Keyword $$ KwVar }
    then { Keyword $$ KwThen }
    let { Keyword $$ KwLet }
    typedef { Keyword $$ KwTypedef }
    vtypedef { Keyword $$ KwVtypedef }
    absvtype { Keyword $$ KwAbsvtype }
    in { Keyword $$ KwIn }
    end { Keyword $$ KwEnd }
    stringType { Keyword $$ KwString }
    charType { Keyword $$ KwChar }
    voidType { Keyword $$ KwVoid }
    implement { Keyword $$ KwImplement }
    primplmnt { Keyword $$ KwProofImplement }
    else { Keyword $$ KwElse }
    bool { Keyword $$ KwBool }
    int { Keyword $$ KwInt }
    nat { Keyword $$ KwNat }
    when { Keyword $$ KwWhen }
    begin { Keyword $$ KwBegin }
    case { Keyword _ (KwCase $$) }
    datatype { Keyword $$ KwDatatype }
    datavtype { Keyword $$ KwDatavtype }
    while { Keyword $$ KwWhile }
    of { Keyword $$ KwOf }
    include { Keyword $$ KwInclude }
    staload { Keyword $$ KwStaload }
    overload { Keyword $$ KwOverload }
    with { Keyword $$ KwWith }
    dataprop { Keyword $$ KwDataprop }
    praxi { Keyword $$ KwPraxi }
    extern { Keyword $$ KwExtern }
    t0pPlain { Keyword $$ (KwT0p None) }
    vt0pCo { Keyword $$ (KwVt0p Plus) }
    where { Keyword $$ KwWhere }
    absprop { Keyword $$ KwAbsprop }
    boolLit { BoolTok _ $$ }
    timeLit { TimeTok _ $$ }
    intLit { IntTok _ $$ }
    floatLit { FloatTok _ $$ }
    identifier { Identifier _ $$ }
    closeParen { Special $$ ")" }
    openParen { Special $$ "(" }
    colon { Special $$ ":" }
    comma { Special $$ "," }
    geq { Operator $$ ">=" }
    leq { Operator $$ "<=" }
    neq { Operator $$ "!=" }
    openTermetric { Operator $$ ".<" }
    closeTermetric { Operator $$ ">." }
    mutateArrow { Operator $$ "->" }
    mutateEq { Operator $$ ":=" }
    lbracket { Operator $$ "<" }
    rbracket { Operator $$ ">" }
    eq { Operator $$ "=" }
    or { Operator $$ "||" }
    vbar { Special $$ "|" }
    lbrace { Special $$ "{" }
    rbrace { Special $$ "}" }
    plainArrow { Arrow $$ "=>" }
    cloref1Arrow { Arrow $$ "=<cloref1>" }
    cloptr1Arrow { Arrow $$ "=<cloptr1>" }
    lsqbracket { Special $$ "[" }
    rsqbracket { Special $$ "]" }
    string { StringTok _ $$ }
    charLit { CharTok _ $$ }
    underscore { Special $$ "_" }
    minus { Operator $$ "-" }
    plus { Operator $$ "+" }
    div { Operator $$ "/" }
    mult { Operator $$ "*" }
    exclamation { Special $$ "!" }
    dot { Special $$ "." }
    at { Special $$ "@" }
    tilde { Operator $$ "~" }
    dollar { Special $$ "$" }
    semicolon { Special $$ ";" }
    andOp { Operator $$ "&&" }
    doubleParens { Operator $$ "()" }
    prfTransform { Operator $$ ">>" } -- For types like &a >> a?!
    refType { Special $$ "&" } -- For types like &a
    maybeProof { Operator $$ "?" } -- For types like a?
    fromVT { Operator $$ "?!" } -- For types like a?!
    openExistential { Operator $$ "#[" } -- Same as `[` in ATS2
    cblock { CBlockLex _ $$ }
    ref { RefTok $$ }
    define { DefineBlock _ $$ }

%%

ATS : { ATS [] }
    | ATS Declaration { ATS ($2 : (\(ATS x) -> x) $1) }

TypeIn : Type { [$1] }
       | TypeIn comma Type { $3 : $1 }

TypeInExpr : TypeIn { $1 }
           | Expression { [ConcreteType $1] }
           | TypeInExpr comma PreExpression { ConcreteType $3 : $1 }

Type : Name openParen TypeInExpr closeParen { Dependent $1 $3 }
     | bool { Bool }
     | int { Int }
     | nat { Nat }
     | stringType { String }
     | charType { Char }
     | voidType { Void }
     | t0pPlain { T0p None }
     | vt0pCo { Vt0p Plus }
     | stringType openParen Expression closeParen { DepString $3 }
     | stringType Expression { DepString $2 }
     | int openParen Expression closeParen { DependentInt $3 }
     | bool openParen Expression closeParen { DependentBool $3 }
     | identifier { Named $1 }
     | int Expression { DependentInt $2 }
     | exclamation Type { Unconsumed $2 }
     | refType Type { RefType $2 }
     | Type maybeProof { MaybeVal $1 } 
     | Type fromVT { FromVT $1 }
     | Type prfTransform Type { AsProof $1 (Just $3) }
     | Type prfTransform underscore { AsProof $1 Nothing }
     | Existential Type { Ex $1 $2 }
     | Universal Type { ForA $1 $2 }
     | Type at Type { At $2 $1 $3 }
     | openParen Type vbar Type closeParen { ProofType $1 $2 $4 }
     | Name identifier { Dependent $1 [Named $2] }
     | openParen TypeIn closeParen { Tuple $1 $2 }
     | absprop identifier openParen Args closeParen { AbsProp $1 $2 [] }
     | openParen Type closeParen { $2 }

Args : { [] }
     | identifier { [Arg $1 Nothing] }
     | identifier colon Type { [Arg $1 (Just $3)] }
     | Type { [Arg "" (Just $1)] }
     | Args comma Type { Arg "" (Just $3) : $1 }
     | Args comma identifier colon Type { Arg $3 (Just $5) : $1 }
     | Args comma identifier { Arg $3 Nothing : $1 }
     | Type vbar Args { $3 } -- FIXME very very bad.

Literal : boolLit { BoolLit $1 }
        | timeLit { TimeLit $1 }
        | intLit { IntLit $1 }
        | floatLit { FloatLit $1 }
        | string { StringLit (init . drop 1 $ $1) }
        | charLit { CharLit $1 }

PatternIn : Pattern { [$1] }
          | PatternIn comma Pattern { $3 : $1 }

Pattern : doubleParens { NullPattern $1 }
        | identifier { PName $1 [] }
        | underscore { Wildcard $1 }
        | identifier doubleParens { PName ($1 ++ "()") [] }
        | tilde Pattern { Free $2 }
        | identifier openParen PatternIn closeParen { PName $1 $3 }
        | identifier Pattern { PSum $1 $2 }
        | openParen Pattern vbar Pattern closeParen { Proof $1 $2 $4 }
        | openParen PatternIn closeParen { TuplePattern $2 }
        | Literal { PLiteral $1 }
        | Pattern when Expression { Guarded $2 $3 $1 }

Case : { [] }
     | Case vbar Pattern LambdaArrow Expression { ($3, $5) : $1 }

ExpressionIn : Expression { [$1] }
             | ExpressionIn comma Expression { $3 : $1 }

TupleExpression : PreExpression comma PreExpression { [$3, $1] }
                | TupleExpression comma PreExpression { $3 : $1 }

LambdaArrow : plainArrow { Plain $1 }
            | cloref1Arrow { Full $1 "cloref1" }
            | cloptr1Arrow { Full $1 "cloptr1" }

Expression : PreExpression { $1 }
           | Name PreExpression { Call $1 [] [] [$2] }
           | openParen TupleExpression closeParen { TupleEx $1 $2 }

PreExpression : identifier lsqbracket PreExpression rsqbracket { Index $2 (Unqualified $1) $3 }
              | Literal { $1 }
              | doubleParens { VoidLiteral $1 }
              | Name doubleParens { Call $1 [] [] [] }
              | Name openParen ExpressionIn closeParen { Call $1 [] [] $3 }
              | Name lbrace ExpressionIn rbrace { Call $1 $3 [] [] }
              | openParen PreExpression vbar PreExpression closeParen { ProofExpr $1 $2 $4 }
              | case PreExpression of Case { Case $3 $1 $2 $4 }
              | PreExpression BinOp PreExpression { Binary $2 $1 $3 }
              | UnOp PreExpression { Unary $1 $2 }
              | PreExpression dot Name { Access $2 $1 $3 }
              | openParen PreExpression closeParen { $2 }
              | if PreExpression then Expression else Expression { If $2 $4 $6 }
              | let ATS in Expression end { Let $1 $2 $4 }
              | lambda Pattern LambdaArrow PreExpression { Lambda $1 $3 $2 $4 }
              | llambda Pattern LambdaArrow PreExpression { LinearLambda $1 $3 $2 $4 }
              | begin PreExpression end { $2 }
              | at lbrace RecordVal rbrace { RecordValue $1 $3 Nothing }
              | at lbrace RecordVal rbrace colon Type { RecordValue $1 $3 (Just $6) }
              | PreExpression semicolon PreExpression { Precede $1 $3 }
              | PreExpression semicolon { $1 }
              | exclamation PreExpression { Deref $1 $2 }
              | PreExpression mutateArrow identifier mutateEq PreExpression { FieldMutate $2 $1 $3 $5 }
              | ref Type rbracket PreExpression { Ref $1 $2 $4 }
              | PreExpression where lbrace Declaration rbrace { WhereExp $1 $4 }
              | PreExpression colon Type { TypeSignature $1 $3 }
              | Name { NamedVal $1 }
              | lbrace ATS rbrace { Actions $2 }
              | while openParen PreExpression closeParen PreExpression { While $1 $3 $5 }
              | include {% Left $ Expected $1 "Expression" "include" }
              | staload {% Left $ Expected $1 "Expression" "staload" }
              | overload {% Left $ Expected $1 "Expression" "overload" }
              | prval {% Left $ Expected $1 "Expression" "prval" }
              | var {% Left $ Expected $1 "Expression" "var" }
              | Termetric {% Left $ Expected (fst $1) "Expression" "termetric" }

Termetric : openTermetric Expression closeTermetric { ($1, $2) }

Existential : lsqbracket Args vbar Expression rsqbracket { Existential $2 Nothing (Just $4) }
            | lsqbracket Args rsqbracket { Existential $2 Nothing Nothing }
            | openExistential Args rsqbracket { Existential $2 Nothing Nothing }
            | openExistential Args vbar Expression rsqbracket { Existential $2 Nothing (Just $4) }
            | lsqbracket Args colon Type rsqbracket { Existential $2 (Just $4) Nothing }
            | lsqbracket Expression rsqbracket { Existential [] Nothing (Just $2) }
            

Universal : lbrace Args vbar PreExpression rbrace { Universal $2 Nothing (Just $4) }
          | lbrace Args rbrace { Universal $2 Nothing Nothing }
          | lbrace Args colon Type vbar Expression rbrace { Universal $2 (Just $4) (Just $6) }
          | lbrace Args colon Type { Universal $2 (Just $4) Nothing }

Implementation : identifier doubleParens eq Expression { Implement $2 [] [] (Unqualified $1) [] $4 }
               | identifier openParen Args closeParen eq Expression { Implement $2 [] [] (Unqualified $1) $3 $6 }
               | identifier Universals openParen Args closeParen eq Expression { Implement $3 [] $2 (Unqualified $1) $4 $7 }
               | Universals identifier openParen Args closeParen eq Expression { Implement $3 $1 [] (Unqualified $2) $4 $7 }
               | Universals identifier Universals openParen Args closeParen eq Expression { Implement $4 $1 $3 (Unqualified $2) $5 $8 }

Name : identifier { Unqualified $1 }
     | dollar identifier dot identifier { Qualified $1 $4 $2 }

RecordVal : identifier eq Expression { [($1, $3)] }
          | RecordVal comma identifier eq Expression { ($3, $5) : $1 }

Records : identifier eq Type { [ ($1, $3) ] }
        | Records comma identifier eq Type { ($3, $5) : $1 }

SumLeaf : vbar identifier { ($2, Nothing) }
        | vbar identifier of Type { ($2, Just $4) }

Leaves : SumLeaf { [$1] }
       | Leaves SumLeaf { $2 : $1 }

Universals : { [] }
           | Universals Universal { $2 : $1 }

OptTermetric : { Nothing }
             | Termetric { Just (snd $1) }

UnOp : tilde { Negate }

BinOp : plus { Add }
      | minus { Sub }
      | div { Div }
      | mult { Mult }
      | geq { GreaterThanEq }
      | leq { LessThanEq }
      | lbracket { LessThan }
      | rbracket { GreaterThan }
      | eq { Equal }
      | neq { NotEqual }
      | andOp { LogicalAnd }
      | or { LogicalOr }

OptExpression : { Nothing }
              | eq Expression { Just $2 }

DataPropLeaf : vbar Universals Expression { DataPropLeaf $2 $3 }

DataPropLeaves : { [] }
               | DataPropLeaves DataPropLeaf { $2 : $1 }

PreFunction : identifier openParen Args closeParen colon Type OptExpression { (PreF $1 [] [] $3 $6 Nothing $7) }
            | identifier Universals OptTermetric colon Type OptExpression { PreF $1 [] $2 [] $5 $3 $6 }
            | identifier Universals OptTermetric doubleParens colon Type OptExpression { PreF $1 [] $2 [] $6 $3 $7 }
            | identifier Universals OptTermetric openParen Args closeParen colon Type OptExpression { PreF $1 [] $2 $5 $8 $3 $9 }
            | Universals identifier Universals OptTermetric openParen Args closeParen colon Type OptExpression { PreF $2 $1 $3 $6 $9 $4 $10 }

Declaration : include string { Include $2 }
            | staload underscore eq string { Staload (Just "_") $4 } -- FIXME
            | staload string { Staload Nothing $2 }
            | staload identifier eq string { Staload (Just $2) $4 }
            | extern Declaration { Extern $1 $2 }
            | var Pattern colon Type eq PreExpression { Var (Just $4) $2 $6 }
            | val Pattern colon Type eq PreExpression { Val $1 (Just $4) $2 $6 }
            | and Pattern colon Type eq PreExpression { AndDecl (Just $4) $2 $6 }
            | val Pattern eq PreExpression { Val $1 Nothing $2 $4 }
            | and Pattern eq PreExpression { AndDecl Nothing $2 $4 }
            | var Pattern eq PreExpression { Var Nothing $2 $4 }
            | prval Pattern eq PreExpression { PrVal $2 $4 }
            | praxi PreFunction { Func $1 (Praxi $2) }
            | implement Implementation { Impl $2 }
            | primplmnt Implementation { ProofImpl $2 }
            | overload BinOp with Name { OverloadOp $1 $2 $4 }
            | stadef identifier eq Name { Stadef $2 $4 }
            | typedef identifier eq at lbrace Records rbrace { RecordType $2 $6 }
            | and PreFunction { Func $1 (And $2) }
            | fun PreFunction { Func $1 (Fun $2) }
            | fnx PreFunction { Func $1 (Fnx $2) }
            | vtypedef identifier eq Type { RecordViewType $2 $4 }
            | datavtype identifier eq Leaves { SumViewType $2 $4 }
            | absvtype identifier openParen Args closeParen eq Type { AbsViewType $1 $2 $4 $7 }
            | datatype identifier eq Leaves { SumType $2 $4 }
            | dataprop identifier openParen Args closeParen eq DataPropLeaves { DataProp $1 $2 $4 $7 }
            | define { Define $1 }
            | cblock { CBlock $1 }
            | lambda {% Left $ Expected $1 "Declaration" "lam" }
            | llambda {% Left $ Expected $1 "Declaration" "llam" }
            | ref {% Left $ Expected $1 "Declaration" "ref" }

{

data ATSError a = Expected AlexPosn a a
                | Unknown AlexPosn
                deriving (Eq, Show, Generic, NFData)

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> ":" <> pretty col

instance Pretty (ATSError String) where
    pretty (Expected p s1 s2) = red "Error: " <> pretty p <> linebreak <> (indent 2 $ "Unexpected" <+> squotes (string s1) <> ", expected:" <+> squotes (string s2)) <> linebreak
    pretty (Unknown p) = red "Error:" <+> "unknown" <+> pretty p <> linebreak

parseError :: [Token] -> Either (ATSError String) a
parseError = Left . Unknown . token_posn . head 
}
