{
    {-# LANGUAGE OverloadedStrings #-}
    {-# LANGUAGE DeriveGeneric     #-}
    {-# LANGUAGE DeriveAnyClass    #-}
    {-# LANGUAGE FlexibleContexts  #-}

    -- | This module contains the parser.
    module Language.ATS.Parser ( parseATS
                               , ATSError
                               ) where

import Language.ATS.Types
import Language.ATS.Lexer ( Token (..)
                          , AlexPosn (..)
                          , Keyword (..)
                          , Addendum (..)
                          , token_posn
                          )

import Control.DeepSeq (NFData)
import Control.Lens (over, _head)
import GHC.Generics (Generic)
import Prelude
import Text.PrettyPrint.ANSI.Leijen hiding ((<$>))

}

%name parseATS
%tokentype { Token }
%error { parseError }
%monad { Either (ATSError String) } { (>>=) } { pure }

%token
    fun { Keyword $$ KwFun }
    fn { Keyword $$ KwFn }
    prfun { Keyword $$ KwPrfun }
    fnx { Keyword $$ KwFnx }
    and { Keyword $$ KwAnd }
    lambda { Keyword $$ KwLambda }
    llambda { Keyword $$ KwLinearLambda }
    if { Keyword $$ KwIf }
    sif { Keyword $$ KwSif }
    stadef { Keyword $$ KwStadef }
    val { Keyword _ (KwVal $$) }
    prval { Keyword $$ KwPrval }
    var { Keyword $$ KwVar }
    then { Keyword $$ KwThen }
    let { Keyword $$ KwLet }
    typedef { Keyword $$ KwTypedef }
    vtypedef { Keyword $$ KwVtypedef }
    absvtype { Keyword $$ KwAbsvtype }
    abstype { Keyword $$ KwAbstype }
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
    t0pCo { Keyword $$ (KwT0p Plus) }
    vt0pCo { Keyword $$ (KwVt0p Plus) }
    vt0pPlain { Keyword $$ (KwVt0p None) }
    where { Keyword $$ KwWhere }
    absprop { Keyword $$ KwAbsprop }
    sortdef { Keyword $$ KwSortdef }
    local { Keyword $$ KwLocal }
    view { Keyword $$ KwView }
    raise { Keyword $$ KwRaise }
    tkindef { Keyword $$ KwTKind }
    assume { Keyword $$ KwAssume }
    addrAt { Keyword $$ KwAddrAt }
    viewAt { Keyword $$ KwViewAt }
    boolLit { BoolTok _ $$ }
    timeLit { TimeTok _ $$ }
    intLit { IntTok _ $$ }
    floatLit { FloatTok _ $$ }
    effmaskWrt { Identifier $$ "effmask_wrt" }
    effmaskAll { Identifier $$ "effmask_all" }
    extfcall { Identifier $$ "extfcall" }
    ldelay { Identifier $$ "ldelay" }
    listVT { Identifier $$ "list_vt" }
    -- TODO token? raise { Identifier $$ "raise" }
    identifier { Identifier _ $$ }
    closeParen { Special $$ ")" }
    openParen { Special $$ "(" }
    signature { SignatureTok _ $$ }
    comma { Special $$ "," }
    geq { Operator $$ ">=" }
    leq { Operator $$ "<=" }
    neq { Operator $$ "!=" }
    openTermetric { Operator $$ ".<" }
    closeTermetric { Operator $$ ">." }
    mutateArrow { FuncType $$ "->" }
    mutateEq { Operator $$ ":=" }
    lbracket { Operator $$ "<" }
    rbracket { Operator $$ ">" }
    eq { Operator $$ "=" }
    or { Operator $$ "||" }
    vbar { Special $$ "|" }
    lbrace { Special $$ "{" }
    rbrace { Special $$ "}" }
    funcArrow { FuncType _ $$ }
    plainArrow { Arrow $$ "=>" }
    cloref1Arrow { Arrow $$ "=<cloref1>" }
    cloptr1Arrow { Arrow $$ "=<cloptr1>" }
    lincloptr1Arrow { Arrow $$ "=<lincloptr1>" }
    spear { Arrow $$ "=>>" }
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
    doubleEq { Operator $$ "==" }
    doubleDot { Operator $$ ".." }
    doubleParens { DoubleParenTok $$ }
    doubleBraces { DoubleBracesTok $$ }
    prfTransform { Operator $$ ">>" } -- For types like &a >> a?!
    refType { Special $$ "&" } -- For types like &a
    maybeProof { Operator $$ "?" } -- For types like a?
    fromVT { Operator $$ "?!" } -- For types like a?!
    openExistential { Operator $$ "#[" } -- Same as `[` in ATS2
    cblock { CBlockLex _ $$ }
    define { MacroBlock _ $$ }
    lineComment { CommentLex _ $$ }
    lspecial { SpecialBracket $$ }
    atbrace { Operator $$ "@{" }
    mod { Keyword $$ KwMod }
    fixAt { Keyword $$ KwFixAt }
    lambdaAt { Keyword $$ KwLambdaAt }

%%

ATS : Declarations { ATS $1 }

-- | Parse declarations in a list
Declarations : { [] } 
             | Declarations Declaration { $2 : $1 }
             | Declarations FunDecl { $2 ++ $1 }
             | Declarations local Declarations in Declarations end { Local $2 $3 $5 : $1 }

-- | Several comma-separated types
TypeIn : Type { [$1] }
       | TypeIn comma Type { $3 : $1 }

-- | Several comma-separated types or static expressions
TypeInExpr : TypeIn { $1 }
           | Expression { [ConcreteType $1] }
           | TypeInExpr comma Type { $3 : $1 }
           | TypeInExpr comma Expression { ConcreteType $3 : $1 }

-- | Parse a type
Type : Name openParen TypeInExpr closeParen { Dependent $1 $3 }
     | bool { Bool }
     | int { Int }
     | nat { Nat }
     | stringType { String }
     | charType { Char }
     | voidType { Void }
     | t0pPlain { T0p None }
     | t0pCo { T0p Plus }
     | vt0pPlain { Vt0p None }
     | vt0pCo { Vt0p Plus }
     | stringType openParen StaticExpression closeParen { DepString $3 }
     | stringType StaticExpression { DepString $2 }
     | int openParen StaticExpression closeParen { DependentInt $3 }
     | bool openParen StaticExpression closeParen { DependentBool $3 }
     | identifier { Named $1 }
     | exclamation Type { Unconsumed $2 }
     | Type mutateArrow Type { FunctionType "->" $1 $3 }
     | Type funcArrow Type { FunctionType $2 $1 $3 }
     | refType Type { RefType $2 }
     | Type maybeProof { MaybeVal $1 } 
     | Type fromVT { FromVT $1 }
     | Type prfTransform Type { AsProof $1 (Just $3) }
     | Type prfTransform underscore { AsProof $1 Nothing }
     | view at Type { ViewType $1 $3 }
     | Existential Type { Ex $1 $2 }
     | Universal Type { ForA $1 $2 }
     | Type at Type { At $2 $1 $3 }
     | openParen Type vbar Type closeParen { ProofType $1 $2 $4 }
     | Name identifier { Dependent $1 [Named $2] }
     | openParen TypeIn closeParen { Tuple $1 $2 }
     | openParen Type closeParen { $2 }
     | int StaticExpression { DependentInt $2 }
     | doubleParens { NoneType $1 }
     | minus {% Left $ Expected $1 "Type" "-" }
     | dollar {% Left $ Expected $1 "Type" "$" }

FullArgs : Args { $1 }

FunArgs : Arg { Comma $1 Nil }
        | FunArgs comma Arg { Comma $3 $1 }
        | FunArgs vbar Arg { Bar $3 $1 }

-- | A comma-separated list of arguments
Args : Arg { [$1] }
     | Args comma Arg { $3 : $1 }
     | Arg vbar Arg { [ PrfArg $1 $3 ] }
     | FullArgs comma Arg vbar Arg { PrfArg $3 $5 : $1 }

Arg : identifier { Arg (First $1) }
    | identifier signature Type { Arg (Both $1 $3) }
    | underscore { Arg (First "_") }
    | Type { Arg (Second $1) }
    | Expression { Arg (Second (ConcreteType $1)) }

-- | Parse a literal
Literal : boolLit { BoolLit $1 }
        | timeLit { TimeLit $1 }
        | intLit { IntLit $1 }
        | floatLit { FloatLit $1 }
        | string { StringLit $1 }
        | charLit { CharLit $1 }
        | doubleParens { VoidLiteral $1 }

-- | Parse a list of comma-separated patterns
PatternIn : Pattern { [$1] }
          | PatternIn comma Pattern { $3 : $1 }

-- | Parse a pattern match
Pattern : identifier { PName $1 [] }
        | underscore { Wildcard $1 }
        | identifier doubleParens { PName ($1 ++ "()") [] }
        | tilde Pattern { Free $2 }
        | identifier openParen PatternIn closeParen { PName $1 $3 }
        | identifier Pattern { PSum $1 $2 }
        | openParen PatternIn vbar PatternIn closeParen { Proof $1 $2 $4 }
        | openParen PatternIn closeParen { TuplePattern $2 }
        | Literal { PLiteral $1 }
        | Pattern when Expression { Guarded $2 $3 $1 }
        | minus {% Left $ Expected $1 "Pattern" "-" }
        | plus {% Left $ Expected $1 "Pattern" "+" }

-- | Parse a case expression
Case : vbar Pattern CaseArrow Expression { [($2, $3, $4)] }
     | Pattern CaseArrow Expression { [($1, $2, $3)] }
     | Case vbar Pattern CaseArrow Expression { ($3, $4, $5) : $1 }

ExpressionPrf : ExpressionIn { (Nothing, $1) }
              | Expression vbar ExpressionIn { (Just $1, $3) } -- FIXME only passes one proof?

-- | A list of comma-separated expressions
ExpressionIn : Expression { [$1] }
             | ExpressionIn comma Expression { $3 : $1 }

Tuple : PreExpression comma PreExpression { [$3, $1] }
      | Tuple comma PreExpression { $3 : $1 }

-- | Parse an arrow in a case statement
CaseArrow : plainArrow { Plain $1 }
          | spear { Spear $1 }
          | minus {% Left $ Expected $1 "Arrow" "-" }
          | eq {% Left $ Expected $1 "Arrow" "=" }

LambdaArrow : plainArrow { Plain $1 }
            | cloref1Arrow { Full $1 "cloref1" } -- TODO do this more efficiently.
            | cloptr1Arrow { Full $1 "cloptr1" }
            | lincloptr1Arrow { Full $1 "lincloptr1" }
            | minus {% Left $ Expected $1 "Arrow" "-" }
            | openParen {% Left $ Expected $1 "Arrow" "(" }
            | closeParen {% Left $ Expected $1 "Arrow" ")" }

-- | Expression or named call to an expression
Expression : PreExpression { $1 }
           | openParen Tuple closeParen { TupleEx $1 $2 }
           | Name PreExpression { Call $1 [] [] Nothing [$2] }
           | begin Expression end { Begin $1 $2 }
           | Expression semicolon Expression { Precede $1 $3 }
           | Expression semicolon { $1 }
           | openParen Expression closeParen { ParenExpr $1 $2 }
           | Expression signature Type { TypeSignature $1 $3 }
           | openParen Expression vbar Expression closeParen { ProofExpr $1 $2 $4 }

TypeArgs : lbrace Type rbrace { [$2] }
         | lbrace TypeIn rbrace { $2 }
         | TypeArgs lbrace Type rbrace { $3 : $1 }
         | lbrace doubleDot rbrace { [ ImplicitType $2 ] } -- FIXME only valid on function calls
         | TypeArgs lbrace TypeIn rbrace { $3 ++ $1 }

BracketedArgs : lbracket Type rbracket { [$2] }
              | lbracket TypeIn rbrace { $2 }

Call : Name doubleParens { Call $1 [] [] Nothing [] }
     | Name openParen ExpressionPrf closeParen { Call $1 [] [] (fst $3) (snd $3) }
     | Name TypeArgs openParen ExpressionPrf closeParen { Call $1 [] $2 (fst $4) (snd $4) }
     | Name TypeArgs { Call $1 [] $2 Nothing [] }
     | Name lspecial TypeIn rbracket openParen ExpressionPrf closeParen { Call $1 $3 [] (fst $6) (snd $6) }
     | Name lspecial TypeIn rbracket { Call $1 $3 [] Nothing [] }
     | dollar raise PreExpression { Call (SpecialName $1 "raise") [] [] Nothing [$3] } -- we do this because a $raise can have at most one argument

StaticExpression : Name { StaticVal $1 }
                 | StaticExpression BinOp StaticExpression { StaticBinary $2 $1 $3 }
                 | intLit { StaticInt $1 }
                 | boolLit { StaticBool $1 }
                 | sif StaticExpression then StaticExpression else StaticExpression { Sif $2 $4 $6 } -- TODO separate type for static expressions

-- | Parse an expression that can be called without parentheses
PreExpression : identifier lsqbracket PreExpression rsqbracket { Index $2 (Unqualified $1) $3 }
              | Literal { $1 }
              | Call { $1 }
              | case PreExpression of Case { Case $3 $1 $2 $4 }
              | PreExpression BinOp Expression { Binary $2 $1 $3 }
              | openParen PreExpression BinOp PreExpression closeParen { ParenExpr $1 (Binary $3 $2 $4) }
              | UnOp PreExpression { Unary $1 $2 } -- FIXME throw error when we try to negate a string literal/time
              | PreExpression dot Name { Access $2 $1 $3 }
              | if Expression then Expression { If $2 $4 Nothing}
              | if Expression then Expression else Expression { If $2 $4 (Just $6) }
              | let ATS in end { Let $1 $2 Nothing }
              | let ATS in Expression end { Let $1 $2 (Just $4) }
              | lambda Pattern LambdaArrow Expression { Lambda $1 $3 $2 $4 }
              | llambda Pattern LambdaArrow Expression { LinearLambda $1 $3 $2 $4 }
              | addrAt PreExpression { AddrAt $1 $2 }
              | viewAt PreExpression { ViewAt $1 $2 }
              | PreExpression at PreExpression { AtExpr $1 $3 }
              | atbrace RecordVal rbrace { RecordValue $1 $2 Nothing }
              | atbrace RecordVal rbrace signature Type { RecordValue $1 $2 (Just $5) }
              | exclamation PreExpression { Deref $1 $2 }
              | PreExpression mutateArrow identifier mutateEq PreExpression { FieldMutate $2 $1 $3 $5 }
              | PreExpression mutateEq PreExpression { Mutate $1 $3 }
              | PreExpression where lbrace Declarations rbrace { WhereExp $1 $4 }
              | Name { NamedVal $1 }
              | lbrace ATS rbrace { Actions $2 }
              | while openParen PreExpression closeParen PreExpression { While $1 $3 $5 }
              | include {% Left $ Expected $1 "Expression" "include" }
              | staload {% Left $ Expected $1 "Expression" "staload" }
              | overload {% Left $ Expected $1 "Expression" "overload" }
              | prval {% Left $ Expected $1 "Expression" "prval" }
              | var {% Left $ Expected $1 "Expression" "var" }
              | Termetric {% Left $ Expected (fst $1) "Expression" "termetric" }
              | fromVT {% Left $ Expected $1 "Expression" "?!" }
              | prfTransform {% Left $ Expected $1 "Expression" ">>" }
              | maybeProof {% Left $ Expected $1 "Expression" "?" }
              | let openParen {% Left $ Expected $1 "Declaration" "(" }

-- | Parse a termetric
Termetric : openTermetric Expression closeTermetric { ($1, $2) }
          | underscore {% Left $ Expected $1 "_" "Name" }
          | dollar {% Left $ Expected $1 "$" "Name" }

-- | Parse an existential quantier on a type
Existential : lsqbracket Args vbar Expression rsqbracket { Existential $2 Nothing (Just $4) }
            | lsqbracket Args rsqbracket { Existential $2 Nothing Nothing }
            | openExistential Args rsqbracket { Existential $2 Nothing Nothing }
            | openExistential Args vbar Expression rsqbracket { Existential $2 Nothing (Just $4) }
            | lsqbracket Args signature Type rsqbracket { Existential $2 (Just $4) Nothing } -- FIXME arguments should include more than just ':'
            | lsqbracket Expression rsqbracket { Existential [] Nothing (Just $2) }
            
-- | Parse a universal quantifier on a type
Universal : lbrace Args rbrace { Universal $2 Nothing Nothing }
          | lbrace Args vbar Expression rbrace { Universal $2 Nothing (Just $4) }

-- | Parse the details of an implementation
Implementation : FunName doubleParens eq Expression { Implement $2 [] [] $1 [] $4 }
               | FunName openParen FullArgs closeParen eq Expression { Implement $2 [] [] $1 $3 $6 }
               | FunName Universals openParen FullArgs closeParen eq Expression { Implement $3 [] $2 $1 $4 $7 }
               | Universals FunName openParen FullArgs closeParen eq Expression { Implement $3 $1 [] $2 $4 $7 }
               | Universals FunName Universals openParen FullArgs closeParen eq Expression { Implement $4 $1 $3 $2 $5 $8 }

-- | Parse a function name
FunName : identifier { Unqualified $1 }
        | identifier dollar identifier { Functorial $1 $3 }

-- | Parse a general name
Name : identifier { Unqualified $1 }
     | listVT { Unqualified "list_vt" }
     | dollar identifier dot identifier { Qualified $1 $4 $2 }
     | dollar effmaskWrt { SpecialName $1 "effmask_wrt" }
     | dollar effmaskAll { SpecialName $1 "effmask_all" }
     | dollar listVT { SpecialName $1 "list_vt" }
     | dollar ldelay { SpecialName $1 "ldelay" } -- FIXME there is probably a better/more efficient way of doing this
     | underscore {% Left $ Expected $1 "_" "Name" }
     | dollar {% Left $ Expected $1 "$" "Name" }

-- | Parse a list of values in a record
RecordVal : identifier eq Expression { [($1, $3)] }
          | RecordVal comma identifier eq Expression { ($3, $5) : $1 }

-- | Parse a list of types in a record
Records : identifier eq Type { [($1, $3)] }
        | Records comma identifier eq Type { ($3, $5) : $1 }

-- | Parse a constructor for a sum type
SumLeaf : vbar identifier { ($2, Nothing) }
        | vbar identifier of Type { ($2, Just $4) }

-- | Parse all constructors of a sum type
Leaves : SumLeaf { [$1] }
       | Leaves SumLeaf { $2 : $1 }
       | identifier of Type { [($1, Just $3)] }
       | identifier { [($1, Nothing)] }
       | dollar {% Left $ Expected $1 "$" "|" }

Universals : { [] }
           | doubleBraces { [] }
           | Universals Universal { $2 : $1 }

-- | Optionally parse a termetric
OptTermetric : { Nothing }
             | Termetric { Just (snd $1) }

-- | Parse a unary operator
UnOp : tilde { Negate }

-- | Parse a binary operator
BinOp : plus { Add }
      | minus { Sub }
      | div { Div }
      | mult { Mult }
      | geq { GreaterThanEq }
      | leq { LessThanEq }
      | lbracket { LessThan }
      | rbracket { GreaterThan }
      | neq { NotEqual }
      | andOp { LogicalAnd }
      | or { LogicalOr }
      | doubleEq { StaticEq }
      | eq { Equal }
      | mod { Mod }

-- | Optionally parse a function body
OptExpression : { Nothing }
              | eq Expression { Just $2 } -- FIXME only let this happen for external declarations

-- | Parse a constructor for a 'dataprop'
DataPropLeaf : vbar Universals Expression { DataPropLeaf $2 $3 }

-- | Parse several constructors for a 'dataprop'
DataPropLeaves : DataPropLeaf { [$1] }
               | DataPropLeaves DataPropLeaf { $2 : $1 }
               | prval {% Left $ Expected $1 "Constructor" "prval" }
               | var {% Left $ Expected $1 "Constructor" "var" }
               | lambda {% Left $ Expected $1 "Constructor" "lam" }
               | llambda {% Left $ Expected $1 "Constructor" "llam" }
               | minus {% Left $ Expected $1 "Constructor" "-" }
               | dollar {% Left $ Expected $1 "Constructor" "$" }
               | fromVT {% Left $ Expected $1 "Constructor" "?!" }
               | prfTransform {% Left $ Expected $1 "Constructor" ">>" }
               | maybeProof {% Left $ Expected $1 "Constructor" "?" }

-- | Parse a type signature and optional function body
PreFunction : FunName openParen FullArgs closeParen signature Type OptExpression { (PreF $1 $5 [] [] $3 $6 Nothing $7) }
            | FunName Universals OptTermetric signature Type OptExpression { PreF $1 $4 [] $2 [NoArgs] $5 $3 $6 }
            | FunName Universals OptTermetric doubleParens signature Type OptExpression { PreF $1 $5 [] $2 [] $6 $3 $7 }
            | FunName Universals OptTermetric openParen FullArgs closeParen signature Type OptExpression { PreF $1 $7 [] $2 $5 $8 $3 $9 }
            | Universals FunName Universals OptTermetric openParen FullArgs closeParen signature Type OptExpression { PreF $2 $8 $1 $3 $6 $9 $4 $10 }
            | prval {% Left $ Expected $1 "Function signature" "prval" }
            | var {% Left $ Expected $1 "Function signature" "var" }
            | lambda {% Left $ Expected $1 "Function signature" "lam" }
            | llambda {% Left $ Expected $1 "Function signature" "llam" }

-- | Parse affiliated `sortdef`s
AndSort : AndSort and identifier eq Type { AndD $1 (SortDef $2 $3 $5) } -- TODO figure out if this is building up the slow way
        | sortdef identifier eq Type { SortDef $1 $2 $4 }

-- | Function declaration
FunDecl : fun PreFunction { [ Func $1 (Fun $2) ] }
        | fn PreFunction { [ Func $1 (Fn $2) ] }
        | prfun PreFunction { [ Func $1 (PrFun $2) ] }
        | fnx PreFunction { [ Func $1 (Fnx $2) ] }
        | extern FunDecl { over _head (Extern $1) $2 }
        | FunDecl and PreFunction { Func $2 (And $3) : $1 }
        | extern fun PreFunction eq {% Left $ Expected $1 "Declaration" "Function body" }
        | lambda {% Left $ Expected $1 "Function declaration" "lam" }
        | llambda {% Left $ Expected $1 "Function declaration" "llam" }

-- | Parse a declaration defining a type
TypeDecl : typedef identifier eq Universals atbrace Records rbrace { RecordType $2 [] $4 $6 }
         | typedef identifier openParen FullArgs closeParen eq Universals atbrace Records rbrace { RecordType $2 $4 $7 $9 }
         | vtypedef identifier eq Universals atbrace Records rbrace { RecordViewType $2 [] $4 $6 }
         | vtypedef identifier openParen FullArgs closeParen eq Universal atbrace Records rbrace { RecordViewType $2 $4 [$7] $9 }
         | vtypedef identifier openParen FullArgs closeParen eq Universals atbrace Records rbrace { RecordViewType $2 $4 $7 $9 }
         | datatype identifier eq Leaves { SumType $2 [] $4 }
         | datatype identifier openParen Args closeParen eq Leaves { SumType $2 $4 $7 }
         | datavtype identifier eq Leaves { SumViewType $2 [] $4 }
         | datavtype identifier openParen Args closeParen eq Leaves { SumViewType $2 $4 $7 }
         | abstype identifier openParen FullArgs closeParen eq Type { AbsType $1 $2 $4 $7 }
         | absvtype identifier openParen FullArgs closeParen eq Type { AbsViewType $1 $2 $4 $7 }
         | dataprop identifier openParen FullArgs closeParen eq DataPropLeaves { DataProp $1 $2 $4 $7 }
         | absprop identifier openParen FullArgs closeParen { AbsProp $1 $2 [] }
         | typedef identifier eq Type { TypeDef $1 $2 [] $4 }
         | typedef identifier openParen FullArgs closeParen eq Type { TypeDef $1 $2 $4 $7 }
         | vtypedef identifier eq Type { ViewTypeDef $1 $2 [] $4 }
         | vtypedef identifier openParen FullArgs closeParen eq Type { ViewTypeDef $1 $2 $4 $7 }
         | stadef identifier eq Name { Stadef $2 $4 [] }
         | stadef identifier eq Name openParen TypeIn closeParen { Stadef $2 $4 $6 }
         | sortdef identifier eq Type { SortDef $1 $2 $4 }
         | AndSort { $1 }

-- | Parse a declaration
Declaration : include string { Include $2 }
            | define { Define $1 }
            | cblock { CBlock $1 }
            | lineComment { Comment $1 }
            | staload underscore eq string { Staload (Just "_") $4 }
            | staload string { Staload Nothing $2 }
            | staload identifier eq string { Staload (Just $2) $4 }
            | extern Declaration { Extern $1 $2 }
            | var Pattern signature Type with PreExpression { Var (Just $4) $2 Nothing (Just $6) } -- FIXME
            | var Pattern signature Type eq PreExpression { Var (Just $4) $2 (Just $6) Nothing }
            | val Pattern signature Type eq PreExpression { Val $1 (Just $4) $2 $6 }
            | val Pattern eq Expression { Val $1 Nothing $2 $4 }
            | var Pattern eq PreExpression { Var Nothing $2 (Just $4) Nothing }
            | var Pattern signature Type { Var (Just $4) $2 Nothing Nothing }
            | var Pattern eq fixAt identifier openParen Args closeParen signature Type plainArrow Expression { Var Nothing $2 (Just $ FixAt (PreF (Unqualified $5) $9 [] [] $7 $10 Nothing (Just $12))) Nothing }
            | var Pattern eq lambdaAt openParen Args closeParen signature Type plainArrow Expression { Var Nothing $2 (Just $ LambdaAt (PreF (Unnamed $4) $8 [] [] $6 $9 Nothing (Just $11))) Nothing }
            | prval Pattern eq PreExpression { PrVal $2 $4 }
            | praxi PreFunction { Func $1 (Praxi $2) }
            | primplmnt Implementation { ProofImpl $2 }
            | implement Implementation { Impl [] $2 }
            | implement openParen Args closeParen Implementation { Impl $3 $5 }
            | overload BinOp with Name { OverloadOp $1 $2 $4 }
            | assume Name openParen Args closeParen eq Expression { Assume $2 $4 $7 }
            | tkindef Name eq string { TKind $1 $2 $4 }
            | TypeDecl { $1 }
            | lambda {% Left $ Expected $1 "Declaration" "lam" }
            | llambda {% Left $ Expected $1 "Declaration" "llam" }
            | minus {% Left $ Expected $1 "Declaration" "-" }
            | dollar {% Left $ Expected $1 "Declaration" "$" }
            | fromVT {% Left $ Expected $1 "Declaration" "?!" }
            | prfTransform {% Left $ Expected $1 "Declaration" ">>" }
            | maybeProof {% Left $ Expected $1 "Declaration" "?" }

{

data ATSError a = Expected AlexPosn a a
                | Unknown Token
                deriving (Eq, Show, Generic, NFData)

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> ":" <> pretty col

instance Pretty (ATSError String) where
    pretty (Expected p s1 s2) = red "Error: " <> pretty p <> linebreak <> (indent 2 $ "Unexpected" <+> squotes (string s2) <> ", expected:" <+> squotes (string s1)) <> linebreak
    pretty (Unknown t) = red "Error:" <+> "unexpected token" <+> squotes (pretty t) <+> "at" <+> pretty (token_posn t) <> linebreak

parseError :: [Token] -> Either (ATSError String) a
parseError = Left . Unknown . head 
}
