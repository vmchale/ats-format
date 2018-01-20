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
                          , to_string
                          , get_addendum
                          )

import Data.Char (toLower)
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
    castfn { Keyword $$ KwCastfn }
    prfun { Keyword $$ KwPrfun }
    prfn { Keyword $$ KwPrfn }
    fnx { Keyword $$ KwFnx }
    and { Keyword $$ KwAnd }
    lambda { Keyword $$ KwLambda }
    llambda { Keyword $$ KwLinearLambda }
    if { Keyword $$ KwIf }
    sif { Keyword $$ KwSif }
    stadef { Keyword $$ KwStadef }
    val { $$@(Keyword _ (KwVal _)) }
    prval { Keyword $$ KwPrval }
    var { Keyword $$ KwVar }
    then { Keyword $$ KwThen }
    let { Keyword $$ KwLet }
    typedef { Keyword $$ KwTypedef }
    vtypedef { Keyword $$ KwVtypedef }
    absview { Keyword $$ KwAbsview }
    absvtype { Keyword $$ KwAbsvtype }
    abstype { Keyword $$ KwAbstype }
    abst0p { Keyword $$ (KwAbst0p None) }
    absvt0p { Keyword $$ (KwAbsvt0p None) }
    viewdef { Keyword $$ KwViewdef }
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
    addr { Keyword $$ KwAddr }
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
    view { Keyword $$ (KwView None) }
    viewPlusMinus { Keyword _ (KwView $$) }
    raise { Keyword $$ KwRaise }
    tkindef { Keyword $$ KwTKind }
    assume { Keyword $$ KwAssume }
    addrAt { Keyword $$ KwAddrAt }
    viewAt { Keyword $$ KwViewAt }
    symintr { Keyword $$ KwSymintr }
    stacst { Keyword $$ KwStacst }
    propdef { Keyword $$ KwPropdef }
    list { Keyword $$ (KwListLit "") }
    list_vt { Keyword $$ (KwListLit "_vt") }
    boolLit { BoolTok _ $$ }
    timeLit { TimeTok _ $$ }
    intLit { IntTok _ $$ }
    floatLit { FloatTok _ $$ }
    effmaskWrt { Identifier $$ "effmask_wrt" }
    effmaskAll { Identifier $$ "effmask_all" }
    extype { Identifier $$ "extype" }
    extfcall { Identifier $$ "extfcall" }
    ldelay { Identifier $$ "ldelay" }
    listVT { Identifier $$ "list_vt" }
    foldAt { Identifier $$ "fold@" }
    identifier { $$@Identifier{} }
    identifierSpace { $$@IdentifierSpace{} }
    closeParen { Special $$ ")" }
    openParen { Special $$ "(" }
    colon { SignatureTok $$ "" }
    signature { SignatureTok _ $$ }
    comma { Special $$ "," }
    percent { Operator $$ "%" }
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
    doubleBrackets { DoubleBracketTok $$ }
    prfTransform { Operator $$ ">>" } -- For types like &a >> a?!
    refType { Special $$ "&" } -- For types like &a
    maybeProof { Operator $$ "?" } -- For types like a?
    fromVT { Operator $$ "?!" } -- For types like a?!
    openExistential { Operator $$ "#[" } -- Same as `[` in ATS2
    cblock { CBlockLex _ $$ }
    define { MacroBlock _ $$ }
    lineComment { $$@CommentLex{} }
    lspecial { SpecialBracket $$ }
    atbrace { Operator $$ "@{" }
    exp { Operator $$ "**" }
    mod { Keyword $$ KwMod }
    fixAt { Keyword $$ KwFixAt }
    lamAt { Keyword $$ KwLambdaAt }
    infixr { FixityTok $$ "infixr" }
    infixl { FixityTok $$ "infixr" }
    prefix { FixityTok $$ "prefix" }
    postfix { FixityTok $$ "postfix" }

%%

ATS : Declarations { ATS $1 }

-- | Parse declarations in a list
Declarations : { [] } 
             | Declarations Declaration { $2 : $1 }
             | Declarations FunDecl { $2 ++ $1 }
             | Declarations local ATS in ATS end { Local $2 $3 $5 : $1 }

-- | Several comma-separated types
TypeIn : Type { [$1] }
       | TypeIn comma Type { $3 : $1 }

-- | Several comma-separated types or static expressions
TypeInExpr : TypeIn { $1 }
           | StaticExpression { [ConcreteType $1] }
           | TypeInExpr comma Type { $3 : $1 }
           | TypeInExpr comma StaticExpression { ConcreteType $3 : $1 }

-- | Parse a type
Type : Name openParen TypeInExpr closeParen { Dependent $1 $3 }
     | identifierSpace openParen TypeInExpr closeParen { Dependent (Unqualified $ to_string $1) $3 }
     | bool { Bool }
     | int { Int }
     | nat { Nat }
     | addr { Addr }
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
     | identifierSpace { Named (Unqualified $ to_string $1) }
     | Name { Named $1 }
     | exclamation Type { Unconsumed $2 }
     | Type mutateArrow Type { FunctionType "->" $1 $3 }
     | Type funcArrow Type { FunctionType $2 $1 $3 }
     | refType Type { RefType $2 }
     | Type maybeProof { MaybeVal $1 } 
     | Type fromVT { FromVT $1 }
     | Type prfTransform Type { AsProof $1 (Just $3) }
     | Type prfTransform underscore { AsProof $1 Nothing }
     | view at Type { ViewType $1 $3 }
     | viewPlusMinus { ViewLiteral $1 }
     | view { ViewLiteral None }
     | Existential Type { Ex $1 $2 }
     | Universal Type { ForA $1 $2 }
     | Type at Type { At $2 (Just $1) $3 }
     | at Type { At $1 Nothing $2 }
     | atbrace Records rbrace { AnonymousRecord $1 $2 }
     | openParen Type vbar Type closeParen { ProofType $1 $2 $4 }
     | identifierSpace identifier { Dependent (Unqualified $ to_string $1) [Named (Unqualified $ to_string $2)] }
     | openParen TypeIn closeParen { Tuple $1 $2 }
     | openParen Type closeParen { $2 }
     | int StaticExpression { DependentInt $2 }
     | doubleParens { NoneType $1 }
     | minus {% Left $ Expected $1 "Type" "-" }
     | dollar {% Left $ Expected $1 "Type" "$" }
     | int identifier openParen {% Left $ Expected (token_posn $2) "Static integer expression" (to_string $2) }

FullArgs : Args { $1 }

-- | A comma-separated list of arguments
Args : Arg { [$1] }
     | FullArgs comma Arg vbar Arg { PrfArg $3 $5 : $1 }
     | Args comma Arg { $3 : $1 }
     | Arg vbar Arg { [ PrfArg $1 $3 ] }

TypeArg : identifier { Arg (First $ to_string $1) }
        | IdentifierOr colon Type { Arg (Both $1 $3) }
        | Type { Arg (Second $1) }

Arg : TypeArg { $1 }
    | StaticExpression { Arg (Second (ConcreteType $1)) }

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
Pattern : identifier { PName (to_string $1) [] }
        | identifierSpace { PName (to_string $1) [] }
        | underscore { Wildcard $1 }
        | identifier doubleParens { PName (to_string $1 ++ "()") [] }
        | tilde Pattern { Free $2 }
        | identifier openParen PatternIn closeParen { PName (to_string $1) $3 }
        | identifier Pattern { PSum (to_string $1) $2 }
        | identifierSpace Pattern { PSum (to_string $1) $2 }
        | openParen PatternIn vbar PatternIn closeParen { Proof $1 $2 $4 }
        | openParen PatternIn closeParen { TuplePattern $2 }
        | Literal { PLiteral $1 }
        | Pattern when Expression { Guarded $2 $3 $1 }
        | at Pattern { AtPattern $1 $2 }
        | identifier Universals Pattern { UniversalPattern (token_posn $1) (to_string $1) $2 $3 }
        | identifierSpace Universals Pattern { UniversalPattern (token_posn $1) (to_string $1) $2 $3 }
        | Existential Pattern { ExistentialPattern $1 $2 }
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
          | minus {% Left $ Expected $1 "Arrow" "-" }

LambdaArrow : plainArrow { Plain $1 }
            | cloref1Arrow { Full $1 "cloref1" } -- TODO do this more efficiently.
            | cloptr1Arrow { Full $1 "cloptr1" }
            | lincloptr1Arrow { Full $1 "lincloptr1" }
            | minus {% Left $ Expected $1 "Arrow" "-" }
            | openParen {% Left $ Expected $1 "Arrow" "(" }
            | closeParen {% Left $ Expected $1 "Arrow" ")" }

-- | Expression or named call to an expression
Expression : identifierSpace PreExpression { Call (Unqualified $ to_string $1) [] [] Nothing [$2] }
           | PreExpression { $1 }
           | openParen Tuple closeParen { TupleEx $1 $2 }
           | Expression semicolon Expression { Precede $1 $3 }
           | Expression semicolon { $1 }
           | openParen Expression closeParen { $2 }
           | Expression colon Type { TypeSignature $1 $3 } -- TODO is a more general expression sensible?
           | openParen Expression vbar Expression closeParen { ProofExpr $1 $2 $4 }
           | list_vt lbrace Type rbrace openParen ExpressionIn closeParen { ListLiteral $1 "vt" $3 $6 }
           | list lbrace Type rbrace openParen ExpressionIn closeParen { ListLiteral $1 "" $3 $6 }
           | begin Expression extern {% Left $ Expected $3 "end" "extern" }
           | Expression prfTransform underscore {% Left $ Expected $2 "Rest of expression or declaration" ">>" }

TypeArgs : lbrace Type rbrace { [$2] }
         | lbrace TypeIn rbrace { $2 }
         | TypeArgs lbrace Type rbrace { $3 : $1 }
         | lbrace doubleDot rbrace { [ ImplicitType $2 ] } -- FIXME only valid on function calls
         | TypeArgs lbrace TypeIn rbrace { $3 ++ $1 }

BracketedArgs : lbracket Type rbracket { [$2] }
              | lbracket TypeIn rbrace { $2 }

Call : Name doubleParens { Call $1 [] [] Nothing [] }
     | identifierSpace openParen ExpressionPrf closeParen { Call (Unqualified $ to_string $1) [] [] (fst $3) (snd $3) }
     | Name openParen ExpressionPrf closeParen { Call $1 [] [] (fst $3) (snd $3) }
     | Name TypeArgs openParen ExpressionPrf closeParen { Call $1 [] $2 (fst $4) (snd $4) }
     | Name TypeArgs { Call $1 [] $2 Nothing [] }
     | Name lspecial TypeIn rbracket doubleParens { Call $1 $3 [] Nothing [VoidLiteral $5] }
     | Name lspecial TypeIn rbracket openParen ExpressionPrf closeParen { Call $1 $3 [] (fst $6) (snd $6) }
     | Name lspecial TypeIn rbracket { Call $1 $3 [] Nothing [] }
     | raise PreExpression { Call (SpecialName $1 "raise") [] [] Nothing [$2] } -- $raise can have at most one argument
     | Name openParen ExpressionPrf end {% Left $ Expected $4 ")" "end"}
     | Name openParen ExpressionPrf else {% Left $ Expected $4 ")" "else"}

StaticArgs : StaticExpression { [$1] }
           | StaticArgs comma StaticExpression { $3 : $1 }

StaticExpression : Name { StaticVal $1 }
                 | StaticExpression BinOp StaticExpression { StaticBinary $2 $1 $3 }
                 | intLit { StaticInt $1 }
                 | doubleParens { StaticVoid $1 }
                 | boolLit { StaticBool $1 }
                 | sif StaticExpression then StaticExpression else StaticExpression { Sif $2 $4 $6 } -- TODO separate type for static expressions
                 | identifierSpace { StaticVal (Unqualified $ to_string $1) }
                 | Name openParen StaticArgs closeParen { SCall $1 $3 }
                 | identifierSpace openParen StaticArgs closeParen { SCall (Unqualified $ to_string $1) $3 }
                 | StaticExpression semicolon StaticExpression { SPrecede $1 $3 }
    
-- | Parse an expression that can be called without parentheses
PreExpression : identifier lsqbracket PreExpression rsqbracket { Index $2 (Unqualified $ to_string $1) $3 }
              | Literal { $1 }
              | Call { $1 }
              | case Expression of Case { Case $3 $1 $2 $4 }
              | openParen Expression closeParen { ParenExpr $1 $2 }
              | PreExpression BinOp PreExpression { Binary $2 $1 $3 }
              | UnOp PreExpression { Unary $1 $2 } -- FIXME throw error when we try to negate a string literal/time
              | PreExpression dot Name { Access $2 $1 $3 }
              | PreExpression dot intLit { Access $2 $1 (Unqualified $ show $3) }
              | PreExpression dot identifierSpace { Access $2 $1 (Unqualified $ to_string $3) }
              | if Expression then Expression { If $2 $4 Nothing}
              | if Expression then Expression else Expression { If $2 $4 (Just $6) }
              | let ATS in end { Let $1 $2 Nothing }
              | let ATS in Expression end { Let $1 $2 (Just $4) }
              | let ATS in Expression vbar {% Left $ Expected $5 "end" "|" }
              | lambda Pattern LambdaArrow Expression { Lambda $1 $3 $2 $4 }
              | llambda Pattern LambdaArrow Expression { LinearLambda $1 $3 $2 $4 }
              | addrAt PreExpression { AddrAt $1 $2 }
              | viewAt PreExpression { ViewAt $1 $2 }
              | PreExpression at PreExpression { AtExpr $1 $3 }
              | atbrace RecordVal rbrace { RecordValue $1 $2 Nothing }
              | atbrace RecordVal rbrace colon Type { RecordValue $1 $2 (Just $5) }
              | exclamation PreExpression { Deref $1 $2 }
              | PreExpression mutateArrow identifierSpace mutateEq PreExpression { FieldMutate $2 $1 (to_string $3) $5 }
              | PreExpression mutateArrow identifier mutateEq PreExpression { FieldMutate $2 $1 (to_string $3) $5 }
              | PreExpression mutateEq PreExpression { Mutate $1 $3 }
              | PreExpression where lbrace Declarations rbrace { WhereExp $1 $4 }
              | identifierSpace { NamedVal (Unqualified $ to_string $1) }
              | begin Expression end { Begin $1 $2 }
              | Name { NamedVal $1 }
              | lbrace ATS rbrace { Actions $2 }
              | while openParen PreExpression closeParen PreExpression { While $1 $3 $5 }
              | include {% Left $ Expected $1 "Expression" "include" }
              | staload {% Left $ Expected $1 "Expression" "staload" }
              | overload {% Left $ Expected $1 "Expression" "overload" }
              | var {% Left $ Expected $1 "Expression" "var" }
              | Termetric {% Left $ Expected (fst $1) "Expression" "termetric" }
              | fromVT {% Left $ Expected $1 "Expression" "?!" }
              | prfTransform {% Left $ Expected $1 "Expression" ">>" }
              | maybeProof {% Left $ Expected $1 "Expression" "?" }
              | let openParen {% Left $ Expected $1 "Expression" "let (" }
              | let ATS in Expression lineComment {% Left $ Expected (token_posn $5) "end" (take 2 $ to_string $5) }
              | let ATS in Expression extern {% Left $ Expected $5 "end" "extern" }
              | let ATS in Expression fun {% Left $ Expected $5 "end" "fun" }
              | let ATS in Expression vtypedef {% Left $ Expected $5 "end" "vtypedef" }
              | if Expression then Expression else else {% Left $ Expected $6 "Expression" "else" }

-- | Parse a termetric
Termetric : openTermetric StaticExpression closeTermetric { ($1, $2) }
          | underscore {% Left $ Expected $1 "_" "Termination metric" }
          | dollar {% Left $ Expected $1 "$" "Termination metric" }

-- | Parse an existential quantier on a type
Existential : lsqbracket Args vbar StaticExpression rsqbracket { Existential $2 False Nothing (Just $4) }
            | lsqbracket Args rsqbracket { Existential $2 False Nothing Nothing }
            | openExistential Args rsqbracket { Existential $2 True Nothing Nothing }
            | openExistential Args vbar StaticExpression rsqbracket { Existential $2 True Nothing (Just $4) }
            | lsqbracket Args colon Type rsqbracket { Existential $2 False (Just $4) Nothing } -- FIXME arguments should include more than just ':'
            | lsqbracket StaticExpression rsqbracket { Existential [] False Nothing (Just $2) }
            
-- | Parse a universal quantifier on a type
Universal : lbrace Args rbrace { Universal $2 Nothing Nothing }
          | lbrace Args vbar StaticExpression rbrace { Universal $2 Nothing (Just $4) }

-- | Parse the details of an implementation
Implementation : FunName doubleParens eq Expression { Implement $2 [] [] $1 [] $4 }
               | FunName openParen FullArgs closeParen eq Expression { Implement $2 [] [] $1 $3 $6 }
               | FunName Universals openParen FullArgs closeParen eq Expression { Implement $3 [] $2 $1 $4 $7 }
               | Universals FunName openParen FullArgs closeParen eq Expression { Implement $3 $1 [] $2 $4 $7 }
               | Universals FunName Universals openParen FullArgs closeParen eq Expression { Implement $4 $1 $3 $2 $5 $8 }

-- | Parse a function name
FunName : IdentifierOr { Unqualified $1 }
        | identifier dollar identifier { Functorial (to_string $1) (to_string $3) }

-- | Parse a general name
Name : identifier { Unqualified (to_string $1) }
     | underscore { Unqualified "_" }
     | listVT { Unqualified "list_vt" }
     | foldAt { Unqualified "fold@" }
     | dollar identifier dot identifier { Qualified $1 (to_string $4) (to_string $2) }
     | dollar identifier dot identifierSpace { Qualified $1 (to_string $4) (to_string $2) }
     | dollar effmaskWrt { SpecialName $1 "effmask_wrt" }
     | dollar effmaskAll { SpecialName $1 "effmask_all" }
     | dollar extype { SpecialName $1 "effmask_all" }
     | dollar listVT { SpecialName $1 "list_vt" }
     | dollar ldelay { SpecialName $1 "ldelay" } -- FIXME there is probably a better/more efficient way of doing this
     | dollar {% Left $ Expected $1 "Name" "$" }

-- | Parse a list of values in a record
RecordVal : IdentifierOr eq Expression { [($1, $3)] }
          | RecordVal comma IdentifierOr eq Expression { ($3, $5) : $1 }

-- | Parse a list of types in a record
Records : IdentifierOr eq Type { [($1, $3)] }
        | Records comma IdentifierOr eq Type { ($3, $5) : $1 }

IdentifiersIn : IdentifierOr { [$1] }
              | IdentifiersIn comma IdentifierOr { $3 : $1 }

OfType : { Nothing }
       | of Type { Just $2 }

-- | Parse a constructor for a sum type
SumLeaf : vbar Universals identifier { Leaf $2 (to_string $3) [] Nothing }
        | vbar Universals identifierSpace of Type { Leaf $2 (to_string $3) [] (Just $5) }
        | vbar Universals IdentifierOr openParen IdentifiersIn closeParen OfType { Leaf $2 $3 $5 $7 }

-- | Parse all constructors of a sum type
Leaves : SumLeaf { [$1] }
       | Leaves SumLeaf { $2 : $1 }
       | Universals identifierSpace of Type { [Leaf $1 (to_string $2) [] (Just $4)] }
       | Universals identifier { [Leaf $1 (to_string $2) [] Nothing] }
       | Universals identifier openParen IdentifiersIn closeParen OfType { [Leaf $1 (to_string $2) $4 $6] } -- FIXME should take any static expression.
       | dollar {% Left $ Expected $1 "|" "$" }

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
      | percent { Mod }

-- | Optionally parse a function body
OptExpression : { Nothing }
              | eq Expression { Just $2 } -- FIXME only let this happen for external declarations

-- | Parse a constructor for a 'dataprop'
DataPropLeaf : vbar Universals Expression { DataPropLeaf $2 $3 Nothing }
             | Universals Expression { DataPropLeaf $1 $2 Nothing }
             | vbar Universals Expression of Expression { DataPropLeaf $2 $3 (Just $5) }
             | Universals Expression of Expression { DataPropLeaf $1 $2 (Just $4) }

-- | Parse several constructors for a 'dataprop'
DataPropLeaves : DataPropLeaf { [$1] }
               | DataPropLeaves DataPropLeaf { $2 : $1 }
               | prval {% Left $ Expected $1 "Constructor" "prval" }
               | var {% Left $ Expected $1 "Constructor" "var" }
               | val {% Left $ Expected (token_posn $1) "Constructor" "val" }
               | lambda {% Left $ Expected $1 "Constructor" "lam" }
               | llambda {% Left $ Expected $1 "Constructor" "llam" }
               | minus {% Left $ Expected $1 "Constructor" "-" }
               | dollar {% Left $ Expected $1 "Constructor" "$" }
               | fromVT {% Left $ Expected $1 "Constructor" "?!" }
               | prfTransform {% Left $ Expected $1 "Constructor" ">>" }
               | maybeProof {% Left $ Expected $1 "Constructor" "?" }

Signature : signature { $1 }
          | colon { "" }

OptType : Type { Just $1 }
        | { Nothing }

-- | Parse a type signature and optional function body
PreFunction : FunName openParen FullArgs closeParen Signature OptType OptExpression { (PreF $1 $5 [] [] $3 $6 Nothing $7) }
            | FunName Universals OptTermetric Signature OptType OptExpression { PreF $1 $4 [] $2 [NoArgs] $5 $3 $6 }
            | FunName Universals OptTermetric doubleParens Signature OptType OptExpression { PreF $1 $5 [] $2 [] $6 $3 $7 }
            | FunName Universals OptTermetric openParen FullArgs closeParen Signature OptType OptExpression { PreF $1 $7 [] $2 $5 $8 $3 $9 }
            | Universals FunName Universals OptTermetric openParen FullArgs closeParen Signature OptType OptExpression { PreF $2 $8 $1 $3 $6 $9 $4 $10 }
            | Universals FunName Universals OptTermetric Signature OptType OptExpression { PreF $2 $5 $1 $3 [] $6 $4 $7 }
            | prval {% Left $ Expected $1 "Function signature" "prval" }
            | var {% Left $ Expected $1 "Function signature" "var" }
            | val {% Left $ Expected (token_posn $1) "Function signature" "val" }
            | lambda {% Left $ Expected $1 "Function signature" "lam" }
            | llambda {% Left $ Expected $1 "Function signature" "llam" }
            | lsqbracket {% Left $ Expected $1 "Function signature" "[" }

-- | Parse affiliated `sortdef`s
AndSort : AndSort and IdentifierOr eq Type { AndD $1 (SortDef $2 $3 $5) } -- TODO figure out if this is building up the slow way
        | sortdef IdentifierOr eq Type { SortDef $1 $2 $4 }

-- | Function declaration
FunDecl : fun PreFunction { [ Func $1 (Fun $2) ] }
        | prfun PreFunction { [ Func $1 (PrFun $2) ] }
        | prfn PreFunction { [ Func $1 (PrFn $2) ] }
        | fnx PreFunction { [ Func $1 (Fnx $2) ] }
        | castfn PreFunction { [ Func $1 (CastFn $2) ] }
        | fn PreFunction identifier {% Left $ Expected (token_posn $3) "=" (to_string $3) }
        | fn PreFunction { [ Func $1 (Fn $2) ] }
        | FunDecl and PreFunction { Func $2 (And $3) : $1 }
        | extern FunDecl { over _head (Extern $1) $2 }
        | extern fun PreFunction eq {% Left $ Expected $1 "Declaration" "Function body" }
        | extern fnx PreFunction eq {% Left $ Expected $1 "Declaration" "Function body" }
        | extern praxi PreFunction eq {% Left $ Expected $1 "Declaration" "Function body" }
        | extern prfun PreFunction eq {% Left $ Expected $1 "Declaration" "Function body" }
        | extern prfn PreFunction eq {% Left $ Expected $1 "Declaration" "Function body" }
        | lambda {% Left $ Expected $1 "Function declaration" "lam" }
        | llambda {% Left $ Expected $1 "Function declaration" "llam" }
        | fun fn {% Left $ Expected $2 "Function name" "fn" }
        | fn fun {% Left $ Expected $2 "Function name" "fun" }
        | extern FunDecl identifier openParen {% Left $ Expected (token_posn $3) "Static integer expression" (to_string $3) }

IdentifierOr : identifier { to_string $1 }
             | identifierSpace { to_string $1 }

MaybeType : eq Type { Just $2 }
          | { Nothing }

-- | Parse a declaration defining a type
TypeDecl : typedef IdentifierOr eq Type { TypeDef $1 $2 [] $4 }
         | typedef IdentifierOr openParen FullArgs closeParen eq Type { TypeDef $1 $2 $4 $7 }
         | vtypedef IdentifierOr eq Type { ViewTypeDef $1 $2 [] $4 }
         | vtypedef IdentifierOr openParen FullArgs closeParen eq Type { ViewTypeDef $1 $2 $4 $7 }
         | datatype IdentifierOr eq Leaves { SumType $2 [] $4 }
         | datatype IdentifierOr openParen Args closeParen eq Leaves { SumType $2 $4 $7 }
         | datavtype IdentifierOr eq Leaves { SumViewType $2 [] $4 }
         | datavtype IdentifierOr openParen Args closeParen eq Leaves { SumViewType $2 $4 $7 }
         | abst0p IdentifierOr eq Type { AbsT0p $1 $2 $4 }
         | viewdef IdentifierOr openParen FullArgs closeParen eq Type { ViewDef $1 $2 $4 $7 }
         | absvt0p IdentifierOr openParen FullArgs closeParen MaybeType { AbsVT0p $1 $2 $4 $6 }
         | absvt0p IdentifierOr eq Type { AbsVT0p $1 $2 [] (Just $4) }
         | absview IdentifierOr openParen FullArgs closeParen MaybeType { AbsView $1 $2 $4 $6 }
         | abstype IdentifierOr openParen FullArgs closeParen MaybeType { AbsType $1 $2 $4 $6 }
         | absvtype IdentifierOr openParen FullArgs closeParen MaybeType { AbsViewType $1 $2 $4 $6 }
         | dataprop IdentifierOr openParen FullArgs closeParen eq DataPropLeaves { DataProp $1 $2 $4 $7 }
         | absprop IdentifierOr openParen FullArgs closeParen { AbsProp $1 $2 $4 }
         | stadef IdentifierOr eq Name { Stadef $2 $4 [] }
         | stadef IdentifierOr eq Name openParen TypeIn closeParen { Stadef $2 $4 $6 }
         | stadef boolLit eq Name { Stadef (over _head toLower (show $2)) $4 [] } -- TODO identifierSpace
         | sortdef IdentifierOr eq Type { SortDef $1 $2 $4 }
         | AndSort { $1 }

Fixity : infixr { RightFix $1 }
       | infixl { LeftFix $1 }
       | prefix { Pre $1 }
       | postfix { Post $1 }

Operator : identifierSpace { to_string $1 }
         | exp { "**" }

Operators : Operator { [$1] }
          | Operators Operator { $2 : $1 }
          | Operators identifier { to_string $2 : $1 }

StackFunction : openParen Args closeParen Signature Type plainArrow Expression { StackF $4 $2 $5 $7 }

-- | Parse a declaration
Declaration : include string { Include $2 }
            | define { Define $1 }
            | define identifierSpace string { Define ($1 ++ " " ++ to_string $2 ++ $3) } -- FIXME better approach?
            | define identifierSpace intLit { Define ($1 ++ " " ++ to_string $2 ++ " " ++ show $3) }
            | cblock { CBlock $1 }
            | lineComment { Comment (to_string $1) }
            | staload underscore eq string { Staload (Just "_") $4 }
            | staload string { Staload Nothing $2 }
            | staload IdentifierOr eq string { Staload (Just $2) $4 }
            | extern Declaration { Extern $1 $2 }
            | var Pattern colon Type with PreExpression { Var (Just $4) $2 Nothing (Just $6) } -- FIXME signature is too general.
            | var Pattern colon Type eq PreExpression { Var (Just $4) $2 (Just $6) Nothing }
            | val Pattern colon Type eq PreExpression { Val (get_addendum $1) (Just $4) $2 $6 }
            | val Pattern eq Expression { Val (get_addendum $1) Nothing $2 $4 }
            | var Pattern eq Expression { Var Nothing $2 (Just $4) Nothing }
            | var Pattern colon Type { Var (Just $4) $2 Nothing Nothing }
            | var Pattern eq fixAt IdentifierOr StackFunction { Var Nothing $2 (Just $ FixAt $5 $6) Nothing }
            | var Pattern eq lamAt StackFunction { Var Nothing $2 (Just $ LambdaAt $5) Nothing }
            | prval Pattern eq Expression { PrVal $2 $4 }
            | praxi PreFunction { Func $1 (Praxi $2) }
            | primplmnt Implementation { ProofImpl $2 }
            | implement Implementation { Impl [] $2 }
            | implement openParen Args closeParen Implementation { Impl $3 $5 }
            | overload BinOp with Name { OverloadOp $1 $2 $4 }
            | overload identifierSpace with Name { OverloadIdent $1 (to_string $2) $4 Nothing }
            | overload tilde with identifierSpace of intLit { OverloadIdent $1 "~" (Unqualified $ to_string $4) (Just $6) } -- FIXME figure out a general solution.
            | assume Name openParen Args closeParen eq Expression { Assume $2 $4 $7 }
            | tkindef IdentifierOr eq string { TKind $1 (Unqualified $2) $4 }
            | TypeDecl { $1 }
            | symintr Name { SymIntr $1 $2 }
            | stacst IdentifierOr colon Type OptExpression { Stacst $1 (Unqualified $2) $4 $5 }
            | propdef IdentifierOr openParen Args closeParen eq Type { PropDef $1 $2 $4 $7 }
            | Fixity intLit Operators { FixityDecl $1 (Just $2) $3 }
            | val Universals IdentifierOr colon Type { StaVal $2 $3 $5 }
            | lambda {% Left $ Expected $1 "Declaration" "lam" }
            | llambda {% Left $ Expected $1 "Declaration" "llam" }
            | minus {% Left $ Expected $1 "Declaration" "-" }
            | dollar {% Left $ Expected $1 "Declaration" "$" }
            | fromVT {% Left $ Expected $1 "Declaration" "?!" }
            | prfTransform {% Left $ Expected $1 "Declaration" ">>" }
            | maybeProof {% Left $ Expected $1 "Declaration" "?" }
            | identifier {% Left $ Expected (token_posn $1) "Declaration" (to_string $1) }

{

data ATSError a = Expected AlexPosn a a
                | Unknown Token -- FIXME error type for expression when a static expression was expected (?)
                deriving (Eq, Show, Generic, NFData)

instance Pretty AlexPosn where
    pretty (AlexPn _ line col) = pretty line <> ":" <> pretty col

instance Pretty (ATSError String) where
    pretty (Expected p s1 s2) = red "Error: " <> pretty p <> linebreak <> (indent 2 $ "Unexpected" <+> squotes (string s2) <> ", expected:" <+> squotes (string s1)) <> linebreak
    pretty (Unknown t) = red "Error:" <+> "unexpected token" <+> squotes (pretty t) <+> "at" <+> pretty (token_posn t) <> linebreak

parseError :: [Token] -> Either (ATSError String) a
parseError = Left . Unknown . head 
}
