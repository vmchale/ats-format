{

    {-# OPTIONS_GHC -fno-warn-unused-matches -fno-warn-incomplete-uni-patterns #-}
    {-# LANGUAGE DeriveGeneric #-}
    {-# LANGUAGE DeriveAnyClass #-}
    {-# LANGUAGE StandaloneDeriving #-}

    -- | Module exporting the lexer itself as well as several data types for
    -- working with tokens.
    module Language.ATS.Lexer ( AlexPosn (..)
                              , Token (..)
                              , Keyword (..)
                              , Addendum (..)
                              , lexATS
                              , token_posn
                              ) where

import Data.Char (toUpper)
import Control.Lens (over, _head)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)

}

%wrapper "posn"

-- Digits
$digit = 0-9

-- Characters
$special = [\+\-\&\|\[\]\{\}\(\)\_\=\!\%\^\$\@\;\~\,\.\\\#]
$alpha = [a-zA-Z]
$terminal = $printable # $white
$esc_char = \27
@escape_ch = \\ (n | t | \')
@escape_str = \\ (n | t | \")
@char = ($terminal # $special) | @escape_ch | $esc_char
@bool = (true | false)

@char_lit = \' @char \'

@time_lit = $digit+ u

-- Integer
@integer = $digit+

-- Floats
@decimals = $digit+
@float = @decimals \. @decimals

-- Strings
@string = \" ($printable # [\"] | @escape_str | $esc_char | \n)* \"

-- Identifiers
@identifier = $alpha ($alpha | $digit | _ | !)*

-- Multi-line comments
@not_close_paren = (\*+ [^\)] | [^\*] \))
@paren_comment = \(\* ([^\)\*] | @not_close_paren | \n)* \*\)
@not_close_slash = (\*+ [^\/] | [^\*] \/)
@slash_comment = \/\* ([^\/\*] | @not_close_slash | \n)* \*\/
@block_comment = @paren_comment | @slash_comment

@ref = "ref<"

@if_block = "#if" ([^\#] | "#then" | "#print" | \n)+ "#endif" .*

-- FIXME this is a disaster lol
@ref_call = @identifier "<" ($alpha | $digit | "(" | ")" | _)+ ">"

@not_close_c = \% [^\}]
@c_block = \%\{ ([^\%] | @not_close_c | \n)* \%\}

@lambda = "=>" | "=<cloref1>" | "=<cloptr1>" | "=>>"

-- FIXME whatever the fuck this is: -<cloptr,fe>
@func_type = "-<fun>" | "-<cloptr1>" | "-<lincloptr1>" | "-<lin,cloptr1>" | "-<lin,prf>" | "-<>" | "-<prf>" -- FIXME allow spaces after comma?

@operator = "+" | "-" | "*" | "/" | "!=" | ">=" | "<=" | "=" | "~" | "&&" | "||" | ">" | "<" | "->" | ":=" | ".<" | ">." | ">>" | "?" | "?!" | "#[" -- TODO context so tilde doesn't follow |

@double_parens = "(" (@block_comment | "") ")"
@double_braces = "{" (@block_comment | "") "}"
@double_brackets = "<" (@block_comment | "") ">"

@view = v | view

@signature = ":<>" | ":<!wrt>" | ":<!exnwrt>" | ":<!laz>" | ":<!all>" | ":"

tokens :-

    $white+                  ;
    ^ "//".*                 { tok (\p s -> CommentLex p s) }
    "//".*                   ;
    @block_comment           ;
    "#define".*              { tok (\p s -> MacroBlock p s) }      
    @if_block                { tok (\p s -> MacroBlock p s) }      
    @c_block                 { tok (\p s -> CBlockLex p s) }
    fun                      { tok (\p s -> Keyword p KwFun) }
    fnx                      { tok (\p s -> Keyword p KwFnx) }
    and                      { tok (\p s -> Keyword p KwAnd) }
    prval                    { tok (\p s -> Keyword p KwPrval) }
    prfn                     { tok (\p s -> Keyword p KwPrfn) }
    prfun                    { tok (\p s -> Keyword p KwPrfun) }
    datatype                 { tok (\p s -> Keyword p KwDatatype) }
    data @view type          { tok (\p s -> Keyword p KwDatavtype) }
    dataview                 { tok (\p s -> Keyword p KwDataview) }
    dataprop                 { tok (\p s -> Keyword p KwDataprop) }
    assume                   { tok (\p s -> Keyword p KwAssume) }
    typedef                  { tok (\p s -> Keyword p KwTypedef) }
    @view typedef            { tok (\p s -> Keyword p KwVtypedef) }
    absprop                  { tok (\p s -> Keyword p KwAbsprop) }
    llam                     { tok (\p s -> Keyword p KwLinearLambda) }
    lam                      { tok (\p s -> Keyword p KwLambda) }
    staload                  { tok (\p s -> Keyword p KwStaload) }
    let                      { tok (\p s -> Keyword p KwLet) }
    in                       { tok (\p s -> Keyword p KwIn) }
    end                      { tok (\p s -> Keyword p KwEnd) }
    case"+"                  { tok (\p s -> Keyword p (KwCase Plus)) }
    case"-"                  { tok (\p s -> Keyword p (KwCase Minus)) }
    case                     { tok (\p s -> Keyword p (KwCase None)) }
    castfn                   { tok (\p s -> Keyword p KwCastfn) }
    val"+"                   { tok (\p s -> Keyword p (KwVal Plus)) }
    val"-"                   { tok (\p s -> Keyword p (KwVal Minus)) }
    val                      { tok (\p s -> Keyword p (KwVal None)) }
    var                      { tok (\p s -> Keyword p KwVar) }
    int                      { tok (\p s -> Keyword p KwInt) }
    if                       { tok (\p s -> Keyword p KwIf) }
    sif                      { tok (\p s -> Keyword p KwSif) }
    then                     { tok (\p s -> Keyword p KwThen) }
    else                     { tok (\p s -> Keyword p KwElse) }
    string                   { tok (\p s -> Keyword p KwString) }
    bool                     { tok (\p s -> Keyword p KwBool) }
    void                     { tok (\p s -> Keyword p KwVoid) }
    nat                      { tok (\p s -> Keyword p KwNat) }
    implement                { tok (\p s -> Keyword p KwImplement) }
    primplmnt                { tok (\p s -> Keyword p KwProofImplement) }
    primplement              { tok (\p s -> Keyword p KwProofImplement) } -- TODO does this squash too much?
    abst"@"ype               { tok (\p s -> Keyword p (KwAbst0p None)) }
    t"@"ype"+"               { tok (\p s -> Keyword p (KwT0p Plus)) }
    t"@"ype"-"               { tok (\p s -> Keyword p (KwT0p Minus)) }
    t"@"ype                  { tok (\p s -> Keyword p (KwT0p None)) }
    @view"t@ype+"            { tok (\p s -> Keyword p (KwVt0p Plus)) }
    @view"t@ype-"            { tok (\p s -> Keyword p (KwVt0p Minus)) }
    @view"t@ype"             { tok (\p s -> Keyword p (KwVt0p None)) }
    abstype                  { tok (\p s -> Keyword p KwAbstype) }
    abs @view type           { tok (\p s -> Keyword p KwAbsvtype) }
    view                     { tok (\p s -> Keyword p KwView) }
    "#"include               { tok (\p s -> Keyword p KwInclude) }
    when                     { tok (\p s -> Keyword p KwWhen) }
    of                       { tok (\p s -> Keyword p KwOf) }
    stadef                   { tok (\p s -> Keyword p KwStadef) }
    local                    { tok (\p s -> Keyword p KwLocal) }
    praxi                    { tok (\p s -> Keyword p KwPraxi) }
    while                    { tok (\p s -> Keyword p KwWhile) }
    where                    { tok (\p s -> Keyword p KwWhere) }
    begin                    { tok (\p s -> Keyword p KwBegin) }
    overload                 { tok (\p s -> Keyword p KwOverload) }
    with                     { tok (\p s -> Keyword p KwWith) }
    char                     { tok (\p s -> Keyword p KwChar) }
    extern                   { tok (\p s -> Keyword p KwExtern) }
    sortdef                  { tok (\p s -> Keyword p KwSortdef) }
    propdef                  { tok (\p s -> Keyword p KwPropdef) }
    tkindef                  { tok (\p s -> Keyword p KwTKind) }
    "$raise"                 { tok (\p s -> Keyword p KwRaise) }
    @double_parens           { tok (\p s -> DoubleParenTok p) }
    @double_braces           { tok (\p s -> DoubleBracesTok p) }
    @double_brackets         { tok (\p s -> DoubleBracketTok p) }
    @char_lit                { tok (\p s -> CharTok p (s !! 1)) }
    @lambda                  { tok (\p s -> Arrow p s) }
    @func_type               { tok (\p s -> FuncType p s) }
    @bool                    { tok (\p s -> BoolTok p (read (over _head toUpper s)))}
    @time_lit                { tok (\p s -> TimeTok p s) }
    @integer                 { tok (\p s -> IntTok p (read s)) }
    @float                   { tok (\p s -> FloatTok p (read s)) }
    @ref                     { tok (\p s -> RefTok p) }
    @ref_call                { tok (\p s -> Identifier p s) }
    @operator                { tok (\p s -> Operator p s) }
    @signature               { tok (\p s -> SignatureTok p (tail s)) }
    $special                 { tok (\p s -> Special p s) }
    @identifier              { tok (\p s -> Identifier p s) }
    @string                  { tok (\p s -> StringTok p s) }

{

deriving instance Generic AlexPosn
deriving instance NFData AlexPosn

tok f p s = f p s

data Addendum = None
              | Plus
              | Minus
              deriving (Eq, Show, Generic, NFData)

data Keyword = KwFun
             | KwFnx
             | KwAnd
             | KwDatatype
             | KwDatavtype
             | KwDataviewtype
             | KwAssume
             | KwTypedef
             | KwVtypedef
             | KwStaload
             | KwLet
             | KwIn
             | KwLocal
             | KwEnd
             | KwImplement
             | KwCase Addendum
             | KwIf
             | KwSif
             | KwThen
             | KwElse
             | KwString
             | KwBool
             | KwInt
             | KwVoid
             | KwNat
             | KwVal Addendum
             | KwVar
             | KwLambda
             | KwLinearLambda
             | KwInclude
             | KwWhen
             | KwOf
             | KwAbsprop
             | KwPrval
             | KwStadef
             | KwPraxi
             | KwWhile
             | KwWhere
             | KwBegin
             | KwOverload
             | KwWith
             | KwChar
             | KwDataview
             | KwDataprop
             | KwView
             | KwAbstype
             | KwType
             | KwAbst0p Addendum
             | KwT0p Addendum
             | KwVt0p Addendum
             | KwPrfun
             | KwPrfn
             | KwCastfn
             | KwExtern
             | KwAbsvtype
             | KwProofImplement
             | KwSortdef
             | KwPropdef
             | KwRaise
             | KwTKind
             deriving (Eq, Show, Generic, NFData)

data Token = Identifier AlexPosn String
           | Keyword AlexPosn Keyword
           | BoolTok AlexPosn Bool
           | IntTok AlexPosn Int
           | FloatTok AlexPosn Float
           | CharTok AlexPosn Char
           | StringTok AlexPosn String
           | Special AlexPosn String
           | CBlockLex AlexPosn String
           | Operator AlexPosn String
           | Arrow AlexPosn String
           | FuncType AlexPosn String
           | CommentLex AlexPosn String
           | RefTok AlexPosn
           | MacroBlock AlexPosn String
           | TimeTok AlexPosn String
           | SignatureTok AlexPosn String
           | DoubleParenTok AlexPosn
           | DoubleBracesTok AlexPosn
           | DoubleBracketTok AlexPosn
           deriving (Eq, Show, Generic, NFData)

token_posn (Identifier p _) = p
token_posn (Keyword p _) = p
token_posn (BoolTok p _) = p
token_posn (IntTok p _) = p
token_posn (FloatTok p _) = p
token_posn (StringTok p _) = p
token_posn (Special p _) = p
token_posn (CBlockLex p _) = p
token_posn (Operator p _) = p
token_posn (Arrow p _) = p
token_posn (FuncType p _) = p
token_posn (CharTok p _) = p
token_posn (CommentLex p _) = p
token_posn (RefTok p) = p
token_posn (MacroBlock p _) = p
token_posn (TimeTok p _) = p
token_posn (SignatureTok p _) = p
token_posn (DoubleParenTok p) = p
token_posn (DoubleBracesTok p) = p
token_posn (DoubleBracketTok p) = p

-- | This function turns a string into a stream of tokens for the parser.
lexATS :: String -> [Token]
lexATS = alexScanTokens

}
