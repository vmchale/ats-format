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
$special = [\+\-\&\|\[\]\{\}\(\)\_\=\!\%\^\$\@\;\~\:\,\.\\\#]
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

-- Identifiers -- FIXME this is buggy (can identifiers start with '!'?)
@identifier = $alpha ($alpha | $digit | _ | !)*

@block_comment = \(\* ([^\)] | \n)* \*\) | \/\* ([^\)] | \n)* \*\/ -- FIXME should be able to do (* ) *)

@ref = "ref<"

@ref_call = @identifier "<" ($alpha | $digit | "(" | ")" | _)+ ">"

@c_block = \%\{\^ (. # [\%] | \n)* \%\} -- FIXME only exclude the real bad parts

@lambda = "=>" | "=<cloref1>" | "=<cloptr1>"

@func_type = "-<fun>" | "-<cloptr1>" | "-<lincloptr1>" | "-<lin,prf>" | "-<>" | "-<prf>" -- FIXME allow spaces after comma?

@operator = "+" | "-" | "*" | "/" | "!=" | ">=" | "()" | "<=" | "=" | "~" | "&&" | "||" | ">" | "<" | "->" | ":=" | ".<" | ">." | ">>" | "?" | "?!" | "#[" -- TODO context so tilde doesn't follow |

-- TODO make stuff more specific = better errors + better speed.

tokens :-

    $white+                  ;
    "//".*                   ;
    @block_comment           ;
    "#define".*              { tok (\p s -> DefineBlock p s) }      
    @c_block                 { tok (\p s -> CBlockLex p s) }
    fun                      { tok (\p s -> Keyword p KwFun) }
    fnx                      { tok (\p s -> Keyword p KwFnx) }
    and                      { tok (\p s -> Keyword p KwAnd) }
    prval                    { tok (\p s -> Keyword p KwPrval) }
    prfn                     { tok (\p s -> Keyword p KwPrfn) }
    prfun                    { tok (\p s -> Keyword p KwPrfun) }
    datatype                 { tok (\p s -> Keyword p KwDatatype) }
    datavtype                { tok (\p s -> Keyword p KwDatavtype) }
    dataview                 { tok (\p s -> Keyword p KwDataview) }
    dataviewtype             { tok (\p s -> Keyword p KwDataviewtype) }
    dataprop                 { tok (\p s -> Keyword p KwDataprop) }
    assume                   { tok (\p s -> Keyword p KwAssume) }
    typedef                  { tok (\p s -> Keyword p KwTypedef) }
    vtypedef                 { tok (\p s -> Keyword p KwVtypedef) }
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
    abst"@"ype               { tok (\p s -> Keyword p (KwAbst0p None)) }
    t"@"ype"+"               { tok (\p s -> Keyword p (KwT0p Plus)) }
    t"@"ype"-"               { tok (\p s -> Keyword p (KwT0p Minus)) }
    t"@"ype                  { tok (\p s -> Keyword p (KwT0p None)) }
    vt"@"ype"+"              { tok (\p s -> Keyword p (KwVt0p Plus)) }
    vt"@"ype"-"              { tok (\p s -> Keyword p (KwVt0p Minus)) }
    vt"@"ype                 { tok (\p s -> Keyword p (KwVt0p None)) }
    abstype                  { tok (\p s -> Keyword p KwAbstype) }
    absvtype                 { tok (\p s -> Keyword p KwAbsvtype) }
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
    @time_lit                { tok (\p s -> TimeTok p s) }
    @char_lit                { tok (\p s -> CharTok p (s !! 1)) }
    @lambda                  { tok (\p s -> Arrow p s) }
    @func_type               { tok (\p s -> FuncType p s) }
    @bool                    { tok (\p s -> BoolTok p (read (over _head toUpper s)))}
    @integer                 { tok (\p s -> IntTok p (read s)) }
    @float                   { tok (\p s -> FloatTok p (read s)) }
    @ref                     { tok (\p s -> RefTok p) }
    @ref_call                { tok (\p s -> Identifier p s) }
    @operator                { tok (\p s -> Operator p s) }
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
             deriving (Eq, Show, Generic, NFData)

data Token = Semicolon AlexPosn
           | Identifier AlexPosn String
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
           | DefineBlock AlexPosn String
           | TimeTok AlexPosn String
           deriving (Eq, Show, Generic, NFData)

token_posn (Semicolon p) = p
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
token_posn (DefineBlock p _) = p
token_posn (TimeTok p _) = p

-- | This function turns a string into a stream of tokens for the parser.
lexATS :: String -> [Token]
lexATS = alexScanTokens

}
