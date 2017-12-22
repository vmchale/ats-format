{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.ATS.PrettyPrint ( printATS
                                , printATSCustom
                                , processClang
                                ) where

import           Control.Arrow                         hiding ((<+>))
import           Control.Composition
import           Control.DeepSeq                       (NFData)
#if __GLASGOW_HASKELL__ >= 801
import           Data.Function                         (on)
#endif
import           Data.Functor.Foldable                 (cata)
import           GHC.Generics                          (Generic)
import           Language.ATS.Types
import           Prelude                               hiding ((<$>))
import           System.Console.ANSI.Types
import           System.Process                        (readCreateProcess, shell)
import           Text.PrettyPrint.ANSI.Leijen
import           Text.PrettyPrint.ANSI.Leijen.Internal

deriving instance Generic Underlining
deriving instance NFData Underlining
deriving instance Generic ConsoleIntensity
deriving instance NFData ConsoleIntensity
deriving instance Generic Color
deriving instance NFData Color
deriving instance Generic ColorIntensity
deriving instance NFData ColorIntensity
deriving instance Generic ConsoleLayer
deriving instance NFData ConsoleLayer
deriving instance Generic Doc
deriving instance NFData Doc

instance Eq Doc where
    (==) = on (==) show

-- FIXME account for %{, %{$, and %{#
processClang :: String -> IO String
processClang ('%':'{':'^':xs) = fmap (('%':) . ('{':) . ('^':)) $ fmap (<> (snd $ takeBlock xs)) $ printClang (fst $ takeBlock xs)
    where
        takeBlock :: String -> (String, String)
        takeBlock ('%':'}':ys) = ("", ('%':) . ('}':) $ ys)
        takeBlock (y:ys)       = first (y:) $ takeBlock ys
        takeBlock []           = ([], [])
processClang (x:xs) = fmap (x:) $ processClang xs
processClang [] = pure []

printClang :: String -> IO String
printClang = readCreateProcess (shell "clang-format")

printATS :: ATS -> String
printATS (ATS x) = g mempty
    where g = (displayS . renderPretty 0.6 120 . pretty) (ATS $ reverse x)

printATSCustom :: Float -> Int -> ATS -> String
printATSCustom r i (ATS x) = g mempty
    where g = (displayS . renderPretty r i . pretty) (ATS $ reverse x)


instance Pretty Name where
    pretty (Unqualified n)   = string n
    pretty (Qualified _ i n) = "$" <> string n <> "." <> string i
    pretty (SpecialName _ s) = "$" <> string s
    pretty (Functorial s s') = string s <> "$" <> string s'

instance Pretty LambdaType where
    pretty Plain{}    = "=>"
    pretty (Full _ v) = "=<" <> string v <> ">"

instance Pretty BinOp where
    pretty Mult          = "*"
    pretty Add           = "+"
    pretty Div           = "/"
    pretty Sub           = "-"
    pretty GreaterThan   = ">"
    pretty LessThan      = "<"
    pretty Equal         = "="
    pretty NotEqual      = "!="
    pretty LogicalAnd    = "&&"
    pretty LogicalOr     = "||"
    pretty LessThanEq    = "<="
    pretty GreaterThanEq = ">="

splits :: BinOp -> Bool
splits Mult       = True
splits Add        = True
splits Div        = True
splits Sub        = True
splits LogicalAnd = True
splits LogicalOr  = True
splits _          = False

instance Pretty Addendum where
    pretty Plus  = "+"
    pretty Minus = "-"
    pretty None  = ""

startsParens :: Doc -> Bool
startsParens d = f (show d) where
    f ('(':_) = True
    f _       = False

instance Pretty Expression where
    pretty = cata a where
        a (IfF e e' (Just e''))         = "if" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (IfF e e' Nothing)            = "if" <+> e <+> "then" <$> indent 2 e'
        a (SifF e e' e'')               = "sif" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (LetF _ e (Just e'))          = "let" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) e)) <$> "in" <$> indent 2 e' <$> "end" -- TODO soft linebreak?
        a (LetF _ e Nothing)            = "let" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) e)) <$> "in end"
        a (BoolLitF True)               = "true"
        a (BoolLitF False)              = "false"
        a (TimeLitF s)                  = string s
        a (IntLitF i)                   = pretty i
        a (LambdaF _ lt p e)            = "lam" <+> pretty p <+> pretty lt <+> e
        a (LinearLambdaF _ lt p e)      = "llam" <+> pretty p <+> pretty lt <+> e
        a (FloatLitF f)                 = pretty f
        a (StringLitF s)                = string s
        a (BinListF op es)
            | splits op = encloseSep (head es) (" " <> pretty op <> " ") (last es) (init $ tail es)
            | otherwise = "error state"
        a (BinaryF op e e')
            | splits op = e </> pretty op <+> e'
            | otherwise = e <+> pretty op <+> e'
        a (IndexF _ n e)                = pretty n <> "[" <> e <> "]"
        a (UnaryF Negate e)             = "~" <> e
        a (NamedValF name)              = pretty name
        a (CallF name [] [] Nothing []) = pretty name <> "()"
        a (CallF name [] [] Nothing [x])
            | startsParens x = pretty name <> pretty x
        a (CallF name [] [] Nothing xs) = pretty name <> prettyArgsG "(" ")" xs
        a (CallF name [] ys Nothing []) = pretty name <> prettyArgsG "<" ">" (fmap pretty ys)-- FIXME this should use { too?
        a (CallF name us [] Nothing []) = pretty name <> prettyArgsG "{" "}" us
        a (CallF name [] ys Nothing xs) = pretty name <> prettyArgsG "<" ">" (fmap pretty ys) <> prettyArgsG "(" ")" xs
        a (CallF name us [] Nothing [x])
            | startsParens x = pretty name <> prettyArgsG "{" "}" us <> pretty x
        a (CallF name us [] Nothing xs) = pretty name <> prettyArgsG "{" "}" us <> prettyArgsG "(" ")" xs
        a (CallF name us ys Nothing [x])
            | startsParens x = pretty name <> prettyArgsG "{" "}" us <> prettyArgsG "<" ">" (fmap pretty ys) <> pretty x
        a (CallF name us ys Nothing xs) = pretty name <> prettyArgsG "{" "}" us <> prettyArgsG "<" ">" (fmap pretty ys) <+> prettyArgsG "(" ")" xs
        a (CaseF _ add e cs)            = "case" <> pretty add <+> e <+> "of" <$> indent 2 (prettyCases cs)
        a (VoidLiteralF _)              = "()"
        a (RecordValueF _ es Nothing)   = prettyRecord es
        a (RecordValueF _ es (Just x))  = prettyRecord es <+> ":" <+> pretty x
        a (PrecedeF e e')               = parens (e <+> ";" </> e')
        a (FieldMutateF _ o f v)        = pretty o <> "->" <> string f <+> ":=" <+> v
        a (MutateF e e')                = e <+> ":=" <+> e'
        a (DerefF _ e)                  = "!" <> e
        a (AccessF _ e n)
            | noParens e = e <> "." <> pretty n
            | otherwise = parens e <> "." <> pretty n
        a (CharLitF '\\')              = "'\\\\'"
        a (CharLitF '\n')              = "'\\n'"
        a (CharLitF '\t')              = "'\\t'"
        a (CharLitF c)                 = "'" <> char c <> "'"
        a (RefF _ t e)                 = "ref<" <> pretty t <> ">" <> parens e
        a (ProofExprF _ e e')          = "(" <> e <+> "|" <+> e' <> ")"
        a (TypeSignatureF e t)         = e <+> ":" <+> pretty t
        a (WhereExpF e d)              = e <+> "where" <$> braces (" " <> pretty (ATS d) <> " ")
        a (TupleExF _ es)              = prettyArgs es -- parens (mconcat $ punctuate ", " (reverse es))
        a (WhileF _ e e')              = "while" <> parens e <> e'
        a (ActionsF as)                = "{" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) as)) <$> "}"
        a UnderscoreLitF{}             = "_"
        a (AtExprF e e')               = e <> "@" <> parens e'
        a (BeginF _ e)
            | not (startsParens e) = linebreak <> indent 2 ("begin" <$> indent 2 e <$> "end")
            | otherwise = e
        a _ = "FIXME"
        prettyCases []           = mempty
        prettyCases [(s, t)]     = "|" <+> pretty s <+> "=>" <+> t
        prettyCases ((s, t): xs) = prettyCases xs $$ "|" <+> pretty s <+> "=>" <+> t

noParens :: Doc -> Bool
noParens = all (`notElem` ("()" :: String)) . show

instance Pretty Pattern where
    pretty = cata a where
        a (WildcardF _)      = "_"
        a (PSumF s x)        = string s <+> x
        a (PLiteralF e)      = pretty e
        a (PNameF s [])      = string s
        a (PNameF s [x])     = string s <> parens x
        a (PNameF s ps)      = string s <> parens (mconcat (punctuate ", " (reverse ps)))
        a (FreeF p)          = "~" <> p
        a (GuardedF _ e p)   = p <+> "when" <+> pretty e
        a (ProofF _ p p')    = parens (p <+> "|" <+> p')
        a NullPatternF{}     = "()"
        a (TuplePatternF ps) = parens (mconcat (punctuate ", " (reverse ps)))

instance Pretty Arg where
    pretty (Arg s Nothing)   = pretty s
    pretty (Arg "" (Just t)) = pretty t
    pretty (Arg s (Just t))  = pretty s <+> colon <+> pretty t
    pretty (PrfArg a as)     = "(" <> pretty a <+> "|" <+> prettyArgs as <> ")"

instance Pretty Type where
    pretty = cata a where
        a IntF                = "int"
        a StringF             = "string"
        a BoolF               = "bool"
        a VoidF               = "void"
        a NatF                = "nat"
        a CharF               = "char"
        a (NamedF n)          = string n
        a (ExF e t)           = pretty e <+> t
        a (DependentIntF e)   = "int(" <> pretty e <> ")"
        a (DependentBoolF e)  = "bool(" <> pretty e <> ")"
        a (DepStringF e)      = "string(" <> pretty e <> ")"
        a (DependentF n ts)   = pretty n <> parens (mconcat (punctuate ", " (fmap pretty (reverse ts))))
        a DoubleF             = "double"
        a FloatF              = "float"
        a (ForAF u t)         = pretty u <+> t
        a (UnconsumedF t)     = "!" <> parens t
        a (AsProofF t (Just t')) = t <+> ">>" <+> t'
        a (AsProofF t Nothing) = t <+> ">> _"
        a (FromVTF t)         = t <> "?!"
        a (MaybeValF t)       = t <> "?"
        a (T0pF ad)           = "t@ype" <> pretty ad
        a (Vt0pF ad)          = "vt@ype" <> pretty ad
        a (AtF _ t t')        = t <> "@" <> t'
        a (ProofTypeF _ t t') = parens (t <+> "|" <+> t')
        a (ConcreteTypeF e)   = pretty e
        a (TupleF _ ts)       = parens (mconcat (punctuate ", " (fmap pretty (reverse ts))))
        a (RefTypeF t)        = "&" <> t
        a (ViewTypeF _ t)     = "view@" <> parens t
        a (FunctionTypeF s t t') = t <+> string s <+> t'

gan :: Maybe Type -> Doc
gan (Just t) = " : " <> pretty t <> " "
gan Nothing  = ""

instance Pretty Existential where
    pretty (Existential [] Nothing (Just e)) = lbracket <+> pretty e <+> rbracket
    pretty (Existential bs ty Nothing)  = lbracket <+> mconcat (punctuate ", " (fmap pretty (reverse bs))) <> gan ty <+> rbracket
    pretty (Existential bs ty (Just e)) = lbracket <+> mconcat (punctuate ", " (fmap go (reverse bs))) <> gan ty <+> "|" <+> pretty e <+> rbracket
        where go (Arg s Nothing)  = pretty s
              go (Arg s (Just t)) = pretty s <+> colon <+> pretty t
              go _                = "FIXME" -- maybe use a new type? I don't think this should ever happen.

instance Pretty Universal where
    pretty (Universal [x] Nothing Nothing) = lbrace <> pretty x <> rbrace
    pretty (Universal bs ty Nothing) = lbrace <+> mconcat (punctuate ", " (fmap pretty (reverse bs))) <> gan ty <+> rbrace
    pretty (Universal bs ty (Just e)) = lbrace <+> mconcat (punctuate ", " (fmap go (reverse bs))) <> gan ty <+> "|" <+> pretty e <+> rbrace
        where go (Arg s Nothing)  = pretty s
              go (Arg s (Just t)) = pretty s <+> colon <+> pretty t
              go _                = "FIXME"

instance Pretty ATS where
    pretty (ATS xs) = concatSame xs

instance Pretty Implementation where
    pretty (Implement _ [] [] n [] e)  = "implement" <+> pretty n <+> "() =" <$> indent 2 (pretty e)
    pretty (Implement _ [] [] n ias e) = "implement" <+> pretty n <+> prettyArgs ias <+> "=" <$> indent 2 (pretty e)
    pretty (Implement _ [] us n ias e) = "implement" <+> pretty n <+> foldMap pretty us </> prettyArgs ias <+> "=" <$> indent 2 (pretty e)
    pretty (Implement _ ps [] n ias e) = "implement" <+> foldMap pretty ps </> pretty n <+> prettyArgs ias <+> "=" <$> indent 2 (pretty e)
    pretty (Implement _ ps us n ias e) = "implement" <+> foldMap pretty ps </> pretty n </> foldMap pretty us <+> prettyArgs ias <+> "=" <$> indent 2 (pretty e)

glue :: Declaration -> Declaration -> Bool
glue Staload{} Staload{}           = True
glue Include{} Include{}           = True
glue ViewTypeDef{} ViewTypeDef{}   = True
glue Val{} Val{}                   = True
glue Val{} Var{}                   = True
glue Var{} Val{}                   = True
glue Var{} Var{}                   = True
glue Comment{} _                   = True
glue (Func _ Fnx{}) (Func _ And{}) = True
glue _ _                           = False

{-# INLINE glue #-}

concatSame :: [Declaration] -> Doc
concatSame []  = mempty
concatSame [x] = pretty x
concatSame (x:x':xs)
    | glue x x' = pretty x <$> concatSame (x':xs)
    | otherwise = pretty x <> line <$> concatSame (x':xs)

-- TODO - soft break
($$) :: Doc -> Doc -> Doc
x $$ y = align (x <$> y)

prettyRecord :: (Pretty a) => [(String, a)] -> Doc
prettyRecord es = group (flatAlt (prettyRecordF True es) (prettyRecordS True es))

prettyRecordS :: (Pretty a) => Bool -> [(String, a)] -> Doc
prettyRecordS _ [] = mempty
prettyRecordS True [(s, t)] = "@{" <+> string s <+> "=" <+> pretty t <+> "}"
prettyRecordS _ [(s, t)] = "@{" <+> string s <+> "=" <+> pretty t
prettyRecordS True ((s, t):xs) = prettyRecordS False xs <> ("," <+> string s <+> "=" <+> pretty t <+> "}")
prettyRecordS x ((s, t):xs) = prettyRecordS x xs <> ("," <+> string s <+> "=" <+> pretty t)

prettyRecordF :: (Pretty a) => Bool -> [(String, a)] -> Doc
prettyRecordF _ [] = mempty
prettyRecordF True [(s, t)] = "@{" <+> string s <+> "=" <+> pretty t <+> "}"
prettyRecordF _ [(s, t)] = "@{" <+> string s <+> "=" <+> pretty t
prettyRecordF True ((s, t):xs) = prettyRecordF False xs $$ indent 1 ("," <+> string s <+> "=" <+> pretty t <$> "}")
prettyRecordF x ((s, t):xs) = prettyRecordF x xs $$ indent 1 ("," <+> string s <+> "=" <+> pretty t)

prettyDL :: [DataPropLeaf] -> Doc
prettyDL []                     = mempty
prettyDL [DataPropLeaf us e]    = indent 2 ("|" <+> foldMap pretty us <+> pretty e)
prettyDL (DataPropLeaf us e:xs) = prettyDL xs $$ indent 2 ("|" <+> foldMap pretty us <+> pretty e)

prettyLeaf :: [(String, Maybe Type)] -> Doc
prettyLeaf []                = mempty
prettyLeaf [(s, Nothing)]    = indent 2 ("|" <+> string s)
prettyLeaf [(s, Just e)]     = indent 2 ("|" <+> string s <+> "of" <+> pretty e)
prettyLeaf ((s, Nothing):xs) = prettyLeaf xs $$ indent 2 ("|" <+> string s)
prettyLeaf ((s, Just e):xs)  = prettyLeaf xs $$ indent 2 ("|" <+> string s <+> "of" <+> pretty e)

prettyArgsG :: Doc -> Doc -> [Doc] -> Doc
prettyArgsG c1 c2 = (c1 <>) . align . indent (-1) . cat . (<> pure c2) . go . reverse -- TODO when it's only one arg, don't split ( off
    where go :: [Doc] -> [Doc]
          go [x]    = [x]
          go (x:xs) = flatAlt (" " <> x) x : fmap (", " <>) xs
          go x      = x

prettyArgs' :: (Pretty a) => Doc -> Doc -> [a] -> Doc
prettyArgs' = fmap pretty -.** prettyArgsG

prettyArgs :: (Pretty a) => [a] -> Doc
prettyArgs = prettyArgs' "(" ")"

fancyU :: [Universal] -> Doc
fancyU = foldMap pretty . reverse

instance Pretty PreFunction where
    pretty (PreF i si [] [] as rt Nothing (Just e)) = pretty i <> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] [] as rt (Just t) (Just e)) = pretty i </> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] us as rt (Just t) (Just e)) = pretty i </> fancyU us </> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] us as rt Nothing (Just e)) = pretty i </> fancyU us </> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus [] as rt Nothing (Just e)) = fancyU pus </> pretty i <> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus [] as rt (Just t) (Just e)) = fancyU pus </> pretty i <+> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus us as rt (Just t) (Just e)) = fancyU pus </> pretty i </> fancyU us </> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus us as rt Nothing (Just e)) = fancyU pus </> pretty i </> fancyU us </> prettyArgs as <+> ":" <> string si </> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus us as rt Nothing Nothing) = fancyU pus </> pretty i </> fancyU us </> prettyArgs as <+> ":" <> string si </> pretty rt
    pretty _ = "FIXME"

instance Pretty DataPropLeaf where
    pretty (DataPropLeaf us e) = "|" <+> foldMap pretty (reverse us) <+> pretty e

instance Pretty Declaration where
    pretty (RecordType s [] rs)   = "typedef" <+> string s <+> "=" <+> prettyRecord rs
    pretty (RecordType s as rs)   = "typedef" <+> string s <> prettyArgs as <+> "=" <+> prettyRecord rs
    pretty (RecordViewType s [] rs) = "vtypedef" <+> string s <+> "=" <+> prettyRecord rs
    pretty (RecordViewType s as rs) = "vtypedef" <+> string s <> prettyArgs as <+> "=" <+> prettyRecord rs
    pretty (SumViewType s [] ls) = "datavtype" <+> string s <+> "=" <$> prettyLeaf ls
    pretty (SumViewType s as ls) = "datavtype" <+> string s <> prettyArgs as <+> "=" <$> prettyLeaf ls
    pretty (SumType s [] ls)     = "datatype" <+> string s <+> "=" <$> prettyLeaf ls
    pretty (SumType s as ls)     = "datatype" <+> string s <> prettyArgs as <+> "=" <$> prettyLeaf ls
    pretty (Impl [] i)           = pretty i
    pretty Impl{}                = "FIXME"
    pretty (PrVal p e)           = "prval" <+> pretty p <+> "=" <+> pretty e
    pretty (Val a Nothing p e)   = "val" <> pretty a <+> pretty p <+> "=" <+> pretty e
    pretty (Val a (Just t) p e)  = "val" <> pretty a <+> pretty p <> ":" <+> pretty t <+> "=" <+> pretty e
    pretty (Var Nothing p e)     = "var" <+> pretty p <+> "=" <+> pretty e
    pretty (Var (Just t) p e)    = "var" <+> pretty p <> ":" <+> pretty t <+> "=" <+> pretty e
    pretty (Include s)           = "#include" <+> pretty s
    pretty (Staload Nothing s)   = "staload" <+> pretty s
    pretty (Staload (Just q) s)  = "staload" <+> pretty q <+> "=" <+> pretty s
    pretty (CBlock s)            = string s
    pretty (Comment s)           = string s
    pretty (OverloadOp _ o n)    = "overload" <+> pretty o <+> "with" <+> pretty n
    pretty (Func _ (Fun pref))   = "fun" </> pretty pref
    pretty (Func _ (Fnx pref))   = "fnx" </> pretty pref
    pretty (Func _ (And pref))   = "and" </> pretty pref
    pretty (Func _ (Praxi pref)) = "praxi" </> pretty pref
    pretty (Extern _ d)          = "extern" <$> pretty d
    pretty (Define s)            = string s
    pretty (DataProp _ s as ls)  = "dataprop" <+> string s <> prettyArgs as <+> "=" <$> prettyDL ls
    pretty (ViewTypeDef _ s [] t) = "vtypedef" <+> string s <+> "=" <+> pretty t
    pretty (ViewTypeDef _ s as t) = "vtypedef" <+> string s <> prettyArgs as <+> "=" <+> pretty t
    pretty (TypeDef _ s [] t)    = "typedef" <+> string s <+> "=" <+> pretty t
    pretty (TypeDef _ s as t)    = "typedef" <+> string s <> prettyArgs as <+> "=" <+> pretty t
    pretty (AbsProp _ n as)      = "absprop" <+> string n <+> prettyArgs as
    pretty (Assume n as e)       = "assume" </> pretty n <> prettyArgs as <+> "=" </> pretty e
    pretty _                     = mempty
