{-# OPTIONS_GHC -fno-warn-orphans #-}

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
import           Data.Function                         (on)
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

instance Pretty Expression where
    pretty = cata a where
        a (IfF e e' e'')               = "if" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (SifF e e' e'')              = "sif" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (LetF _ e e')                = "let" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) e)) <$> "in" <$> indent 2 e' <$> "end" -- TODO soft linebreak?
        a (BoolLitF True)              = "true"
        a (BoolLitF False)             = "false"
        a (TimeLitF s)                 = string s
        a (IntLitF i)                  = pretty i
        a (LambdaF _ lt p e)          = "lam" <+> pretty p <+> pretty lt <+> e
        a (LinearLambdaF _ lt p e)    = "llam" <+> pretty p <+> pretty lt <+> e
        a (FloatLitF f)                = pretty f
        a (StringLitF s)               = dquotes (string s)
        a (BinaryF op e e')
            | splits op = e </> pretty op <+> e' -- TODO if one operator splits, the rest should.
            | otherwise = e <+> pretty op <+> e'
        a (IndexF _ n e)               = pretty n <> "[" <> e <> "]"
        a (UnaryF Negate e)            = "~" <> e
        a (NamedValF name)             = pretty name
        a (CallF name [] [] [])        = pretty name <> "()"
        a (CallF name [] [] xs)        = pretty name <> prettyArgsG "(" ")" xs
        a (CallF name [] ys [])        = pretty name <> prettyArgsG "<" ">" (fmap pretty ys)-- FIXME this should use { too?
        a (CallF name us [] [])        = pretty name <> prettyArgsG "{" "}" us
        a (CallF name [] ys xs)        = pretty name <> prettyArgsG "<" ">" (fmap pretty ys) <> prettyArgsG "(" ")" xs
        a (CallF name us [] xs)        = pretty name <> prettyArgsG "{" "}" us <> prettyArgsG "(" ")" xs
        a (CallF name us ys xs)        = pretty name <> prettyArgsG "{" "}" us <> prettyArgsG "<" ">" (fmap pretty ys) <> prettyArgsG "(" ")" xs
        a (CaseF _ add e cs)           = "case" <> pretty add <+> e <+> "of" <$> indent 2 (prettyCases cs)
        a (VoidLiteralF _)             = "()"
        a (RecordValueF _ es Nothing)  = prettyRecord es
        a (RecordValueF _ es (Just x)) = prettyRecord es <+> ":" <+> pretty x
        a (BeginF e)                   = "begin" <$> indent 2 (pretty e) <$> "end"
        a (PrecedeF e e')              = parens (e <+> ";" </> e')
        a (FieldMutateF _ o f v)       = pretty o <> "->" <> string f <+> ":=" <+> v
        a (DerefF _ e)                 = "!" <> e
        a (AccessF _ e n)
            | noParens e = e <> "." <> pretty n
            | otherwise = parens e <> "." <> pretty n
        a (CharLitF c)                 = "'" <> char c <> "'"
        a (RefF _ t e)                 = "ref<" <> pretty t <> ">" <> parens e
        a LocalF{}                     = "FIXME"
        a (ProofExprF _ e e')          = "(" <> e <+> "|" <+> e' <> ")"
        a (TypeSignatureF e t)         = e <+> ":" <+> pretty t
        a (WhereExpF e d)              = e <+> "where" <$> braces (" " <> pretty d <> " ")
        a (TupleExF _ es)              = parens (mconcat $ punctuate ", " es)
        a (WhileF _ e e')              = "while" <> parens e <> e'
        a (ActionsF as)                = "{" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) as)) <$> "}"
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
        a (UnconsumedF t)     = "!" <> t
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
        a (AbsPropF _ n as)   = "absprop" <+> string n <+> prettyArgs as

gan :: Maybe Type -> Doc
gan (Just t) = " : " <> pretty t <> " "
gan Nothing  = ""

instance Pretty Existential where
    pretty (Existential [] Nothing (Just e)) = lbracket <+> pretty e <+> rbracket
    pretty (Existential bs ty Nothing)  = lbracket <+> mconcat (punctuate ", " (fmap pretty (reverse bs))) <> gan ty <+> rbracket
    pretty (Existential bs ty (Just e)) = lbracket <+> mconcat (punctuate ", " (fmap go (reverse bs))) <> gan ty <+> "|" <+> pretty e <+> rbracket
        where go (Arg s Nothing)  = pretty s
              go (Arg s (Just t)) = pretty s <+> colon <+> pretty t

instance Pretty Universal where
    pretty (Universal [x] Nothing Nothing) = lbrace <> pretty x <> rbrace
    pretty (Universal bs ty Nothing) = lbrace <+> mconcat (punctuate ", " (fmap pretty (reverse bs))) <> gan ty <+> rbrace
    pretty (Universal bs ty (Just e)) = lbrace <+> mconcat (punctuate ", " (fmap go (reverse bs))) <> gan ty <+> "|" <+> pretty e <+> rbrace
        where go (Arg s Nothing)  = pretty s
              go (Arg s (Just t)) = pretty s <+> colon <+> pretty t

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
glue (Func _ Fnx{}) (Func _ And{}) = True
glue _ _                           = False

repulse :: Declaration -> Declaration -> Bool
repulse Impl{} Impl{}   = True
repulse Extern{} Impl{} = True
repulse _ Extern{}      = True
repulse _ _             = False

{-# INLINE glue #-}

concatSame :: [Declaration] -> Doc
concatSame []  = mempty
concatSame [x] = pretty x
concatSame (x:x':xs)
    | glue x x' = pretty x <> concatSame (x':xs)
    | repulse x x' = pretty x <> line <$> concatSame (x':xs)
    | otherwise = pretty x <$> concatSame (x':xs)

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
prettyArgs' = fmap pretty -.** prettyArgsG -- -.** fmap pretty

prettyArgs :: (Pretty a) => [a] -> Doc
prettyArgs = prettyArgs' "(" ")"

fancyU :: [Universal] -> Doc
fancyU = foldMap pretty . reverse

instance Pretty PreFunction where
    pretty (PreF i [] [] as rt Nothing (Just e)) = pretty i <> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i [] [] as rt (Just t) (Just e)) = pretty i </> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i [] us as rt (Just t) (Just e)) = pretty i </> fancyU us </> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i [] us as rt Nothing (Just e)) = pretty i </> fancyU us </> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i pus [] as rt Nothing (Just e)) = fancyU pus </> pretty i <> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i pus [] as rt (Just t) (Just e)) = fancyU pus </> pretty i <+> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i pus us as rt (Just t) (Just e)) = fancyU pus </> pretty i </> fancyU us </> ".<" <> pretty t <> ">." </> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i pus us as rt Nothing (Just e)) = fancyU pus </> pretty i </> fancyU us </> prettyArgs as <+> ":" <+> pretty rt <+> "=" <$> indent 2 (pretty e) <> line
    pretty (PreF i pus us as rt Nothing Nothing) = fancyU pus </> pretty i </> fancyU us </> prettyArgs as <+> ":" <+> pretty rt
    pretty _ = "FIXME"

instance Pretty DataPropLeaf where
    pretty (DataPropLeaf us e) = "|" <+> foldMap pretty (reverse us) <+> pretty e

instance Pretty Declaration where
    pretty (RecordType s rs)     = "typedef" <+> string s <+> "=" <+> prettyRecord rs <> line
    pretty (SumViewType s ls)    = "datavtype" <+> string s <+> "=" <$> prettyLeaf ls
    pretty (SumType s ls)        = "datatype" <+> string s <+> "=" <$> prettyLeaf ls
    pretty (Impl i)              = pretty i
    pretty (PrVal p e)           = "prval" <+> pretty p <+> "=" <+> pretty e
    pretty (Val a Nothing p e)   = "val" <> pretty a <+> pretty p <+> "=" <+> pretty e
    pretty (Val a (Just t) p e)  = "val" <> pretty a <+> pretty p <> ":" <+> pretty t <+> "=" <+> pretty e
    pretty (Var Nothing p e)     = "var" <+> pretty p <+> "=" <+> pretty e
    pretty (Var (Just t) p e)    = "var" <+> pretty p <> ":" <+> pretty t <+> "=" <+> pretty e
    pretty (Include s)           = "#include" <+> pretty s <> line
    pretty (Staload Nothing s)   = "staload" <+> pretty s <> line
    pretty (Staload (Just q) s)  = "staload" <+> pretty q <+> "=" <+> pretty s <> line
    pretty (CBlock s)            = string s <> line
    pretty (Comment s)           = string s
    pretty (OverloadOp _ o n)    = "overload" <+> pretty o <+> "with" <+> pretty n <> linebreak
    pretty (Func _ (Fun pref))   = "fun" </> pretty pref
    pretty (Func _ (Fnx pref))   = "fnx" </> pretty pref
    pretty (Func _ (And pref))   = "and" </> pretty pref
    pretty (Func _ (Praxi pref)) = "praxi" </> pretty pref
    pretty (Extern _ d)          = "extern" </> pretty d
    pretty (Define s)            = string s
    pretty (DataProp _ s as ls)  = "dataprop" <+> string s <> prettyArgs as <+> "=" <$> prettyDL ls
    pretty _                     = mempty
