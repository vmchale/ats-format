{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.ATS.PrettyPrint ( printATS
                                , printATSCustom
                                , processClang
                                ) where

import           Control.Arrow                         hiding ((<+>))
import           Control.Composition                   hiding ((&))
import           Control.DeepSeq                       (NFData)
import           Control.Lens                          hiding (op, pre)
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

takeBlock :: String -> (String, String)
takeBlock ('%':'}':ys) = ("", ('%':) . ('}':) $ ys)
takeBlock (y:ys)       = first (y:) $ takeBlock ys
takeBlock []           = ([], [])

rest :: String -> IO String
rest xs = fmap (<> (snd $ takeBlock xs)) $ printClang (fst $ takeBlock xs)

processClang :: String -> IO String
processClang ('%':'{':'^':xs) = fmap (('%':) . ('{':) . ('^':)) $ rest xs
processClang ('%':'{':'#':xs) = fmap (('%':) . ('{':) . ('#':)) $ rest xs
processClang ('%':'{':'$':xs) = fmap (('%':) . ('{':) . ('$':)) $ rest xs
processClang ('%':'{':xs)     = fmap (('%':) . ('{':)) $ rest xs
processClang (x:xs)           = fmap (x:) $ processClang xs
processClang []               = pure []

printClang :: String -> IO String
printClang = readCreateProcess (shell "clang-format")

printATS :: ATS -> String
printATS (ATS x) = g mempty
    where g = (displayS . renderSmart 0.6 120 . (<> "\n") . pretty) (ATS $ reverse x)

printATSCustom :: Float -> Int -> ATS -> String
printATSCustom r i (ATS x) = g mempty
    where g = (displayS . renderSmart r i . pretty) (ATS $ reverse x)


instance Pretty Name where
    pretty (Unqualified n)   = text n
    pretty (Qualified _ i n) = "$" <> text n <> "." <> text i
    pretty (SpecialName _ s) = "$" <> text s
    pretty (Functorial s s') = text s <> "$" <> text s'
    pretty Unnamed{}         = mempty

instance Pretty LambdaType where
    pretty Plain{}    = "=>"
    pretty Spear{}    = "=>>"
    pretty (Full _ v) = "=<" <> text v <> ">"

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
    pretty StaticEq      = "=="
    pretty Mod           = "%"

splits :: BinOp -> Bool
splits Mult       = True
splits Add        = True
splits Div        = True
splits Sub        = True
splits LogicalAnd = True
splits LogicalOr  = True
splits _          = False

startsParens :: Doc -> Bool
startsParens d = f (show d) where
    f ('(':_) = True
    f _       = False

prettyBinary :: Doc -> [Doc] -> Doc
prettyBinary _ []       = mempty
prettyBinary _ [e]      = e
prettyBinary op [e, e'] = e <+> op <+> e'
prettyBinary _ _        = undefined

lengthAlt :: Doc -> Doc -> Doc
lengthAlt d1 d2
    | length (show d2) >= 40 = d1 <$> indent 4 d2
    | otherwise = d1 <+> d2

instance Pretty Expression where
    pretty = cata a . rewriteATS where
        a (IfF e e' (Just e''))         = "if" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (IfF e e' Nothing)            = "if" <+> e <+> "then" <$> indent 2 e'
        a (LetF _ e (Just e'))          = flatAlt
            ("let" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) e)) <$> "in" <$> indent 2 e' <$> "end")
            ("let" <+> pretty ((\(ATS x) -> ATS $ reverse x) e) <$> "in" <+> e' <$> "end")
        a (LetF _ e Nothing)            = flatAlt
            ("let" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) e)) <$> "in end")
            ("let" <+> pretty ((\(ATS x) -> ATS $ reverse x) e) <$> "in end")
        a (BoolLitF True)               = "true"
        a (BoolLitF False)              = "false"
        a (TimeLitF s)                  = text s
        a (IntLitF i)                   = pretty i
        a (LambdaF _ lt p e)            = let pre = "lam" <+> pretty p <+> pretty lt in flatAlt (lengthAlt pre e) (pre <+> e)
        a (LinearLambdaF _ lt p e)      = let pre = "llam" <+> pretty p <+> pretty lt in flatAlt (lengthAlt pre e) (pre <+> e)
        a (FloatLitF f)                 = pretty f
        a (StringLitF s)                = text s -- FIXME escape indentation in multi-line strings.
        a (ParenExprF _ e)              = parens e
        a (BinListF op@Add es)          = prettyBinary (pretty op) es
        a (BinaryF op e e')
            | splits op = e </> pretty op <+> e'
            | otherwise = e <+> pretty op <+> e'
        a (IndexF _ n e)                = pretty n <> "[" <> e <> "]"
        a (UnaryF Negate e)             = "~" <> e
        a (NamedValF nam)              = pretty nam
        a (CallF nam [] [] Nothing []) = pretty nam <> "()"
        a (CallF nam [] [] (Just e) xs) = pretty nam <> prettyArgsG ("(" <> pretty e <+> "| ") ")" xs -- FIXME split eagerly on "|"
        a (CallF nam [] [] Nothing xs) = pretty nam <> prettyArgsG "(" ")" xs
        a (CallF nam [] us Nothing []) = pretty nam <> prettyArgsU "{" "}" us
        a (CallF nam is [] Nothing []) = pretty nam <> prettyArgsU "<" ">" is
        a (CallF nam is [] Nothing [x])
            | startsParens x = pretty nam <> prettyArgsU "<" ">" is <> pretty x
        a (CallF nam is [] Nothing xs) = pretty nam <> prettyArgsU "<" ">" is <> prettyArgsG "(" ")" xs
        a (CaseF _ add e cs)            = "case" <> pretty add <+> e <+> "of" <$> indent 2 (prettyCases cs)
        a (VoidLiteralF _)              = "()"
        a (RecordValueF _ es Nothing)   = prettyRecord es
        a (RecordValueF _ es (Just x))  = prettyRecord es <+> ":" <+> pretty x
        a (PrecedeF e e')               = parens (e <+> ";" </> e')
        a (PrecedeListF es)             = lineAlt (prettyArgsList "; " "(" ")" es) ("(" <> mconcat (punctuate " ; " es) <> ")")
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
        a (ProofExprF _ e e')          = "(" <> e <+> "|" <+> e' <> ")"
        a (TypeSignatureF e t)         = e <+> ":" <+> pretty t
        a (WhereExpF e d)              = e <+> "where" <$> braces (" " <> nest 2 (pretty (ATS $ reverse d)) <> " ")
        a (TupleExF _ es)              = parens (mconcat $ punctuate ", " (reverse es))
        a (WhileF _ e e')              = "while" <> parens e <> e'
        a (ActionsF as)                = "{" <$> indent 2 (pretty ((\(ATS x) -> ATS $ reverse x) as)) <$> "}"
        a UnderscoreLitF{}             = "_"
        a (AtExprF e e')               = e <> "@" <> e'
        a (BeginF _ e)
            | not (startsParens e) = linebreak <> indent 2 ("begin" <$> indent 2 e <$> "end")
            | otherwise = e
        a (FixAtF (PreF n s [] [] as t Nothing (Just e))) = "fix@" <+> pretty n <+> prettyArgs as <+> ":" <> pretty s <+> pretty t <+> "=>" <$> indent 2 (pretty e)
        a (LambdaAtF (PreF Unnamed{} s [] [] as t Nothing (Just e))) = "lam@" <+> prettyArgs as <+> ":" <> pretty s <+> pretty t <+> "=>" <$> indent 2 (pretty e)
        a (AddrAtF _ e)                = "addr@" <> e
        a (ViewAtF _ e)                = "view@" <> e
        a (ListLiteralF _ s t es)      = "list" <> string s <> "{" <> pretty t <> "}" <> prettyArgs es
        a _                            = undefined
        prettyCases []              = mempty
        prettyCases [(s, l, t)]     = "|" <+> pretty s <+> pretty l <+> t
        prettyCases ((s, l, t): xs) = prettyCases xs $$ "|" <+> pretty s <+> pretty l <+> t -- FIXME can leave space with e.g. => \n begin ...

noParens :: Doc -> Bool
noParens = all (`notElem` ("()" :: String)) . show

patternHelper :: [Doc] -> Doc
patternHelper ps = mconcat (punctuate ", " (reverse ps))

instance Pretty Pattern where
    pretty = cata a where
        a (WildcardF _)      = "_"
        a (PSumF s x)        = string s <+> x
        a (PLiteralF e)      = pretty e
        a (PNameF s [])      = string s
        a (PNameF s [x])     = string s <> parens x
        a (PNameF s ps)      = string s <> parens (patternHelper ps)
        a (FreeF p)          = "~" <> p
        a (GuardedF _ e p)   = p <+> "when" <+> pretty e
        a (ProofF _ p p')    = parens (patternHelper p <+> "|" <+> patternHelper p')
        a (TuplePatternF ps) = parens (patternHelper ps)
        a (AtPatternF _ p)   = "@" <> p

instance Pretty Arg where
    pretty (Arg (First s))  = pretty s
    pretty (Arg (Second t)) = pretty t
    pretty (Arg (Both s t)) = pretty s <+> colon <+> pretty t
    pretty (PrfArg a a')    = pretty a <+> "|" <+> pretty a'
    pretty NoArgs           = undefined

squish :: BinOp -> Bool
squish Add  = True
squish Sub  = True
squish Mult = True
squish _    = False

instance Pretty StaticExpression where
    pretty = cata a where
        a (StaticValF n)            = pretty n
        a (StaticBinaryF op se se')
            | squish op = se <> pretty op <> se'
            | otherwise = se <+> pretty op <+> se'
        a (StaticIntF i)            = pretty i
        a StaticVoidF{}             = "()"
        a (SifF e e' e'')           = "sif" <+> e <+> "then" <$> indent 2 e' <$> "else" <$> indent 2 e''
        a (StaticBoolF True)        = "true"
        a (StaticBoolF False)       = "false"
        a (SCallF n cs)             = pretty n <> parens (mconcat (punctuate "," . reverse . fmap pretty $ cs))
        a (SPrecedeF e e')          = e <> ";" <+> e'

instance Pretty Type where
    pretty = cata a where
        a IntF                   = "int"
        a StringF                = "string"
        a BoolF                  = "bool"
        a VoidF                  = "void"
        a NatF                   = "nat"
        a AddrF                  = "addr"
        a CharF                  = "char"
        a (NamedF n)             = pretty n
        a (ExF e t)              = pretty e <+> t
        a (DependentIntF e)      = "int(" <> pretty e <> ")"
        a (DependentBoolF e)     = "bool(" <> pretty e <> ")"
        a (DepStringF e)         = "string(" <> pretty e <> ")"
        a (DependentF n ts)      = pretty n <> parens (mconcat (punctuate ", " (fmap pretty (reverse ts))))
        a DoubleF                = "double"
        a FloatF                 = "float"
        a (ForAF u t)            = pretty u <+> t
        a (UnconsumedF t)        = "!" <> t
        a (AsProofF t (Just t')) = t <+> ">>" <+> t'
        a (AsProofF t Nothing)   = t <+> ">> _"
        a (FromVTF t)            = t <> "?!"
        a (MaybeValF t)          = t <> "?"
        a (T0pF ad)              = "t@ype" <> pretty ad
        a (Vt0pF ad)             = "vt@ype" <> pretty ad
        a (AtF _ (Just t) t')    = t <+> "@" <+> t'
        a (AtF _ Nothing t)      = "@" <> t
        a (ProofTypeF _ t t')    = parens (t <+> "|" <+> t')
        a (ConcreteTypeF e)      = pretty e
        a (TupleF _ ts)          = parens (mconcat (punctuate ", " (fmap pretty (reverse ts))))
        a (RefTypeF t)           = "&" <> t
        a (ViewTypeF _ t)        = "view@" <> parens t
        a (FunctionTypeF s t t') = t <+> string s <+> t'
        a (ViewLiteralF c)       = "view" <> pretty c
        a NoneTypeF{}            = "()"
        a ImplicitTypeF{}        = ".."

gan :: Maybe Type -> Doc
gan (Just t) = " : " <> pretty t <> " "
gan Nothing  = ""

instance Pretty Existential where
    pretty (Existential [] Nothing (Just e)) = lbracket <+> pretty e <+> rbracket
    pretty (Existential bs ty Nothing)  = lbracket <+> mconcat (punctuate ", " (fmap pretty (reverse bs))) <> gan ty <+> rbracket
    pretty (Existential bs ty (Just e)) = lbracket <+> mconcat (punctuate ", " (fmap go (reverse bs))) <> gan ty <+> "|" <+> pretty e <+> rbracket
        where go (Arg (First s))  = pretty s
              go (Arg (Both s t)) = pretty s <+> colon <+> pretty t
              go (Arg (Second t)) = pretty t
              go _                = undefined

instance Pretty Universal where
    pretty (Universal [x@PrfArg{}] Nothing Nothing) = lbrace <+> pretty x <+> rbrace -- FIXME universals can now be length-one arguments
    pretty (Universal [x] Nothing Nothing) = lbrace <> pretty x <> rbrace -- FIXME universals can now be length-one arguments
    pretty (Universal bs ty Nothing) = lbrace <+> mconcat (punctuate ", " (fmap pretty (reverse bs))) <> gan ty <+> rbrace
    pretty (Universal bs ty (Just e)) = lbrace <+> mconcat (punctuate ", " (fmap go (reverse bs))) <> gan ty <+> "|" <+> pretty e <+> rbrace
        where go (Arg (First s))  = pretty s
              go (Arg (Both s t)) = pretty s <+> colon <+> pretty t
              go (Arg (Second t)) = pretty t
              go _                = undefined

instance Pretty ATS where
    pretty (ATS xs) = concatSame (fmap rewriteDecl xs)

instance Pretty Implementation where
    pretty (Implement _ [] [] n [] e)  = "implement" <+> pretty n <+> "() =" <$> indent 2 (pretty e)
    pretty (Implement _ [] [] n ias e) = "implement" <+> pretty n <+> prettyArgs ias <+> "=" <$> indent 2 (pretty e)
    pretty (Implement _ [] us n ias e) = "implement" <+> pretty n <+> foldMap pretty us </> prettyArgs ias <+> "=" <$> indent 2 (pretty e)
    pretty (Implement _ ps [] n ias e) = "implement" <+> foldMap pretty ps </> pretty n <+> prettyArgs ias <+> "=" <$> indent 2 (pretty e)
    pretty (Implement _ ps us n ias e) = "implement" <+> foldMap pretty ps </> pretty n </> foldMap pretty us <+> prettyArgs ias <+> "=" <$> indent 2 (pretty e)

isVal :: Declaration -> Bool
isVal Val{}   = True
isVal Var{}   = True
isVal PrVal{} = True
isVal _       = False

glue :: Declaration -> Declaration -> Bool
glue x y
    | isVal x && isVal y = True
glue Staload{} Staload{}           = True
glue Include{} Include{}           = True
glue ViewTypeDef{} ViewTypeDef{}   = True
glue TypeDef{} TypeDef{}           = True
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

lineAlt :: Doc -> Doc -> Doc
lineAlt = group .* flatAlt

prettyRecord :: (Pretty a) => [(String, a)] -> Doc
prettyRecord es = lineAlt (prettyRecordF True es) (prettyRecordS True es)

prettyRecordS :: (Pretty a) => Bool -> [(String, a)] -> Doc
prettyRecordS _ []             = mempty
prettyRecordS True [(s, t)]    = "@{" <+> text s <+> "=" <+> pretty t <+> "}"
prettyRecordS _ [(s, t)]       = "@{" <+> text s <+> "=" <+> pretty t
prettyRecordS True ((s, t):xs) = prettyRecordS False xs <> ("," <+> text s <+> "=" <+> pretty t <+> "}")
prettyRecordS x ((s, t):xs)    = prettyRecordS x xs <> ("," <+> text s <+> "=" <+> pretty t)

prettyRecordF :: (Pretty a) => Bool -> [(String, a)] -> Doc
prettyRecordF _ []             = mempty
prettyRecordF True [(s, t)]    = "@{" <+> text s <+> "=" <+> pretty t <+> "}"
prettyRecordF _ [(s, t)]       = "@{" <+> text s <+> "=" <+> pretty t
prettyRecordF True ((s, t):xs) = prettyRecordF False xs $$ indent 1 ("," <+> text s <+> "=" <+> pretty t <$> "}")
prettyRecordF x ((s, t):xs)    = prettyRecordF x xs $$ indent 1 ("," <+> text s <+> "=" <+> pretty t)

prettyDL :: [DataPropLeaf] -> Doc
prettyDL []                               = mempty
prettyDL [DataPropLeaf [] e Nothing]      = indent 2 ("|" <+> pretty e)
prettyDL [DataPropLeaf [] e (Just e')]    = indent 2 ("|" <+> pretty e <+> "of" <+> pretty e')
prettyDL (DataPropLeaf [] e Nothing:xs)   = prettyDL xs $$ indent 2 ("|" <+> pretty e)
prettyDL (DataPropLeaf [] e (Just e'):xs) = prettyDL xs $$ indent 2 ("|" <+> pretty e <+> "of" <+> pretty e')
prettyDL [DataPropLeaf us e Nothing]      = indent 2 ("|" <+> foldMap pretty us <+> pretty e)
prettyDL [DataPropLeaf us e (Just e')]    = indent 2 ("|" <+> foldMap pretty us <+> pretty e <+> "of" <+> pretty e')
prettyDL (DataPropLeaf us e Nothing:xs)   = prettyDL xs $$ indent 2 ("|" <+> foldMap pretty us <+> pretty e)
prettyDL (DataPropLeaf us e (Just e'):xs) = prettyDL xs $$ indent 2 ("|" <+> foldMap pretty us <+> pretty e <+> "of" <+> pretty e')

universalHelper :: [Universal] -> Doc
universalHelper = mconcat . fmap pretty . reverse

prettyLeaf :: [Leaf] -> Doc
prettyLeaf []                         = mempty
prettyLeaf [Leaf [] s [] Nothing]     = indent 2 ("|" <+> text s)
prettyLeaf [Leaf [] s [] (Just e)]    = indent 2 ("|" <+> text s <+> "of" <+> pretty e)
prettyLeaf (Leaf [] s [] Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> text s)
prettyLeaf (Leaf [] s [] (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> text s <+> "of" <+> pretty e)
prettyLeaf [Leaf [] s as Nothing]     = indent 2 ("|" <+> text s <> prettyArgs as)
prettyLeaf [Leaf [] s as (Just e)]    = indent 2 ("|" <+> text s <> prettyArgs as <+> "of" <+> pretty e)
prettyLeaf (Leaf [] s as Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> text s <> prettyArgs as)
prettyLeaf (Leaf [] s as (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> text s <> prettyArgs as <+> "of" <+> pretty e)
prettyLeaf [Leaf us s [] Nothing]     = indent 2 ("|" <+> universalHelper us <+> text s)
prettyLeaf [Leaf us s [] (Just e)]    = indent 2 ("|" <+> universalHelper us <+> text s <+> "of" <+> pretty e)
prettyLeaf (Leaf us s [] Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> universalHelper us <+> text s)
prettyLeaf (Leaf us s [] (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> universalHelper us <+> text s <+> "of" <+> pretty e)
prettyLeaf [Leaf us s as Nothing]     = indent 2 ("|" <+> universalHelper us <+> text s <> prettyArgs as)
prettyLeaf [Leaf us s as (Just e)]    = indent 2 ("|" <+> universalHelper us <+> text s <> prettyArgs as <+> "of" <+> pretty e)
prettyLeaf (Leaf us s as Nothing:xs)  = prettyLeaf xs $$ indent 2 ("|" <+> universalHelper us <+> text s <> prettyArgs as)
prettyLeaf (Leaf us s as (Just e):xs) = prettyLeaf xs $$ indent 2 ("|" <+> universalHelper us <+> text s <> prettyArgs as <+> "of" <+> pretty e)

prettyHelper :: Doc -> [Doc] -> [Doc]
prettyHelper _ [x]    = [x]
prettyHelper c (x:xs) = flatAlt (" " <> x) x : fmap (c <>) xs
prettyHelper _ x      = x

prettyBody :: Doc -> Doc -> [Doc] -> Doc
prettyBody c1 c2 [d] = c1 <> d <> c2
prettyBody c1 c2 ds  = (c1 <>) . align . indent (-1) . cat . (<> pure c2) $ ds

prettyArgsG' :: Doc -> Doc -> Doc -> [Doc] -> Doc
prettyArgsG' c3 c1 c2 = prettyBody c1 c2 . prettyHelper c3 . reverse

prettyArgsList :: Doc -> Doc -> Doc -> [Doc] -> Doc
prettyArgsList c3 c1 c2 = prettyBody c1 c2 . va . prettyHelper c3

va :: [Doc] -> [Doc]
va = (& _tail.traverse %~ group)

prettyArgsG :: Doc -> Doc -> [Doc] -> Doc
prettyArgsG = prettyArgsG' ", "

prettyArgsU :: (Pretty a) => Doc -> Doc -> [a] -> Doc
prettyArgsU = prettyArgs' ","

prettyArgs' :: (Pretty a) => Doc -> Doc -> Doc -> [a] -> Doc
prettyArgs' = fmap pretty -.*** prettyArgsG'

prettyArgs :: (Pretty a) => [a] -> Doc
prettyArgs = prettyArgs' ", " "(" ")"

fancyU :: [Universal] -> Doc
fancyU = foldMap pretty . reverse

(<#>) :: Doc -> Doc -> Doc
(<#>) a b = lineAlt (a <$> indent 2 b) (a <+> b)

-- FIXME figure out a nicer algorithm for when/how to split lines.
instance Pretty PreFunction where
    pretty (PreF i si [] [] [NoArgs] rt Nothing (Just e)) = pretty i <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e) -- FIXME this is an awful hack
    pretty (PreF i si [] [] as rt Nothing (Just e)) = pretty i <> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] [] as rt (Just t) (Just e)) = pretty i </> ".<" <> pretty t <> ">." <#> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] us as rt (Just t) (Just e)) = pretty i </> fancyU us </> ".<" <> pretty t <> ">." <#> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] us [NoArgs] rt Nothing (Just e)) = pretty i </> fancyU us <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] us as rt Nothing (Just e)) = pretty i </> fancyU us <#> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus [] as rt Nothing (Just e)) = fancyU pus </> pretty i <> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus [] as rt (Just t) (Just e)) = fancyU pus </> pretty i <+> ".<" <> pretty t <> ">." <#> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus us as rt (Just t) (Just e)) = fancyU pus </> pretty i </> fancyU us </> ".<" <> pretty t <> ">." <#> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si pus us as rt Nothing (Just e)) = fancyU pus </> pretty i </> fancyU us <#> prettyArgs as <+> ":" <> text si <#> pretty rt <+> "=" <$> indent 2 (pretty e)
    pretty (PreF i si [] [] as rt Nothing Nothing) = pretty i <> prettyArgs as <+> ":" <> text si <#> pretty rt
    pretty (PreF i si [] us [] rt Nothing Nothing) = pretty i </> fancyU us <+> ":" <> text si <#> pretty rt
    pretty (PreF i si [] us as rt Nothing Nothing) = pretty i </> fancyU us <#> prettyArgs as <+> ":" <> text si <#> pretty rt
    pretty (PreF i si pus us as rt Nothing Nothing) = fancyU pus </> pretty i </> fancyU us </> prettyArgs as <+> ":" <> text si <#> pretty rt
    pretty _ = undefined

instance Pretty DataPropLeaf where
    pretty (DataPropLeaf us e Nothing)   = "|" <+> foldMap pretty (reverse us) <+> pretty e
    pretty (DataPropLeaf us e (Just e')) = "|" <+> foldMap pretty (reverse us) <+> pretty e <+> "of" <+> pretty e'

typeHelper :: [(String, Type)] -> Doc
typeHelper = ("=" <#>) . prettyRecord

instance Pretty Declaration where
    pretty (AbsType _ s as Nothing)     = "abstype" <+> text s <> prettyArgs as
    pretty (AbsViewType _ s as Nothing) = "absvtype" <+> text s <> prettyArgs as
    pretty (RecordType s [] [] rs)      = "typedef" <+> text s <+> "=" <+> prettyRecord rs
    pretty (RecordType s as [] rs)      = "typedef" <+> text s <> prettyArgs as <+> "=" <+> prettyRecord rs
    pretty (RecordViewType s [] [] rs)  = "vtypedef" <+> text s <+> "=" </> prettyRecord rs
    pretty (RecordViewType s as [] rs)  = "vtypedef" <+> text s <> prettyArgs as <+> typeHelper rs
    pretty (RecordViewType s as us rs)  = "vtypedef" <+> text s <> prettyArgs as <+> "=" </> fancyU us </> prettyRecord rs
    pretty (SumViewType s [] ls)        = "datavtype" <+> text s <+> "=" <$> prettyLeaf ls
    pretty (SumViewType s as ls)        = "datavtype" <+> text s <> prettyArgs as <+> "=" <$> prettyLeaf ls
    pretty (SumType s [] ls)            = "datatype" <+> text s <+> "=" <$> prettyLeaf ls
    pretty (SumType s as ls)            = "datatype" <+> text s <> prettyArgs as <+> "=" <$> prettyLeaf ls
    pretty (Impl [] i)                  = pretty i
    pretty (PrVal p e)                  = "prval" <+> pretty p <+> "=" <+> pretty e
    pretty (Val a Nothing p e)          = "val" <> pretty a <+> pretty p <+> "=" <+> pretty e
    pretty (Val a (Just t) p e)         = "val" <> pretty a <+> pretty p <> ":" <+> pretty t <+> "=" <+> pretty e
    pretty (Var (Just t) p Nothing e)   = "var" <+> pretty p <> ":" <+> pretty t <+> "with" <+> pretty e
    pretty (Var Nothing p e Nothing)    = "var" <+> pretty p <+> "=" <+> pretty e
    pretty (Var (Just t) p e Nothing)   = "var" <+> pretty p <> ":" <+> pretty t <+> "=" <+> pretty e
    pretty (Include s)                  = "#include" <+> pretty s
    pretty (Staload Nothing s)          = "staload" <+> pretty s
    pretty (Staload (Just q) s)         = "staload" <+> pretty q <+> "=" <+> pretty s
    pretty (CBlock s)                   = string s
    pretty (Comment s)                  = string s
    pretty (OverloadOp _ o n)           = "overload" <+> pretty o <+> "with" <+> pretty n
    pretty (OverloadIdent _ i n)        = "overload" <+> text i <+> "with" <+> pretty n
    -- We use 'text' here, which means indentation might get fucked up for
    -- C preprocessor macros, but you absolutely deserve it if you indent your
    -- macros.
    pretty (Define s)                   = text s
    pretty (Func _ (Fn pref))           = "fn" </> pretty pref
    pretty (Func _ (Fun pref))          = "fun" </> pretty pref
    pretty (Func _ (CastFn pref))       = "castfn" </> pretty pref
    pretty (Func _ (Fnx pref))          = "fnx" </> pretty pref
    pretty (Func _ (And pref))          = "and" </> pretty pref
    pretty (Func _ (Praxi pref))        = "praxi" </> pretty pref
    pretty (Func _ (PrFun pref))        = "prfun" </> pretty pref
    pretty (Func _ (PrFn pref))         = "prfn" </> pretty pref
    pretty (Extern _ d)                 = "extern" <$> pretty d
    pretty (DataProp _ s as ls)         = "dataprop" <+> text s <> prettyArgs as <+> "=" <$> prettyDL ls
    pretty (ViewTypeDef _ s [] t)       = "vtypedef" <+> text s <+> "=" </> pretty t
    pretty (ViewTypeDef _ s as t)       = "vtypedef" <+> text s <> prettyArgs as <+> "=" </> pretty t
    pretty (TypeDef _ s [] t)           = "typedef" <+> text s <+> "=" <+> pretty t
    pretty (TypeDef _ s as t)           = "typedef" <+> text s <> prettyArgs as <+> "=" <+> pretty t
    pretty (AbsProp _ n as)             = "absprop" <+> text n <+> prettyArgs as
    pretty (Assume n as e)              = "assume" </> pretty n <> prettyArgs as <+> "=" </> pretty e
    pretty (SymIntr _ n)                = "symintr" <+> pretty n
    pretty (Stacst _ n t Nothing)       = "stacst" </> pretty n <+> ":" </> pretty t
    pretty (Stacst _ n t (Just e))      = "stacst" </> pretty n <+> ":" </> pretty t <+> "=" </> pretty e
    pretty (PropDef _ s as t)           = "propdef" </> text s <+> prettyArgs as <+> "=" </> pretty t
    pretty _                            = undefined
