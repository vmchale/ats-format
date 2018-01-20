{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Language.ATS.Exec ( exec
                         ) where

import Control.Arrow hiding ((<+>))
import           Control.Monad                (unless, (<=<))
import           Data.FileEmbed               (embedStringFile)
import qualified Data.HashMap.Lazy            as HM
import           Data.Maybe                   (fromMaybe)
import           Data.Monoid                  ((<>))
import qualified Data.Text.IO                 as TIO
import           Data.Version
import           Language.ATS
import           Options.Applicative
import           Paths_ats_format
import           System.Directory             (doesFileExist)
import           System.Exit                  (exitFailure)
import           System.IO                    (hPutStr, stderr)
import           Text.PrettyPrint.ANSI.Leijen (pretty)
import           Text.Toml
import           Text.Toml.Types              hiding (Parser)
import           System.Process                        (readCreateProcess, shell)

data Program = Program { _path :: Maybe FilePath, _inplace :: Bool, _noConfig :: Bool, _defaultConfig :: Bool }

takeBlock :: String -> (String, String)
takeBlock ('%':'}':ys) = ("", ('%':) . ('}':) $ ys)
takeBlock (y:ys)       = first (y:) $ takeBlock ys
takeBlock []           = ([], [])

rest :: String -> IO String
rest xs = fmap (<> (snd $ takeBlock xs)) $ printClang (fst $ takeBlock xs)

printClang :: String -> IO String
printClang = readCreateProcess (shell "clang-format")

processClang :: String -> IO String
processClang ('%':'{':'^':xs) = fmap (('%':) . ('{':) . ('^':)) $ rest xs
processClang ('%':'{':'#':xs) = fmap (('%':) . ('{':) . ('#':)) $ rest xs
processClang ('%':'{':'$':xs) = fmap (('%':) . ('{':) . ('$':)) $ rest xs
processClang ('%':'{':xs)     = fmap (('%':) . ('{':)) $ rest xs
processClang (x:xs)           = fmap (x:) $ processClang xs
processClang []               = pure []

file :: Parser Program
file = Program
    <$> optional (argument str
        (metavar "FILEPATH"
        <> completer (bashCompleter "file -X '!*.*ats' -o plusdirs")
        <> help "File path to ATS source."))
    <*> switch
        (short 'i'
        <> help "Modify file in-place")
    <*> switch
        (long "no-config"
        <> short 'o'
        <> help "Ignore configuration file")
    <*> switch
        (long "default-config"
        <> help "Generate default configuration file in the current directory")

versionInfo :: Parser (a -> a)
versionInfo = infoOption ("madlang version: " ++ showVersion version) (short 'V' <> long "version" <> help "Show version")

wrapper :: ParserInfo Program
wrapper = info (helper <*> versionInfo <*> file)
    (fullDesc
    <> progDesc "ATS source code formater. For more detailed help, see 'man atsfmt'"
    <> header "ats-format - a source code formatter written using happy/alex")

exec :: IO ()
exec = execParser wrapper >>= pick

printFail :: String -> IO a
printFail = const exitFailure <=< hPutStr stderr

defaultConfig :: FilePath -> IO ()
defaultConfig = flip writeFile $(embedStringFile ".atsfmt.toml")

asFloat :: Node -> Maybe Float
asFloat (VFloat d) = Just (realToFrac d)
asFloat _          = Nothing

asInt :: Node -> Maybe Int
asInt (VInteger i) = Just (fromIntegral i)
asInt _            = Nothing

asBool :: Node -> Maybe Bool
asBool (VBoolean True)  = Just True
asBool (VBoolean False) = Just False
asBool _                = Nothing

parseToml :: String -> IO (Float, Int, Bool)
parseToml p = do
    f <- TIO.readFile p
    case parseTomlDoc p f of
        Right x -> pure . fromMaybe (0.6, 120, False) $ do
            r <- asFloat =<< HM.lookup "ribbon" x
            w <- asInt =<< HM.lookup "width" x
            cf <- asBool =<< HM.lookup "clang-format" x
            pure (r, w, cf)
        Left e  -> printFail $ parseErrorPretty e

printCustom :: ATS -> IO String
printCustom ats = do
    let p = ".atsfmt.toml"
    config <- doesFileExist p
    if config then do
        (r, w, cf) <- parseToml p
        let t = printATSCustom r w ats
        if cf then
            processClang t
        else
            pure t
    else
        pure $ printATS ats

genErr :: Bool -> Either (ATSError String) ATS -> IO ()
genErr b = either (printFail . show . pretty) (putStrLn <=< go)
    where go = if not b then printCustom else pure . printATS

inplace :: FilePath -> (String -> IO String) -> IO ()
inplace p f = do
    contents <- readFile p
    newContents <- f contents
    unless (null newContents) $
        writeFile p newContents

fancyError :: Either (ATSError String) ATS -> IO ATS
fancyError = either (printFail . show . pretty) pure

pick :: Program -> IO ()
pick (Program (Just p) False nc _)  = (genErr nc . parseATS . lexATS) =<< readFile p
pick (Program Nothing _ nc False)   = (genErr nc . parseATS . lexATS) =<< getContents
pick (Program Nothing _ _ True)     = defaultConfig ".atsfmt.toml"
pick (Program (Just p) True True _) = inplace p (fmap printATS . fancyError . parseATS . lexATS)
pick (Program (Just p) True _ _)    = inplace p ((printCustom <=< fancyError) . parseATS . lexATS)
