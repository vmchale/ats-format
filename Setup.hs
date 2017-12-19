import           Control.Lens        ((<&>))
import           Control.Monad       (unless, void)
import           Data.Maybe
import           Distribution.Simple
import           System.Directory    (createDirectoryIfMissing)
import           System.Environment  (lookupEnv)
import           System.Process      (readCreateProcessWithExitCode, shell)

manpage :: IO String
manpage = readFile "man/atsfmt.1"

main :: IO ()
main = setManpath >>
    writeManpages >>
    writeBashCompletions >>
    defaultMain

setManpath :: IO ()
setManpath = do
    home <- lookupEnv "HOME"
    case home of
        Just x -> do
            let bashRc = x ++ "/.bashrc"
            config <- readFile bashRc
            unless (or (["#manpath updated by atsfmt", "#manpath updated by madlang"] <&> (`elem` lines config)))
                (appendFile bashRc "\n#manpath updated by atsfmt\nexport MANPATH=~/.local/share:$MANPATH\n" >>
                 void (readCreateProcessWithExitCode (shell $ "MANPATH=" ++ x ++ "/.local/share mandb") ""))
        Nothing -> pure ()

writeManpages :: IO ()
writeManpages = do
    home <- lookupEnv "HOME"
    case home of
        Just x -> do
            let manPath = x ++ "/.local/share/man/man1"
            createDirectoryIfMissing True manPath
            writeFile (manPath ++ "/atsfmt.1") =<< manpage
        Nothing -> pure ()

writeBashCompletions :: IO ()
writeBashCompletions = do
    home <- lookupEnv "HOME"
    case home of
        Just x -> do
            let bashRc = x ++ "/.bashrc"
            config <- readFile bashRc
            unless ("# Added by atsfmt" `elem` lines config)
                (appendFile bashRc "\n# Added by atsfmt\neval \"$(atsfmt --bash-completion-script atsfmt)\"\n")
        Nothing -> pure ()
