
module Main(main) where

import GHC
import GHC.Paths
import DynFlags
import Control.Exception
import System.Directory
import Control.Monad.IO.Class
import Control.Monad
import FastString
import System.Exit
import System.Process
import Data.List

data Mode = Processes -- Spawn ghc.exe processes
          | APIMake -- Use the GHC API to emulate --make
          | APISingle -- Use the GHC API to emulate -c then linking

mode = APISingle

flag = L (UnhelpfulSpan $ fsLit "ghc-process")

main :: IO ()
main = withCurDir "src" $
    defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
        liftIO $ do
            putStrLn "% Cleaning..."
            xs <- getDirectoryContents "."
            forM_ xs $ \x ->
                when (not ("." `isPrefixOf` x) && not (".hs" `isSuffixOf` x)) $
                    removeFile x

        case mode of
            Processes -> do
                let sys x = do putStrLn $ "% " ++ x ++ "..."; ExitSuccess <- system x; return ()
                sys "ghc -c Imported.hs"
                sys "ghc -c Root.hs"
                sys "ghc -o Root Root.o Imported.o"

            APIMake -> do
                runGhc (Just libdir) $ do
                    liftIO $ putStrLn $ "% Building everything..."
                    dflags <- getSessionDynFlags
                    setSessionDynFlags (gopt_set dflags Opt_Static){verbosity = 1}
                    target <- guessTarget "Root.hs" Nothing
                    setTargets [target]
                    load LoadAllTargets
                return ()

            APISingle -> do
                forM_ ["Imported.hs","Root.hs"] $ \file -> do
                    runGhc (Just libdir) $ do
                        liftIO $ putStrLn $ "% Compiling " ++ file ++ "..."
                        dflags <- getSessionDynFlags
                        setSessionDynFlags dflags{ghcMode=OneShot, hscTarget = HscAsm, ghcLink=NoLink, verbosity=1}
                        setTargets [Target (TargetFile file Nothing) True Nothing]
                        load LoadAllTargets

                runGhc (Just libdir) $ do
                    liftIO $ putStrLn "% Linking..."
                    dflags <- getSessionDynFlags
                    (dflags, [], []) <- parseDynamicFlags dflags $ take 0 [flag "Root.o", flag "Imported.o"]
                    dflags <- return $ dflags{ghcMode=OneShot, hscTarget = HscAsm, ghcLink=LinkBinary, verbosity=1}
                    dflags <- return $ gopt_set dflags Opt_Static
                    setSessionDynFlags dflags
                    setTargets [Target (TargetFile "Root.hs" Nothing) True Nothing]
                    load LoadAllTargets
                return ()


withCurDir :: FilePath -> IO a -> IO a
withCurDir dir act =
    bracket getCurrentDirectory setCurrentDirectory $ const $ do
        setCurrentDirectory dir; act
