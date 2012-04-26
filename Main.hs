module Main where

-- (c) Dan Rosén 2012
-- compile with
-- ghc -package ghc Main.hs

import GHC
import Outputable
import GHC.Paths
import DynFlags
import HscTypes
import SimplCore

import CoreSyn
import FloatOut
import UniqSupply
import CoreMonad

import Halt.Trans
import FOL.Pretty

import Control.Monad
import System.Environment

desugar :: FilePath -> IO (ModGuts,[CoreBind])
desugar targetFile =
  defaultErrorHandler defaultLogAction $
    {- defaultCleanupHandler defaultDynFlags $ -} do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags' = foldl dopt_set dflags
                            [Opt_CaseMerge,Opt_FloatIn
                            ,Opt_CSE,Opt_DoEtaReduction
                            ,Opt_StaticArgumentTransformation
                            ]
        void $ setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        void $ load LoadAllTargets
        modSum <- getModSummary (mkModuleName targetFile)
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        let modguts = dm_core_module d
        s <- getSession
        modguts' <- liftIO (core2core s modguts)
        let coreBinds = mg_binds modguts'
            float_switches = FloatOutSwitches
                               { floatOutLambdas = Just 100
                               , floatOutConstants = False
                               , floatOutPartialApplications = False
                               }
        us <- liftIO (mkSplitUniqSupply 'l')
           -- ^ Make a UniqSupply out of thin air. Trying char 'l'
        floatedProg <- liftIO (floatOutwards float_switches dflags' us coreBinds)
        return (modguts',floatedProg)

main :: IO ()
main = do
  [file] <- getArgs
  (modguts,floated_prog) <- desugar file
  let core_binds = mg_binds modguts
      ty_cons    = mg_tcs modguts
  putStrLn "************************"
  putStrLn "desugared:\n"
  mapM_ (printDump . ppr) core_binds
  putStrLn "************************"
  putStrLn "let-lifted:\n"
  mapM_ (printDump . ppr) floated_prog
  let (tptp,msgs) = translate ty_cons floated_prog
  putStrLn "************************"
  unless (null msgs) $ putStrLn $ "msgs:\n" ++ unlines msgs ++ "\n"
  putStrLn "tptp:\n"
  outputTPTP tptp
