{-# LANGUAGE RecordWildCards #-}
module Main where

-- (c) Dan Rosén 2012
-- compile with
-- ghc -package ghc Main.hs

import BasicTypes
import CoreMonad
import CoreSubst (simpleOptExpr)
import CoreSyn
import DynFlags
import FloatOut
import GHC
import GHC.Paths
import HscTypes
import TysWiredIn
import Outputable
import UniqSupply

import Halt.Trans
import Halt.Lift
import Halt.Conf
import Halt.Monad

import Contracts.Make
import Contracts.Trans
import Contracts.Types

import FOL.LinTPTP

import Control.Monad
import System.Environment
import System.Exit

desugar :: Bool -> FilePath -> IO (ModGuts,DynFlags)
desugar debug_float_out targetFile =
  defaultErrorHandler defaultLogAction $
    {- defaultCleanupHandler defaultDynFlags $ -} do
      runGhc (Just libdir) $ do
        dflags <- getSessionDynFlags
        let dflags'
              | debug_float_out = foldl dopt_set dflags [Opt_D_dump_simpl_stats
                                                        ,Opt_D_verbose_core2core]
              | otherwise = dflags

        void $ setSessionDynFlags dflags'
        target <- guessTarget targetFile Nothing
        setTargets [target]
        void $ load LoadAllTargets
        modSum <- getModSummary (mkModuleName targetFile)
        p <- parseModule modSum
        t <- typecheckModule p
        d <- desugarModule t
        let modguts = dm_core_module d
        return (modguts,dflags')

lambdaLift :: DynFlags -> CoreProgram -> IO CoreProgram
lambdaLift dflags program = do
    us <- mkSplitUniqSupply 'l'
    floatOutwards float_switches dflags us (map simpleOpt program)
  where
    simpleOpt (NonRec v e) = NonRec v (simpleOptExpr e)
    simpleOpt (Rec vses)   = Rec [ (v,simpleOptExpr e) | (v,e) <- vses ]

    float_switches = FloatOutSwitches
                      { floatOutLambdas = Just 100
                      , floatOutConstants = False
                      , floatOutPartialApplications = True
                      }

main :: IO ()
main = do
    file:opts <- getArgs
    let flagged x = when (x `elem` opts)
    (modguts,dflags) <- desugar ("-debug-float-out" `elem` opts) file
    let core_binds = mg_binds modguts

    (program,m_stmts) <-
         if "-contracts" `elem` opts
             then do us <- mkSplitUniqSupply 'c'
                     let ((r,msgs),_us') = collectContracts us core_binds
                     flagged "-dbmkcontr" (mapM_ putStrLn msgs)
                     case r of
                          Right (stmts,bs) -> do flagged "-dbmkcontr" (mapM_ print stmts)
                                                 return (bs,Just stmts)
                          Left err         -> do putStrLn err
                                                 exitFailure
             else return (core_binds,Nothing)

    floated_prog <- lambdaLift dflags program
    us <- mkSplitUniqSupply 'f'

    let ty_cons    = mg_tcs modguts
        ty_cons_with_builtin :: [TyCon]
        ty_cons_with_builtin = listTyCon : boolTyCon : unitTyCon
                             : map (tupleTyCon BoxedTuple) [2..4]
                               -- ^ choice: only tuples up to 4 supported
                             ++ ty_cons
        halt_conf  = sanitizeConf $ HaltConf
                        { use_cnf      = "-cnf" `elem` opts
                        , inline_projs = True
                        , use_min      = "-no-min" `notElem` opts
                        , common_min   = "-common-min" `elem` opts
                        , unr_and_bad  = True
                        }
        ((lifted_prog,msgs_lift),_us) = caseLetLift floated_prog us
        halt_env          = mkEnv halt_conf ty_cons_with_builtin lifted_prog
        (tptp,msgs_trans) = translate halt_env ty_cons_with_builtin lifted_prog

        printSrc = do
            putStrLn $ "Original file, " ++ file ++ ":\n"
            putStrLn =<< readFile (file ++ ".hs")

        printMsgs msgs = unless (null msgs) $ putStrLn $ unlines msgs

        endl = putStrLn "\n"

        printCore msg core = do
            putStrLn $ msg ++ ":\n"
            mapM_ (printDump . ppr) core
            endl

    flagged "-src-before" printSrc

    flagged "-origcore" (printCore "Original core" core_binds)

    flagged "-lamlift" (printCore "Lambda lifted core" floated_prog)

    flagged "-dbcaseletlift" (printMsgs msgs_lift)
    flagged "-caseletlift"   (printCore "Case/let lifted core" lifted_prog)

    flagged "-src" printSrc

    flagged "-dbtptp" (printMsgs msgs_trans)

    case m_stmts of
        Nothing -> do
             unless ("-no-tptp" `elem` opts) (endl >> outputTPTP tptp >> endl)
        Just stmts -> forM_ stmts $ \stmt@(Statement{..}) -> do
             let (tr_contract,msgs_tr_contr) = runHaltM halt_env (trStatement stmt)
             flagged "-dbtrcontr" (printMsgs msgs_tr_contr)
             print statement_name
             outputTPTP (tr_contract)
             endl
             writeTPTP (show statement_name ++ ".tptp") (tptp ++ tr_contract)

