{-# LANGUAGE ScopedTypeVariables, ParallelListComp,
             RankNTypes, ExistentialQuantification,
             ImpredicativeTypes, FlexibleContexts #-}
module Halt.FOL.Rename where

import Var
import Name
import Id
import Outputable

import Halt.FOL.Internals.Internals
import Halt.FOL.Operations
import Halt.FOL.Abstract

import Data.Bimap (Bimap)
import qualified Data.Bimap as B
import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S

import Data.Data
import Data.Char
import Data.Maybe

-- renameVarClauses :: [VarClause] -> [StrClause]
-- renameVarClauses = map (renameQVar suggest) . renameFuns
--   where
--     suggest :: Var -> [String]
--     suggest v = [ case s of
--                     x:xs | isAlpha x -> toUpper x:xs
--                          | otherwise -> 'Q':s
--                     []   -> 'Q':show i
--                 | s <- varSuggest v
--                 | i <- [(0 :: Int)..]
--                 ]
--
-- renameAxClauses :: [AxClause] -> [StrClause]
-- renameAxClauses = map (renameQVar suggest) . renameFuns
--   where
--     suggest _ = [ 'X':show i | i <- [(0 :: Int)..] ]

mkFunRenamer :: forall q . Data q =>
                [forall q' . (Data q',Data (Clause q' Var)) => Clause q' Var] ->
                Clause q Var -> Clause q String
mkFunRenamer clauses =

-- renameFuns :: Data q => [Clause q Var] -> [Clause q String]
-- renameFuns clauses =
    let symbols :: [Var]
        symbols = concatMap allSymbols clauses

        rep_map :: Map Var String
        rep_map = B.toMap (foldr (allot varSuggest protectedWiredIn)
                                 B.empty symbols)

        replace :: Var -> String
        replace v = case M.lookup v rep_map of
                        Just s  -> s
                        Nothing -> error $ "renameFuns: " ++ showSDoc (ppr v)
                                        ++ " not renamed!"

    in  clauseMapTerms (replaceVarsTm replace) id

renameQVar :: forall q . (Data q,Ord q)
            => (q -> [String])
            -> Clause q String -> Clause String String
renameQVar suggest clause =
    let symbols :: Set String
        symbols = S.fromList (allSymbols clause)

        quants :: [q]
        quants = allQuant clause

        rep_map :: Map q String
        rep_map = B.toMap (foldr (allot suggest
                                        (symbols `S.union` protectedWiredIn))
                                 B.empty quants)

        replace :: q -> String
        replace q = case M.lookup q rep_map of
                        Just s  -> s
                        Nothing -> error $ "renameQVar: elt not renamed!"

    in  clauseMapTerms (replaceQVarsTm replace) replace clause

allot :: Ord v => (v -> [String]) -> Set String -> v
               -> Bimap v String -> Bimap v String
allot suggest protected var bimap = B.insert var selected bimap
  where
    selected :: String
    selected = head [ cand
                    | cand <- suggest var
                    , cand `B.notMemberR` bimap
                    , cand `S.notMember` protected
                    ]

varSuggest :: Var -> [String]
varSuggest var = candidates
  where
    name :: Name
    name = idName var

    vars :: [Name]
    vars = [ localiseName name | not (isSystemName name) ] ++ [ name ]

    strs :: [String]
    strs = map (toTPTPid . showSDoc . ppr) vars

    fallback :: String
    fallback = last strs

    candidates :: [String]
    candidates = strs ++ [ fallback ++ "_" ++ show x | x <- [(0 :: Int)..] ]


toTPTPid s | Just x <- M.lookup s prelude = x
           | otherwise                    = escape (lower s)

escape :: String -> String
escape = concatMap (\c -> fromMaybe [c] (M.lookup c escapes))

lower :: String -> String
lower = map toLower

protectedWiredIn :: Set String
protectedWiredIn = S.fromList ["app","min","cf"]

escapes :: Map Char String
escapes = M.fromList
    [ ('\'',"prime")
    , ('!' ,"bang")
    , ('#' ,"hash")
    , ('$' ,"dollar")
    , ('%' ,"pc")
    , ('*' ,"star")
    , ('+' ,"plus")
    , ('.' ,"_")
    , ('/' ,"slash")
    , ('<' ,"less")
    , ('=' ,"equals")
    , ('>' ,"greater")
    , ('?' ,"quest")
    , ('\\',"bslash")
    , ('^' ,"hat")
    , ('|' ,"pipe")
    , (':' ,"colon")
    , ('-' ,"minus")
    , ('~' ,"tilde")
    , ('@' ,"at")
    ]


prelude :: Map String String
prelude = M.fromList
   [ ("[]","nil")
   , (":","cons")
   , ("()","unit")
   , ("(,)","tup")
   , ("(,,)","triple")
   , ("(,,,)","quad")
   , ("(,,,,)","quint")

   , ("++","append")
   , ("!!","index")

   , ("+","add")
   , ("-","sub")
   , ("/","div")
   , ("*","mul")
   , ("^","pow")
   , ("**","fpow")
   , ("^^","ipow")

   , (".","comp")
   , ("$","apply")

   , ("&&","or")
   , ("||","and")

   , ("==","eq")
   , ("/=","ne")

   , (">","gt")
   , ("<","lt")
   , (">=","ge")
   , ("<=","le")

   , (">>=","bind")
   , ("=<<","mapply")
   , (">>","then")
   , ("<<","after")

   , ("<$>","fmap")
   , ("<*>","ap")
   ]

