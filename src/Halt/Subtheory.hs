{-# LANGUAGE RecordWildCards #-}
module Halt.Subtheory where

import Halt.Util
import Halt.FOL.Abstract hiding (Lemma)
import qualified Halt.FOL.Abstract as A

import Var
import TyCon

data Content
    = Function [Var]
    -- ^ A group of mutually recursive definitions
    | Pointer Var
    -- ^ The pointer to a definition
    | Data TyCon
    -- ^ Discrimination and projection axioms for a data type
    | CrashFree TyCon
    -- ^ [contracts] CF predicates for a data type
    | PrimConAxioms
    -- ^ [contracts] Axioms about UNR and BAD
    | Typing TyCon
    -- ^ [hipspec] Type predicates for a data type
    | Lemma String [Var]
    -- ^ [hipspec] Lemma with a name, regarding a group of definitions

data Subtheory
    = Subtheory { provides    :: Content
                -- ^ Content defined
                , depends     :: [Content]
                -- ^ Content depending upon
                , description :: String
                -- ^ Commentary
                , formulae    :: [Formula']
                -- ^ Formulae in this subtheory
                }

toClauses :: Subtheory -> [Clause']
toClauses (Subtheory{..}) =
    (description /= "" ? (comment description:))
    (map (namedClause name cltype) formulae)
  where
    name   = case provides of
                 Lemma s _ -> s
                 _         -> "_"

    cltype = case provides of
                 Lemma{}    -> A.Lemma
                 Function{} -> A.Definition
                 _          -> A.Axiom
