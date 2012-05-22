module Halt.FOL.Style where

import Var
import Name
import Outputable
import Id

import Halt.FOL.Linearise

import Data.Char

strStyle :: Bool -> Style String String
strStyle cnf = Style
    { linVar  = text
    , linQVar = text
    , linApp  = text "app"
    , linMin  = text "min"
    , linCF   = text "cf"
    , linProj = \i n -> text (n ++ "_" ++ show i)
    , linPtr  = text . (++ "_ptr")
    , linCNF  = cnf
    , linConstant = text . show
    , linComments = True
    }

{-
varStyle :: Bool -> Style Var Var
varStyle cnf = Style
    { linVar  = text . prettyName . lower . showSDoc . ppr . maybeLocaliseName . idName
    , linQVar = text . capInit . showSDoc . ppr . maybeLocaliseName . idName
    , linApp  = text "app"
    , linMin  = text "min"
    , linCF   = text "cf"
    , linProj = \i -> text . prettyName . (++  "_" ++ show i) . lower . showSDoc . ppr . localiseName . idName
    , linPtr  = text . prettyName . (++ ".ptr") . showSDoc . ppr
    , linCNF  = cnf
    , linConstant = text . show
    , linComments = True
    }

axStyle :: Bool -> Style Int Var
axStyle cnf = Style
    { linVar  = text . prettyName . lower . showSDoc . ppr . maybeLocaliseName . idName
    , linQVar = text . ("X" ++) . show
    , linApp  = text "app"
    , linMin  = text "min"
    , linCF   = text "cf"
    , linProj = \i -> text . prettyName . (++  "_" ++ show i) . lower . showSDoc . ppr . localiseName . idName
    , linPtr  = text . prettyName . (++ ".ptr") . showSDoc . ppr
    , linCNF  = cnf
    , linConstant = text . show
    , linComments = True
    }

maybeLocaliseName :: Name -> Name
maybeLocaliseName n | isSystemName n = n
                    | otherwise      = localiseName n

capInit :: String -> String
capInit (x:xs) | isAlpha x = toUpper x : xs
               | otherwise = "Q" -- < this is just dumb
capInit "" = "Q"

lower :: String -> String
lower = map toLower
-}
