module Halt.Util (module Control.Arrow, module Control.Applicative) where

import Control.Arrow ((***),(&&&),first,second)
import Control.Applicative ((<$>),(<*>))

-- | Apply the function if true, otherwise propagate
(?) :: Bool -> (a -> a) -> a -> a
True  ? f = f
False ? _ = id

-- | Pair up a list with its previous and next elements
--
-- > selections "abc" = [("",'a',"bc"),("a",'b',"c"),("ab",'c',"")]
selections :: [a] -> [([a],a,[a])]
selections xs = map (fromSplit . (`splitAt` xs)) [0..length xs-1]
  where fromSplit (as,b:bs) = (as,b,bs)
        fromSplit _         = error "selections fromSplit unreachable"

-- | Pair up a list with the element and the rest of the elements
--
-- > inspect "abc" = [('a',"bc"),('b',"ac"),('c',"ab")]
inspect :: [a] -> [(a,[a])]
inspect = map (\(i,x,r) -> (x,i++r)) . selections

uniqueCartesian :: [a] -> [(a,a)]
uniqueCartesian as = concat [ zip (repeat x) xs | (x,xs) <- inspect as ]

-- | Pair up a list with its previous elements
--
-- > withPrevious "abc" = [('a',""),('b',"a"),('c',"ab")]
withPrevious :: [a] -> [(a,[a])]
withPrevious xs = zip xs (inits xs)

-- | If true, put in right, if false, put in left
putEither :: Bool -> a -> Either a a
putEither True  = Right
putEither False = Left

-- | concatMapM
concatMapM :: (Functor m,Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f = fmap concat . mapM f

-- | If any is nothing (unreachable branch etc), return nothing,
--   otherwise return just the catMaybes.
concatMaybe :: [Maybe [a]] -> Maybe [a]
concatMaybe = fmap concat . sequence

-- | Monadic if
mif :: Monad m => m Bool -> m a -> m a -> m a
mif mb mt mf = do
   b <- mb
   if b then mt else mf

nubSorted :: Ord a => [a] -> [a]
nubSorted = map head . group . sort

-- | Function composition deluxe
(.:) :: (b -> c) -> (a -> a' -> b) -> a -> a' -> c
(.:) = (.) . (.)

-- | Count the number of occurences satisfying the predicate
countBy :: (a -> Bool) -> [a] -> Int
countBy = length .: filter

groupSortedOn :: (Eq b,Ord b) => (a -> b) -> [a] -> [[a]]
groupSortedOn f = groupBy ((==) `on` f)
                . sortBy (comparing f)

color :: Color -> String -> String
color c s = fgcol c ++ s ++ normal

normal :: String
normal = "\ESC[0m"

bold :: String -> String
bold = ("\ESC[1m" ++)

fgcol :: Color -> String
fgcol col = "\ESC[0" ++ show (30+col2num col) ++ "m"

data Color = Red | Green | Blue | Pink | Yellow | Turquoise

col2num c = case c of
  Red       -> 1
  Green     -> 2
  Yellow    -> 3
  Blue      -> 4
  Pink      -> 5
  Turquoise -> 7


