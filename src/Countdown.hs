-- Countdown: Chapter 9 of the Hutton book
--
-- This was the first chapter I tackled from the book, and I tried to use it
-- to really dig into quickcheck, hspec, and possible patterns around testing.
module Countdown
    ( valid
    , Op(..)
    , Expr(..)
    , eval
    , choices
    , isSolution
    , subsequences
    , values
    , allInserts
    , permutations
    , splits
    , exprs
    , solutions
    ) where


data Op = Add | Sub | Mul | Div
  deriving (Show, Eq)


valid :: Op -> Int -> Int -> Bool
valid Add _ _ = True
valid Mul _ _ = True
valid Sub x y = x > y
valid Div x y = x `mod` y == 0


data Expr = Val Int | Apply Op Expr Expr
  deriving (Show, Eq)


values :: Expr -> [Int]
values (Val x) = [x]
values (Apply _ a b) = (values a) ++ (values b)

apply :: Op -> Int -> Int -> Int
apply Add = (+)
apply Sub = (-)
apply Mul = (*)
apply Div = div


eval :: Expr -> Maybe Int
eval (Val n) = if n > 0 then (Just n) else Nothing
eval (Apply o l r) = do
  x <- eval l
  y <- eval r
  z <- eval $ Val $ apply o x y
  pure z


-- subsequences (in order)
subsequences :: [a] -> [[a]]
subsequences [] = [[]]
subsequences (x:xs) =
  ys ++ map (x:) ys
  where ys = subsequences xs

-- all posible inserts
allInserts :: a -> [a] -> [[a]]
allInserts x [] = [[x]]
allInserts x (y:ys) =
  (x:y:ys) : map (y:) (allInserts x ys)


permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat $ map (allInserts x) (permutations xs)


choices :: [a] -> [[a]]
choices = concat . map permutations . subsequences

isSolution :: Expr -> [Int] -> Int -> Bool
isSolution e xs n = elem (values e) (choices xs) && eval e == Just n


splits :: [a] -> [([a], [a])]
splits [] = []
splits [_] = []
splits (x:xs) = (([x], xs) : [((x:ls), rs) | (ls, rs) <- splits xs])


exprs :: [Int] -> [Expr]
exprs [] = []
exprs [n] = [Val n]
exprs xs = [
  (Apply o l r) | (ls, rs) <- splits xs,
                   l <- exprs ls,
                   r <- exprs rs,
                   o <- [Add, Sub, Mul, Div]]


solutions :: [Int] -> Int -> [Expr]
solutions xs n = [e | e <- exprs xs, eval e == Just n]


-- NOTE I stopped work on this section without finishing the part on trimming
-- the solution space, because I was satisfied with my introduction to testing
-- and I know that
-- (a) doing the same exercises with better data structures would be a better exercise
-- (b) most of the interesting ideas come up again later in the same book
