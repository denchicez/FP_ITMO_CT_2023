module HW1.T3
  ( Tree (..)
  , tsize
  , tdepth
  , tmember
  , tinsert
  , tFromList
  ) where

data Meta = M Int Int
  deriving (Show)

data Tree a = Leaf | Branch Meta (Tree a) a (Tree a)
  deriving (Show)


tsize :: Tree a -> Int -- O(1)
tsize Leaf = 0
tsize (Branch (M sz _) _ _ _) = sz

tdepth :: Tree a -> Int -- O(1)
tdepth Leaf = 0
tdepth (Branch (M _ depth) _ _ _) = depth

mkBranch :: Tree a -> Tree a -- O(1)
mkBranch Leaf = Leaf
mkBranch (Branch (M sz depth) left val right) = Branch (M ((tsize left) + (tsize right) + 1) ((max (tdepth left) (tdepth right)) + 1)) left val right

tmember :: Ord a => a -> Tree a -> Bool -- O(height)
tmember _ Leaf = False
tmember x (Branch _ left val right)
    | x < val = tmember x left
    | x > val = tmember x right
    | otherwise = True

tinsert :: Ord a => a -> Tree a -> Tree a -- O(height)
tinsert x Leaf = Branch (M 1 1) Leaf x Leaf
tinsert x (Branch (M sz depth) left val right)
    | x < val = mkBranch (Branch (M sz depth) (tinsert x left) val right)
    | x > val = mkBranch (Branch (M sz depth) left val (tinsert x right))
    | otherwise = mkBranch (Branch (M sz depth) left val right)

tFromList :: Ord a => [a] -> Tree a -- O(n * height)
tFromList [] = Leaf
tFromList (first : a) = tinsert first (tFromList a)
