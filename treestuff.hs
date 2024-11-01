-- data types for binary trees + one-hole contexts and zippers, as well
-- as some pretty-printing routines

module Treestuff where

import Data.Tree
import Data.List (sort)
import System.Random (randomRIO)

--define a labelled binary tree
data LabBin a = Ll a | Bl a (LabBin a) (LabBin a)
  deriving (Show,Eq)

--define a modified context that takes labels into account
data BinCxt a = Hole
              | B0 a (BinCxt a) (LabBin a)
              | B1 a (LabBin a) (BinCxt a)
  deriving (Show,Eq)

--similarly, plug is modified
plug :: BinCxt a -> LabBin a -> LabBin a
plug Hole      t = t
plug (B0 l c t2) t = plug c (Bl l t t2)
plug (B1 l t1 c) t = plug c (Bl l t1 t)

--create a random tree with MAXIMUM depth of n
--creating random trees handled \/
randomTree :: Int -> IO (LabBin Int)
randomTree 0 = do
  let x = 0
  return (Ll x)
randomTree n = do
    --we can change the branching probability to make it more probable to reach deeper trees
    branch <- (randomRIO (0, 3) :: IO Int)
    if branch == 0 
        then do 
            let x = 0
            return (Ll x)
        else do
            t1 <- randomTree (n-1)
            t2 <- randomTree (n-1)
            let x = 0
            return (Bl x t1 t2)

--counts number of nodes for a tree
countNodes :: LabBin a -> Int
countNodes (Ll val) = 1
countNodes (Bl val left right) = 1 + countNodes left + countNodes right

--label a tree given a list of labels (in preorder)
intractPreOrd :: LabBin a -> [a] -> LabBin a
intractPreOrd (Ll _) x = (Ll (x!!0))
intractPreOrd (Bl _ left right) x = 
    let aux = countNodes left 
    in  let xright = drop (aux + 1) x
        in let r = intractPreOrd right xright
            in
            (Bl (x!!0) (intractPreOrd left (tail x)) r)

--given a list [res], prepends it with a random number in range [lower, upper], such that the number
--doesnt already exist in [res]. n is an auxilliary counter as to how many times the function recurses. 
--params: lower / upper / n / res
toGenerate :: Int -> Int -> Int -> [Int] -> IO [Int]
toGenerate lower upper 0 res = do return res
toGenerate lower upper n res = do
    num <- randomRIO(lower, upper)
    if num `elem` res
        then (toGenerate lower upper n res)
        else (toGenerate lower upper (n - 1) (num : res))

--generate a list of length n of random numbers in range [lower upper] without repetitions
--params: lower / upper / n
randomIndicesNoRep :: Int -> Int -> Int -> IO [Int]
randomIndicesNoRep lower upper n = toGenerate lower upper n []

--given two lists that contains indices of the result list that should be labelled, do this labeling
--example: l1 = [1,2,3] l2 = [4,5,7] and n = 8 then res = [0,1,1,1,2,2,0,2]
--params i / n / l1 / l2
assignLabels :: Int -> Int -> [Int] -> [Int] -> IO [Int]
assignLabels i n [] [] = 
    if i < n 
        then do 
            res <- assignLabels (i + 1) n [] []
            return (0 : res)
    else do return []
assignLabels i n l1 [] = 
    if head l1 == i 
        then do 
        res <- assignLabels (i + 1) n (tail l1) []
        return (1 : res)
    else do
        res <- assignLabels (i + 1) n l1 []
        return (0 : res)
assignLabels i n [] l2 = 
    if head l2 == i then do
        res <- assignLabels (i + 1) n [] (tail l2)
        return (2 : res)
    else do
        res <- assignLabels (i + 1) n [] l2
        return (0 : res)
assignLabels i n l1 l2 = 
    if head l1 == i then do
        res <- assignLabels (i + 1) n (tail l1) l2
        return (1 : res)
    else if head l2 == i then do
        res <- assignLabels (i + 1) n l1 (tail l2)
        return (2 : res)
    else do
        res <- assignLabels (i + 1) n l1 l2
        return (0 : res)

--generate a random list with labels
--params: n (length) x (number of non-neutral labels of each category)
randomLabels :: Int -> Int -> IO [Int]
randomLabels n x = do
    labeling <- randomIndicesNoRep 0 (n-1) (2*x)
    let (l1, l2) = splitAt (length labeling `div` 2) labeling
    let l1sorted = sort l1
    let l2sorted = sort l2
    assignLabels 0 n l1sorted l2sorted

--zipper
type BinZip a = (BinCxt a, LabBin a)

--this kind of still needs checkin'
go_left :: BinZip a -> Maybe (BinZip a)
go_left (c,Bl l t1 t2) = Just (B0 l c t2,t1)  -- focus on the left child
go_left (c,Ll _)     = Nothing            -- (leaf => no left child)

go_right :: BinZip a -> Maybe (BinZip a)
go_right (c,Bl l t1 t2) = Just (B1 l t1 c,t2) -- focus on the right child
go_right (c,Ll _)     = Nothing           -- (leaf => no right child)

go_down :: BinZip a -> Maybe (BinZip a)
go_down (B0 l c t2,t) = Just (c,Bl l t t2)    -- focus on parent *from* left child
go_down (B1 l t1 c,t) = Just (c,Bl l t1 t)    -- focus on parent *from* right child
go_down (Hole,t)    = Nothing            -- (root => no parent)

-- Finally, we include some pretty-printing routines for binary trees
-- and binary tree zippers.

-- We make use of drawTree :: Tree String -> String from the Data.Tree
-- module, after first defining some conversion routines from Bin's
-- and BinZip's to Tree String's, which also relies on interpreting a
-- BinCxt as a function Tree String -> Tree String.

treeFromBin :: Show a => LabBin a -> Tree String
treeFromBin (Ll x)     = Node (show x) []
treeFromBin (Bl x t1 t2) = Node (show x) [treeFromBin t1,treeFromBin t2]

treeCxtFromBinCxt :: Show a => BinCxt a -> Tree String -> Tree String
treeCxtFromBinCxt Hole      t = t
treeCxtFromBinCxt (B0 x c t2) t = treeCxtFromBinCxt c (Node (show x) [t, treeFromBin t2])
treeCxtFromBinCxt (B1 x t1 c) t = treeCxtFromBinCxt c (Node (show x) [treeFromBin t1, t])

treeFromBinZip :: Show a => BinZip a -> Tree String
treeFromBinZip (c,t) = treeCxtFromBinCxt c (t'{rootLabel=rootLabel t' ++ marker})
  where
    t' = treeFromBin t
    marker = "@ <--you"

drawBin :: Show a => LabBin a -> String
drawBin = drawTree . treeFromBin

drawBinZip :: Show a => BinZip a -> String
drawBinZip = drawTree . treeFromBinZip
