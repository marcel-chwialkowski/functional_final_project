-- data types for binary trees + one-hole contexts and zippers, as well
-- as some pretty-printing routines

module Bin where

import Data.Tree

-- basic data type of unlabelled binary trees
data Bin a = L a | B (Bin a) (Bin a)
  deriving (Show,Eq)

-- a "one-hole context" for a binary tree may be thought of as a
-- binary tree with a hole for another binary tree
data BinCxt a = Hole
              | B0 (BinCxt a) (Bin a)
              | B1 (Bin a) (BinCxt a)
  deriving (Show,Eq)

-- Plugging a one-hole context with a binary tree to produce a binary
-- tree is accomplished by the following function
plug :: BinCxt a -> Bin a -> Bin a
plug Hole      t = t
plug (B0 c t2) t = plug c (B t t2)
plug (B1 t1 c) t = plug c (B t1 t)

-- Under this definition of plugging, values of type BinCxt are
-- interpreted "inside-out" as describing paths *from* the hole *to*
-- the root.  Alternatively, we can interpret contexts "outside-in" as
-- describing paths from the root to the hole, in which case we would
-- use the following plugging function.

{-
plug' :: BinCxt a -> Bin a -> Bin a
plug' Hole      t = t
plug' (B0 c t2) t = B (plug' c t) t2
plug' (B1 t1 c) t = B t1 (plug' c t)
-}

-- But the inside-out representation is more useful for navigating
-- within a tree using "zippers".

-- A zipper is a pair of a one-hole context c and a tree t, which we
-- think of as defining a pointer to t as a subtree of u = plug c t.
type BinZip a = (BinCxt a,Bin a)
-- (The terminology comes from Gérard Huet's paper, "The Zipper".)

-- The following functions implement moving the pointer up to the
-- left child, up to the right child, or down to the parent of a
-- subtree.  (Note that these operations are only partial, i.e., return a
-- Maybe type, since the subtree may not have a child or a parent.)

go_left :: BinZip a -> Maybe (BinZip a)
go_left (c,B t1 t2) = Just (B0 c t2,t1)  -- focus on the left child
go_left (c,L _)     = Nothing            -- (leaf => no left child)

go_right :: BinZip a -> Maybe (BinZip a)
go_right (c,B t1 t2) = Just (B1 t1 c,t2) -- focus on the right child
go_right (c,L _)     = Nothing           -- (leaf => no right child)

go_down :: BinZip a -> Maybe (BinZip a)
go_down (B0 c t2,t) = Just (c,B t t2)    -- focus on parent *from* left child
go_down (B1 t1 c,t) = Just (c,B t1 t)    -- focus on parent *from* right child
go_down (Hole,t)    = Nothing            -- (root => no parent)

-- It is also easy to implement operations that perform simple edits,
-- such as say grafting another tree off to the left or right of the
-- the subtree in focus.

graft_left :: Bin a -> BinZip a -> BinZip a
graft_left  g (c,t) = (c,B g t)
graft_right :: Bin a -> BinZip a -> BinZip a
graft_right g (c,t) = (c,B t g)

-- Finally, we include some pretty-printing routines for binary trees
-- and binary tree zippers.

-- We make use of drawTree :: Tree String -> String from the Data.Tree
-- module, after first defining some conversion routines from Bin's
-- and BinZip's to Tree String's, which also relies on interpreting a
-- BinCxt as a function Tree String -> Tree String.

treeFromBin :: Show a => Bin a -> Tree String
treeFromBin (L x)     = Node (show x) []
treeFromBin (B t1 t2) = Node "*" [treeFromBin t1,treeFromBin t2]

treeCxtFromBinCxt :: Show a => BinCxt a -> Tree String -> Tree String
treeCxtFromBinCxt Hole      t = t
treeCxtFromBinCxt (B0 c t2) t = treeCxtFromBinCxt c (Node "*" [t, treeFromBin t2])
treeCxtFromBinCxt (B1 t1 c) t = treeCxtFromBinCxt c (Node "*" [treeFromBin t1, t])

treeFromBinZip :: Show a => BinZip a -> Tree String
treeFromBinZip (c,t) = treeCxtFromBinCxt c (t'{rootLabel=rootLabel t' ++ marker})
  where
    t' = treeFromBin t
    marker = "@ <--you"

drawBin :: Show a => Bin a -> String
drawBin = drawTree . treeFromBin

drawBinZip :: Show a => BinZip a -> String
drawBinZip = drawTree . treeFromBinZip
