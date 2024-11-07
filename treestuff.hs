-- data types for binary trees + one-hole contexts and zippers, as well
-- as some pretty-printing routines

module Treestuff where

import Data.Tree
import Data.Tree.Pretty (drawVerticalTree)
import Data.List (sort)
import System.Random (randomRIO)

--define a labelled binary tree
--new addition: labeling includes list of enemies and friends in this node
data LabBin a = Ll ([a],[a]) | Bl ([a],[a]) (LabBin a) (LabBin a)
  deriving (Show,Eq)

--define a modified context that takes labels into account
data BinCxt a = Hole
              | B0 ([a],[a]) (BinCxt a) (LabBin a)
              | B1 ([a],[a]) (LabBin a) (BinCxt a)
  deriving (Show,Eq)

--similarly, plug is modified
plug :: BinCxt a -> LabBin a -> LabBin a
plug Hole      t = t
plug (B0 p c t2) t = plug c (Bl p t t2)
plug (B1 p t1 c) t = plug c (Bl p t1 t)

--zipper
type BinZip a = (BinCxt a, LabBin a)


--create a random tree with MAXIMUM depth of n
--creating random trees handled \/
randomTree :: Int -> IO (LabBin Int)
randomTree 0 = do
  let x = ([],[])
  return (Ll x)
randomTree n = do
    --we can change the branching probability to make it more probable to reach deeper trees
    branch <- (randomRIO (0, 3) :: IO Int)
    if branch == 0 
        then do 
            let x = ([],[])
            return (Ll x)
        else do
            t1 <- randomTree (n-1)
            t2 <- randomTree (n-1)
            let x = ([],[])
            return (Bl x t1 t2)

--counts number of nodes for a tree
countNodes :: LabBin a -> Int
countNodes (Ll p) = 1
countNodes (Bl p left right) = 1 + countNodes left + countNodes right

--label a tree given a list of labels (in preorder)
intractPreOrd :: LabBin Int -> [Int] -> LabBin Int
intractPreOrd (Ll _) x = if (x!!0) == 2 then Ll ([],[2]) 
                         else  if (x!!0) == 1 then Ll ([1], [])
                         else Ll ([],[])
intractPreOrd (Bl _ left right) x = 
    let aux = countNodes left 
    in  let xright = drop (aux + 1) x
        in let r = intractPreOrd right xright
            in if (x!!0) == 2 then (Bl ([],[2]) (intractPreOrd left (tail x)) r)
            else if (x!!0) == 1 then (Bl ([1],[]) (intractPreOrd left (tail x)) r)
            else (Bl ([],[]) (intractPreOrd left (tail x)) r)

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

--utility function
enemFriendNumbersFstExcluded :: LabBin Int -> (Int, Int)
enemFriendNumbersFstExcluded (Ll l) = (0, 0)
enemFriendNumbersFstExcluded (Bl l t1 t2) = let left = enemFriendNumbers t1 in let right = enemFriendNumbers t2 in 
    (fst left + fst right, snd left + snd right)

enemFriendNumbers :: LabBin Int -> (Int, Int)
enemFriendNumbers (Ll l) = (length (snd l), length (fst l))
enemFriendNumbers (Bl l t1 t2) = let curr = enemFriendNumbers (Ll l) in let left = enemFriendNumbers t1 in let right = enemFriendNumbers t2 in
    (fst curr + fst left + fst right, snd curr + snd left + snd right)


-- =====================================================================
-- moving for non playable characters
-- =====================================================================


labelAdd :: ([Int], [Int]) -> Int -> ([Int], [Int])
labelAdd x 1 = ((1 : (fst x)), snd x)
labelAdd x 2 = (fst x, 2 : snd x)

labelSub :: ([Int], [Int]) -> Int -> ([Int], [Int])
labelSub x 1 = (tail (fst x), snd x)
labelSub x 2 = (fst x, tail (snd x))


moveLeftUpdate :: BinZip Int -> Int -> Maybe (BinZip Int)
moveLeftUpdate (c, Ll l) _ = Nothing
moveLeftUpdate (c, Bl l t1 t2) i = let lp = labelSub l i in 
    case t1 of 
    Ll tl -> let tlp = labelAdd tl i in 
        let t1p = Ll tlp in
            Just (B0 lp c t2, t1p) 
    Bl tl x y -> let tlp = labelAdd tl i in
        let t1p = Bl tlp x y in
            Just (B0 lp c t2, t1p)

moveRightUpdate :: BinZip Int -> Int -> Maybe (BinZip Int)
moveRightUpdate (c, Ll l) _ = Nothing
moveRightUpdate (c, Bl l t1 t2) i = let lp = labelSub l i in 
    case t2 of 
    Ll tl -> let tlp = labelAdd tl i in 
        let t2p = Ll tlp in
            Just (B1 lp t1 c,t2p) 
    Bl tl x y -> let tlp = labelAdd tl i in
        let t2p = Bl tlp x y in
            Just (B1 lp t1 c, t2p)

--params: zipper, ind -> ind specifies if a friend or enemy momves
moveDownUpdate :: BinZip Int -> Int -> Maybe (BinZip Int)
moveDownUpdate (Hole, t) _ = Nothing
moveDownUpdate (B0 l c t2,t) i = let lp = labelAdd l i in 
    case t of
    Ll tl -> let tlp = labelSub tl i in 
        let tp = Ll tlp in 
            Just (c, Bl lp tp t2) 
    Bl tl st1 st2 -> let tlp = labelSub tl i in 
        let tp = (Bl tlp st1 st2) in
            Just (c, Bl lp tp t2)
moveDownUpdate (B1 l t1 c, t) i = let lp = labelAdd l i in 
    case t of
    Ll tl -> let tlp = labelSub tl i in 
        let tp = Ll tlp in 
            Just (c, Bl lp t1 tp) 
    Bl tl st1 st2 -> let tlp = labelSub tl i in 
        let tp = (Bl tlp st1 st2) in
            Just (c, Bl lp t1 tp)


-- =====================================================================
-- fairly certain that the movement is fine
-- =====================================================================


-- =====================================================================
-- copying labels between zippers
-- =====================================================================


adjustLabeling :: LabBin Int -> BinZip Int -> BinZip Int
adjustLabeling z z1 = do
    case z1 of 
        (B0 lab c r, t) -> deepen (adjustLabeling z (c, Bl lab t r)) z1
        (B1 lab l c, t) -> deepen (adjustLabeling z (c, Bl lab l t)) z1
        (Hole, t) -> (Hole, z)

--params: from, to
deepen :: BinZip Int -> BinZip Int -> BinZip Int 
deepen (cFrom, (Bl newl newlhs newrhs)) to = case to of
        (B1 _ _ _, _) -> (B1 newl newlhs cFrom, newrhs)
        (B0 _ _ _, _) -> (B0 newl cFrom newrhs, newlhs)

adjustAllLabelings :: LabBin Int -> [BinZip Int] -> [BinZip Int]
adjustAllLabelings t = foldr (\z acc -> (adjustLabeling t z) : acc) []


-- =====================================================================
-- cut off handling -> similar to the above
-- =====================================================================
--fron is the new thing 

adjustLabelingRobust :: LabBin Int -> BinZip Int -> Maybe (BinZip Int)
adjustLabelingRobust z z1 = 
    case z1 of 
        (B0 lab c r, t) -> do 
            aux <- adjustLabelingRobust z (c, Bl lab t r)
            deepenRobust aux z1
        (B1 lab l c, t) -> do
            aux <- adjustLabelingRobust z (c, Bl lab l t)
            deepenRobust aux z1 
        (Hole, t) -> Just (Hole, z)

deepenRobust :: BinZip Int -> BinZip Int -> Maybe (BinZip Int)
deepenRobust (cFrom, (Bl newl newlhs newrhs)) to = case to of
        (B1 _ _ _, _) -> Just (B1 newl newlhs cFrom, newrhs)
        (B0 _ _ _, _) -> Just (B0 newl cFrom newrhs, newlhs)
deepenRobust (cFrom, (Ll newl)) to = Nothing

adjustAllLabelingsRobust :: LabBin Int -> [BinZip Int] -> [BinZip Int]
adjustAllLabelingsRobust t = foldr (\z acc -> case adjustLabelingRobust t z of
    Just result -> result : acc
    Nothing     -> acc) []

applyTreeToZipper :: LabBin Int -> BinZip Int -> Maybe (BinZip Int)
applyTreeToZipper t (Hole, tp) = Just (Hole, t)
applyTreeToZipper t (B0 lp c t2p, tp) = case t of
    Ll _ -> Nothing
    Bl l t1 t2 -> let aux = applyTreeToZipper t1 (c, tp) in 
        case aux of
        Nothing -> Nothing
        Just zp -> Just (B0 lp (fst zp) t2, snd zp)
applyTreeToZipper t (B1 lp t1p c, tp) = case t of
    Ll _ -> Nothing
    Bl l t1 t2 -> let aux = applyTreeToZipper t2 (c, tp) in 
        case aux of
        Nothing -> Nothing
        Just zp -> Just (B1 lp t1 (fst zp), snd zp)

applyTreeToZipperList :: LabBin Int -> [BinZip Int] -> [BinZip Int]
applyTreeToZipperList t = foldr (\z acc -> case applyTreeToZipper t z of
    Just result -> result : acc
    Nothing     -> acc) []


-- =====================================================================
--Creating zippers from trees
-- =====================================================================


auxFriendZippers :: BinZip Int -> [ BinZip Int ]
auxFriendZippers (c, Ll (f, e)) = case length f of 
    0 -> []
    otherwise -> let zp = (c, Ll (f, e)) in [zp | _ <- [1.. length f]]
auxFriendZippers (c, Bl (f, e) t1 t2) = 
    let 
        r1 = auxFriendZippers (B0 (f,e) c t2, t1) 
        r2 = auxFriendZippers (B1 (f,e) t1 c, t2)
    in 
        case length f of 
        0 -> r1 ++ r2
        otherwise -> let zp = (c, Bl (f, e) t1 t2) in [zp | _ <- [1..length f]] ++ r1 ++ r2

auxEnemyZippers :: BinZip Int -> [ BinZip Int ]
auxEnemyZippers (c, Ll (f, e)) = case length e of 
    0 -> []
    otherwise -> let zp = (c, Ll (f, e)) in [zp | _ <- [1.. length e]]
auxEnemyZippers (c, Bl (f, e) t1 t2) = 
    let 
        r1 = auxEnemyZippers (B0 (f,e) c t2, t1) 
        r2 = auxEnemyZippers (B1 (f,e) t1 c, t2)
    in 
        case length e of 
        0 -> r1 ++ r2
        otherwise -> let zp = (c, Bl (f, e) t1 t2) in [zp | _ <- [1..length e]] ++ r1 ++ r2

createFriendZippers :: LabBin Int -> [BinZip Int]
createFriendZippers t = auxFriendZippers (Hole, t)

createEnemyZippers :: LabBin Int -> [BinZip Int]
createEnemyZippers t = auxEnemyZippers (Hole, t)


-- =====================================================================
-- this works!!!
-- =====================================================================


--kill is triggered on by zipper 1 on a list of zippers. remove 1 corresponding zipper
killZipper :: BinZip Int -> [BinZip Int] -> [BinZip Int]
killZipper killer (z:zs)
    | killer == z = zs 
    | otherwise   = z : killZipper killer zs

--it is shameful to do this instead of defining a datastructure but here we are for now
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

--another auxilliary for simple insertions
modifyAt :: Int -> a -> [a] -> [a]
modifyAt _ _ [] = []  
modifyAt i newVal (x:xs)
  | i < 0     = x : xs  
  | i == 0    = newVal : xs  
  | otherwise = x : modifyAt (i - 1) newVal xs  


moveInd :: [BinZip Int] -> [BinZip Int] -> BinZip Int -> Int -> Int -> IO (([BinZip Int],[BinZip Int],BinZip Int))
moveInd g1 g2 player id ind = 
    let currZip = g1!!ind in
    case currZip of 
    (Hole, Ll x) -> do return ((g1, g2, player)) --this is immovable
    otherwise -> do
        luckyguess <- randomRIO(1, 4) :: IO Int
        case luckyguess of
            1 -> 
                let attempt = moveLeftUpdate currZip id in
                case attempt of
                    Nothing -> (moveInd g1 g2 player id ind)
                    Just newZip -> do
                        let newTree = plug (fst newZip) (snd newZip)
                        let g1p = modifyAt ind newZip g1
                        let newg1 = adjustAllLabelings newTree g1p
                        let newg2 = adjustAllLabelings newTree g2
                        let newplayer = adjustLabeling newTree player
                        return (newg1, newg2, newplayer)

            2 -> 
                let attempt = moveRightUpdate currZip id in
                    case attempt of
                    Nothing ->  (moveInd g1 g2 player id ind)
                    Just newZip -> do
                        let newTree = plug (fst newZip) (snd newZip)
                        let g1p = modifyAt ind newZip g1
                        let newg1 = adjustAllLabelings newTree g1p
                        let newg2 = adjustAllLabelings newTree g2
                        let newplayer = adjustLabeling newTree player
                        return (newg1, newg2, newplayer)

            3 -> 
                let attempt = moveDownUpdate currZip id in
                case attempt of
                    Nothing -> (moveInd g1 g2 player id ind)
                    Just newZip -> do
                        let newTree = plug (fst newZip) (snd newZip)
                        let g1p = modifyAt ind newZip g1
                        let newg1 = adjustAllLabelings newTree g1p
                        let newg2 = adjustAllLabelings newTree g2
                        let newplayer = adjustLabeling newTree player
                        return (newg1, newg2, newplayer)
            4 -> return ((g1, g2, player))
                        

moveGroup :: [BinZip Int] -> [BinZip Int] -> BinZip Int -> Int -> Int -> IO (([BinZip Int],[BinZip Int],BinZip Int))
moveGroup g1 g2 player id 0 = do return (g1, g2, player)
moveGroup g1 g2 player id n = 
    do 
        res <- moveInd g1 g2 player id (length g1 - n) 
        moveGroup (first res) (second res) (third res) id (n - 1)

--printing routines

-- original functions
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

drawBinPretty :: Show a => LabBin a -> String
drawBinPretty = drawVerticalTree . treeFromBin

drawBinZipPretty :: Show a => BinZip a -> String
drawBinZipPretty = drawVerticalTree . treeFromBinZip
-------------

-- p ensures we only print the number levels visited by the player 
treeFromBinNew :: Show a => Int -> LabBin a -> Tree String
treeFromBinNew p (Ll x)     = Node (show x) []
treeFromBinNew p (Bl x t1 t2) 
    | p > 2 = Node (show x) [treeFromBinNew (p - 1) t1,treeFromBinNew (p - 1) t2]
    | otherwise = Node (show x) []

-- v ensures we print based on the player's vision 
treeFromBinShowVis :: Show a => Int -> LabBin a -> Tree String
treeFromBinShowVis v (Ll x)     = Node (show x) []
treeFromBinShowVis v (Bl x t1 t2) 
    | v > 0 = Node (show x) [treeFromBinShowVis (v - 1) t1,treeFromBinShowVis (v - 1) t2]
    | otherwise = Node (show x) []

-- third function, takes tree nodes in BinCxt (more info about each node) form to turn them into tree strings
-- p ensures we only print the number levels visited by the player 
treeCxtFromBinCxtNew :: Show a => Int -> BinCxt a -> Tree String -> Tree String
treeCxtFromBinCxtNew p Hole      t = t
treeCxtFromBinCxtNew p (B0 x c t2) t  
    | p > 0 = treeCxtFromBinCxtNew (p - 1) c (Node (show x) [t, treeFromBinNew (p - 1) t2])
    | otherwise = Node (show x) [] -- t
treeCxtFromBinCxtNew p (B1 x t1 c) t
    | p > 0 = treeCxtFromBinCxtNew (p - 1) c (Node (show x) [treeFromBinNew (p - 1) t1, t])
    | otherwise = Node (show x) [] -- t

-- second function, takes a binary tree zipper to convert into a tree string
treeFromBinZipNew :: Show a => Int -> Int -> BinZip a -> Tree String -- param for vision, then use to decide how many levels of neighbors visible
treeFromBinZipNew p v (c,t) = treeCxtFromBinCxtNew p c (t'{rootLabel=rootLabel t' ++ marker})
  where
    t' = treeFromBinShowVis v t
    marker = "@ <--you"

-- first function, takes in a tree to be converted to a string to print
drawBinZipPrettyNew :: Show a => Int -> Int -> BinZip a -> String -- param for vision, pass to treeFromBinZip
drawBinZipPrettyNew p v = drawVerticalTree . (treeFromBinZipNew p v)
