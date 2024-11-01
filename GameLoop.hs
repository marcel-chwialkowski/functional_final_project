import Treestuff
import Parser
import Cmd

import System.IO
import Control.Concurrent (threadDelay)

-- the top-level interactive loop
gameSetup :: Int -> IO (LabBin Int)
gameSetup treeDepth = do
  gametreeUnlabelled <- randomTree treeDepth
  let treeSize = countNodes gametreeUnlabelled
  if treeSize < 3 
    then gameSetup treeDepth
  else do
    let teamSize = treeSize `div` 3
    labeling <- randomLabels treeSize teamSize 
    let gametreeLabelled = intractPreOrd gametreeUnlabelled labeling
    return gametreeLabelled

repl :: IO ()
repl = do
    putStrLn "Welcome"
    putStrLn "Choose maximum tree depth. Between 3 and 6 recommended for a fun game :).\n"

    input <- getLine
    let treeDepth = read input
    gametreeLabelled <- gameSetup treeDepth
    
    putStrLn "You are entering the tree... \nGood Luck!"

    --define some variables for start
    let zip = (Hole, gametreeLabelled)
    let enemiesNumber = (countNodes gametreeLabelled) `div` 3
    let friendsNumber = enemiesNumber

    go zip enemiesNumber friendsNumber
    --params: zipper, enemiesNumber, friendsNumber
    where 
    go :: BinZip Int -> Int -> Int -> IO ()
    go z 0 _ = do return () --this generally should terminate the game
    go z e f = do 
      --at the beginning of each turn, the player is informed of their whereabouts in the tree
      putStr (drawBinZip z)
      line <- getLine
      case parseInput parseCmd line of
          Nothing -> do
            putStrLn "I'm sorry, I do not understand."
            go z e f 

          Just Go_Left ->
            case z of
              (c,Bl l t1 t2) -> go (B0 l c t2,t1) e f          -- climb up to the left
              (c,Ll _) -> do
                putStrLn "You cannot climb any further."
                go z e f

          Just Go_Right ->
            case z of
              (c,Bl l t1 t2) -> go (B1 l t1 c,t2) e f          -- climb up to the right
              (c,Ll _) -> do
                putStrLn "You cannot climb any further."
                go z e f

          Just Go_Down ->
            case z of
              (B0 l c t2,t) -> go (c,Bl l t t2) e f         -- climb down from the left, or
              (B1 l t1 c,t) -> go (c,Bl l t1 t) e f        -- climb down from the right, or
              (Hole,t) -> do                           -- already at the root
                putStrLn "You are already at the root."
                putStrLn "You cannot climb down any further."
                go z e f

          Just Kill ->
            
          Just Quit ->
            do return ()


main = repl
