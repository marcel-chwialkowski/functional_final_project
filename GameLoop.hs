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

--for now the score is proportional to friends remaining but that shouldnt be the case - it should be inversely prop to friends killed
endGame :: Int -> Int -> IO ()
endGame friendsKilled turns = do
  let score = fromIntegral 100 / fromIntegral (if friendsKilled > 0 then friendsKilled * turns else turns)
  putStrLn ("Well played! You finished the game with a score of " ++ (show score))
  putStrLn ("Type yes if you want to play again!")
  line <- getLine
  case parseInput parseCmd line of 
    Just Continue -> do 
      putStrLn ("Okay, restarting")
      repl
    otherwise -> return ()

repl :: IO ()
repl = do
    putStrLn "Welcome!" -- add "do you know how to play? yes -> continue, no -> provide information"
    putStrLn "Choose maximum tree depth. Between 3 and 6 recommended for a fun game :).\n"

    input <- getLine
    let treeDepth = read input
    gametreeLabelled <- gameSetup treeDepth
    
    putStrLn "You are entering the tree on team 1." 
    putStrLn "You can move through the tree in the following ways:"
    putStrLn "\"go down\" to go down one step towards the root" 
    putStrLn "\"go right\" to go one step up the right branch"
    putStrLn "\"go left\" to go one step up the left branch" 
    putStrLn "\"kill\" to kill whoever is at the current node" 
    putStrLn "\"cut off\" to cut off part of the tree extending from that branch" 
    putStrLn "\"meditate\" to stay at the current node for a turn" 
    putStrLn "All of these commands count as moves, so try to minimize them. Kill all your enemies (team 2) with the least amount of moves possible... \nGood Luck!"

    let enemiesNumber = (countNodes gametreeLabelled) `div` 3
    let friendsNumber = enemiesNumber
    -- let friendsKilled = 0 -- use to keep track of friends killed, that'll then be used in score calculation

    --zipper to move around the tree
    let zip = (Hole, gametreeLabelled)
    go zip enemiesNumber friendsNumber 0 0 

    --params: zipper, enemiesNumber, friendsAlive, friendsKilled, Turn
    where 
    go :: BinZip Int -> Int -> Int -> Int -> Int -> IO ()
    go z 0 fa fk i = do endGame fk i --this generally should terminate the game
    go z e fa fk i = do 
      putStr ("--- Turn " ++ (show (i + 1)) ++ " ---\n")
      --at the beginning of each turn, the player is informed of their whereabouts in the tree
      putStr (drawBinZip z)
      line <- getLine
      case parseInput parseCmd line of
          Nothing -> do
            putStrLn "I'm sorry, I do not understand."
            go z e fa fk (i + 1)

          Just Go_Left ->
            case z of
              (c,Bl l t1 t2) -> go (B0 l c t2,t1) e fa fk (i + 1)          -- climb up to the left
              (c,Ll _) -> do
                putStrLn "You cannot climb any further."
                go z e fa fk (i + 1)

          Just Go_Right ->
            case z of
              (c,Bl l t1 t2) -> go (B1 l t1 c,t2) e fa fk (i + 1)         -- climb up to the right
              (c,Ll _) -> do
                putStrLn "You cannot climb any further."
                go z e fa fk (i + 1)

          Just Go_Down ->
            case z of
              (B0 l c t2,t) -> go (c,Bl l t t2) e fa fk (i + 1)        -- climb down from the left, or
              (B1 l t1 c,t) -> go (c,Bl l t1 t) e fa fk (i + 1)      -- climb down from the right, or
              (Hole,t) -> do                           -- already at the root
                putStrLn "You are already at the root."
                putStrLn "You cannot climb down any further."
                go z e fa fk (i + 1)

          --we dont even need to 
          Just Kill ->
            case snd z of 
              Bl l t1 t2 -> case l of
                0 -> do 
                  putStrLn "But there is no-one here?"
                  go z e fa fk (i + 1)
                1 -> do 
                  putStrLn "Killed a friend! Why would you do that?"
                  let z1 = (fst z, Bl 0 t1 t2)
                  go z1 e (fa - 1) (fk + 1) (i + 1)
                2 -> do
                  putStrLn "Killed an enemy!"
                  let z1 = (fst z, Bl 0 t1 t2)
                  go z1 (e - 1) fa fk (i + 1)
              Ll l -> case l of
                0 -> do 
                  putStrLn "But there is no-one here?"
                  go z e fa fk (i + 1)
                1 -> do 
                  putStrLn "Killed a friend! Why would you do that?"
                  let z1 = (fst z, Ll 0)
                  go z1 e (fa - 1) (fk + 1) (i + 1)
                2 -> do
                  putStrLn "Killed an enemy!"
                  let z1 = (fst z, Ll 0)
                  go z1 (e - 1) fa fk (i + 1)
          
          Just Cut_Off ->
            --we also dont need the context here, we are not moving
            case snd z of
              Ll l -> do
                putStrLn "No branches to cut here!"
                go z e fa fk (i + 1)
              Bl l t1 t2 -> do
                let upd = enemFriendNumbersFstExcluded (snd z)
                let z1 = (fst z, Ll l)
                putStrLn "Branch cut off!"
                putStrLn ("Killed " ++ (show (fst upd)) ++ " enemies")
                putStrLn ("and " ++ (show (snd upd)) ++ " friends")
                go z1 (e - fst upd) (fa - snd upd) (fk + snd upd) (i + 1)
                
          Just Quit ->
            do return ()


main = repl
