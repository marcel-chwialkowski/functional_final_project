import Treestuff
import Parser
import Cmd

import System.IO
import Control.Concurrent (threadDelay)

import Control.Monad.Trans.State (StateT, evalStateT, execStateT, runStateT, get, put, modify)
import Control.Monad.State (StateT, lift)
import Control.Monad.IO.Class (liftIO)

data GameState = GameState
  {binzip :: BinZip Int,
   enZips :: [BinZip Int],
   frZips :: [BinZip Int],
   enRem :: Int,
   frRem :: Int,
   frKill :: Int,
   pos :: Int, -- keeps track of the level the player is at in the tree
   turn :: Int,
   hp :: Int,
   vision :: Int,
   freeze :: Bool}

type Game = StateT GameState IO

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
  let score = if friendsKilled > 0 then (fromIntegral friendsKilled) * (fromIntegral turns) else (fromIntegral turns)
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
    putStrLn "Welcome!" -- TO DO "do you know how to play? yes -> continue, no -> provide information"
    putStrLn "Shall I tell you how to play?"
    inst <- getLine
    case parseInput parseCmd inst of 
      Just Continue -> do 
        putStrLn "\nYou are in a tree surrounded by both friends and enemies. You and your friends are on team 1, your enemies are on team 2.\n"
        putStrLn "Gameplay: To navigate through the tree, you can do the following:"
        putStrLn "\"go down\" to go down one step towards the root" 
        putStrLn "\"go right\" to go one step up the right branch"
        putStrLn "\"go left\" to go one step up the left branch" 
        putStrLn "You can also choose to stay at a given node and perform one of the following moves:"
        putStrLn "\"kill\" to kill whoever is at the current node" 
        putStrLn "\"cut off\" to cut off the part of the tree extending from that branch" 
        putStrLn "\"meditate\" to do nothing but stay at the current node for a turn\n" 
        putStrLn "Powerup: By default, you can see the tree up to the depth of your position as well as your immediate neighbors when you choose to go left or right."
        putStrLn "If you meditate by a friend, they can augment your vision, allowing you to see another level deeper into the tree."
        putStrLn "However if you meditate by an enemy, they can kill you!\n"
        putStrLn "Scoring: The goal is to kill all your enemies while saving as many friends as possible."
        putStrLn "Your final score is calculated based on the number of moves you make and the number of friends you kill, so try to keep both of these numbers as low as possible.\n\n"
        putStrLn "Choose maximum tree depth. Between 3 and 6 recommended for a fun game :).\n"
      otherwise -> do
        putStrLn "Choose maximum tree depth. Between 3 and 6 recommended for a fun game :).\n"
        
    input <- getLine
    let treeDepth = read input
    gametreeLabelled <- gameSetup treeDepth
    
    putStrLn "You are entering the tree on team 1." 
    putStrLn "Kill all your enemies (team 2) with the least amount of moves possible... \nGood Luck!"

    let enemiesNumber = (countNodes gametreeLabelled) `div` 3
    let friendsNumber = enemiesNumber

    --here we shoudl traverse the tree to define the lists
    let enemyZippers = createEnemyZippers gametreeLabelled
    let friendZippers = createFriendZippers gametreeLabelled

    --binzipper to move around the tree
    let binzip = (Hole, gametreeLabelled)
    let startState = GameState {binzip = binzip, enZips = enemyZippers, frZips = friendZippers, enRem = enemiesNumber, frRem = friendsNumber, frKill = 0, pos = 0, turn = 0, hp = 5, vision = 1, freeze = True}

    evalStateT go startState
    where 
    go :: Game ()
    go = do
      gameState <- get
      if enRem gameState == 0 
        then liftIO $ endGame (frKill gameState) (turn gameState)
      else if hp gameState <= 0
        then do 
          liftIO $ putStr ("You stayed close to an enemy too long. He kills you")
          liftIO $ endGame (frKill gameState) (turn gameState)
      else
          do
          
          -- movement of enemies
          let bzpre = binzip gameState
          if freeze gameState == False then do
            friendsMove <- lift (moveGroup (frZips gameState) (enZips gameState) bzpre 1 (frRem gameState))
            enemiesMove <- lift (moveGroup (second friendsMove) (first friendsMove) (third friendsMove) 2 (enRem gameState))
            modify (\s -> s {binzip = (third enemiesMove), enZips = (first enemiesMove), frZips = (second enemiesMove)})
          else do
            modify (\s -> s {freeze = False})

          gameState <- get
          
          let bz = binzip gameState

          --print turn information at the beginning
          liftIO $ putStr ("--- Turn " ++ (show ((turn gameState)+1)) ++ " ---\n")
          -- Check the level in the tree: liftIO $ putStr ("--- Level in tree " ++ (show ((pos gameState))) ++ " ---\n")
          liftIO $ putStr (drawBinZipPrettyNew (pos gameState) (vision gameState) (binzip gameState))
          
          --get player input
          line <- liftIO getLine

          -- as we always increment the turn, maybe we can do it here outside pattern matching
          modify (\s -> s {turn = (turn s) + 1})

          case parseInput parseCmd line of
              Nothing -> do
                liftIO $ putStrLn "I'm sorry, I do not understand."
                go

              Just Go_Left ->
                case bz of
                  (c,Bl l t1 t2) -> do
                    let newZip = (B0 l c t2,t1)
                    modify (\s -> s {binzip = newZip, pos = pos s + 1})
                    go
                  (c,Ll _) -> do
                    liftIO $ putStrLn "You cannot climb any further."
                    go 

              Just Go_Right ->
                case bz of
                  (c,Bl l t1 t2) -> do
                    let newZip = (B1 l t1 c,t2)
                    modify (\s -> s {binzip = newZip, pos = pos s + 1})
                    go
                  (c,Ll _) -> do
                    liftIO $ putStrLn "You cannot climb any further."
                    go 

              Just Go_Down ->
                case bz of
                  (B0 l c t2,t) -> do
                    let newZip = (c,Bl l t t2)
                    modify (\s -> s {binzip = newZip, pos = pos s - 1})
                    go    
                  (B1 l t1 c,t) -> do
                    let newZip = (c,Bl l t1 t) 
                    modify (\s -> s {binzip = newZip, pos = pos s - 1})
                    go

                  (Hole,t) -> do                    
                    liftIO $ putStrLn "You are already at the root."
                    liftIO $ putStrLn "You cannot climb down any further."
                    go

              Just Kill ->
                case snd bz of 
                  Bl (f, e) t1 t2 -> case length e of
                    0 -> do 
                      liftIO $ putStrLn "But there are no enemies here?"
                      go
                    otherwise -> do 
                      liftIO $ putStrLn "Killed an enemy!"
                      let newEnZipsP = killZipper bz (enZips gameState)
                      let newZip  = (fst bz, Bl (f, tail e) t1 t2)
                      let newTree = plug (fst newZip) (snd newZip)
                      let newEnZips = adjustAllLabelings newTree newEnZipsP
                      let newFrZips = adjustAllLabelings newTree (frZips gameState)

                      modify (\s -> s {binzip = newZip, enZips = newEnZips, frZips = newFrZips, enRem = enRem s - 1})
                      go
                  Ll (f, e) -> case length e of
                    0 -> do 
                      liftIO $ putStrLn "But there is no-one here?"
                      go
                    otherwise -> do 
                      liftIO $ putStrLn "Killed an enemy!"
                      let newEnZipsP = killZipper bz (enZips gameState)
                      let newZip  = (fst bz, Ll (f, tail e))
                      let newTree = plug (fst newZip) (snd newZip)
                      let newEnZips = adjustAllLabelings newTree newEnZipsP
                      let newFrZips = adjustAllLabelings newTree (frZips gameState)

                      modify (\s -> s {binzip = newZip, enZips = newEnZips, frZips = newFrZips, enRem = enRem s - 1})
                      go
              
              Just Cut_Off -> do
                --we also dont need the context here, we are not moving
                --check if hp has to be updated first
                case snd bz of 
                  Ll (f, e) -> do modify (\s -> s {hp = hp gameState - length e})
                  Bl (f, e) t1 t2 -> do modify (\s -> s {hp = hp gameState - length e})

                case snd bz of
                  Ll l -> do
                    liftIO $ putStrLn "No branches to cut here!"
                    go 
                  Bl l t1 t2 -> do
                    let upd = enemFriendNumbersFstExcluded (snd bz)
                    let newZip = (fst bz, Ll l)
                    let newTree = plug (fst newZip) (snd newZip)
                    let newFrZips = adjustAllLabelingsRobust newTree (frZips gameState)
                    let newEnZips = adjustAllLabelingsRobust newTree (enZips gameState)

                    liftIO $ putStrLn "Branch cut off!"
                    liftIO $ putStrLn ("Killed " ++ (show (fst upd)) ++ " enemies")
                    liftIO $ putStrLn ("and " ++ (show (snd upd)) ++ " friends")
                    modify (\s -> s {binzip = newZip, enZips = newEnZips, frZips = newFrZips, enRem = enRem gameState - fst upd, frRem = frRem gameState - snd upd, frKill = frKill gameState + snd upd})
                    go

             --do some edge cases here, not super important
              Just Meditate -> do
                  liftIO $ putStrLn "You rest."
                  case snd bz of 
                    Ll (f, e) -> do modify (\s -> s {hp = hp gameState - length e})
                    Bl (f, e) t1 t2 -> do modify (\s -> s {hp = hp gameState - length e})
                  case snd bz of
                    Ll (f, e) -> case length f of 
                      0 -> return ()
                      otherwise -> do
                        liftIO $ putStrLn "A friend is nearby. Your vision increases."
                        modify (\s -> s {vision = vision gameState + 1})
                    Bl (f, e) t1 t2 -> case length f of
                      0 -> return ()
                      otherwise -> do 
                        liftIO $ putStrLn "A friend is nearby. Your vision increases."
                        modify (\s -> s {vision = vision gameState + 1})
                  go
              Just Quit ->
                do return ()


main = repl
