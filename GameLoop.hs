import Treestuff
import Parser
import Cmd

import System.IO
import Control.Concurrent (threadDelay)

import Control.Monad.Trans.State (StateT, evalStateT, execStateT, runStateT, get, put, modify)
import Control.Monad.IO.Class (liftIO)


data GameState = GameState
  {binzip :: BinZip Int,
   enRem :: Int,
   frRem :: Int,
   turn :: Int,
   die :: Bool}

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
endGame turns friends = do
  let score = fromIntegral friends / fromIntegral turns
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
    putStrLn "Welcome"
    putStrLn "Choose maximum tree depth. Between 3 and 6 recommended for a fun game :).\n"

    input <- getLine
    let treeDepth = read input
    gametreeLabelled <- gameSetup treeDepth
    
    putStrLn "You are entering the tree... \nGood Luck!"

    let enemiesNumber = (countNodes gametreeLabelled) `div` 3
    let friendsNumber = enemiesNumber

    --binzipper to move around the tree
    let binzip = (Hole, gametreeLabelled)
    let startState = GameState {binzip = binzip, enRem = enemiesNumber, frRem = friendsNumber, turn = 0, die = False}

    evalStateT go startState
    where 
    go :: Game ()
    go = do
      gameState <- get
      if enRem gameState == 0 
        then liftIO $ endGame (frRem gameState) (turn gameState)
      else if die gameState == True
        then do 
          liftIO $ putStr ("You stayed close to an enemy too long. He kills you")
          liftIO $ endGame (frRem gameState) (turn gameState)
      else
          do
          --print turn information at the beginning
          liftIO $ putStr ("--- Turn " ++ (show ((turn gameState)+1)) ++ " ---\n")
          liftIO $ putStr (drawBinZip (binzip gameState))
          
          --get player input
          line <- liftIO getLine

          -- as we always increment the turn, maybe we can do it here outside pattern matching
          modify (\s -> s {turn = (turn s) + 1})
          let bz = binzip gameState

          case parseInput parseCmd line of
              Nothing -> do
                liftIO $ putStrLn "I'm sorry, I do not understand."
                go

              Just Go_Left ->
                case bz of
                  (c,Bl l t1 t2) -> do
                    let newZip = (B0 l c t2,t1)
                    modify (\s -> s {binzip = newZip})
                    go
                  (c,Ll _) -> do
                    liftIO $ putStrLn "You cannot climb any further."
                    go 

              Just Go_Right ->
                case bz of
                  (c,Bl l t1 t2) -> do
                    let newZip = (B1 l t1 c,t2)
                    modify (\s -> s {binzip = newZip})
                    go
                  (c,Ll _) -> do
                    liftIO $ putStrLn "You cannot climb any further."
                    go 

              Just Go_Down ->
                case bz of
                  (B0 l c t2,t) -> do
                    let newZip = (c,Bl l t t2)
                    modify (\s -> s {binzip = newZip})
                    go    
                  (B1 l t1 c,t) -> do
                    let newZip = (c,Bl l t1 t) 
                    modify (\s -> s {binzip = newZip})
                    go

                  (Hole,t) -> do                    
                    liftIO $ putStrLn "You are already at the root."
                    liftIO $ putStrLn "You cannot climb down any further."
                    go

              --we dont even need to 
              Just Kill ->
                case snd bz of 
                  Bl l t1 t2 -> case l of
                    0 -> do 
                      liftIO $ putStrLn "But there is no-one here?"
                      go
                    1 -> do 
                      liftIO $ putStrLn "Killed a friend! Why would you do that?"
                      let newZip = (fst bz, Bl 0 t1 t2)
                      modify (\s -> s {binzip = newZip, frRem = frRem s - 1})
                      go 
                    2 -> do
                      liftIO $ putStrLn "Killed an enemy!"
                      let newZip  = (fst bz, Bl 0 t1 t2)
                      modify (\s -> s {binzip = newZip, enRem = enRem s - 1})
                      go
                  Ll l -> case l of
                    0 -> do 
                      liftIO $ putStrLn "But there is no-one here?"
                      go
                    1 -> do 
                      liftIO $ putStrLn "Killed a friend! Why would you do that?"
                      let newZip = (fst bz, Ll 0)
                      modify (\s -> s {binzip = newZip, frRem = frRem s - 1})
                      go
                    2 -> do
                      liftIO $ putStrLn "Killed an enemy!"
                      let newZip = (fst bz, Ll 0)
                      modify (\s -> s {binzip = newZip, enRem = enRem s - 1})
                      go
              
              Just Cut_Off ->
                --we also dont need the context here, we are not moving
                case snd bz of
                  Ll l -> do
                    liftIO $ putStrLn "No branches to cut here!"
                    go 
                  Bl l t1 t2 -> do
                    let upd = enemFriendNumbersFstExcluded (snd bz)
                    let newZip = (fst bz, Ll l)
                    liftIO $ putStrLn "Branch cut off!"
                    liftIO $ putStrLn ("Killed " ++ (show (fst upd)) ++ " enemies")
                    liftIO $ putStrLn ("and " ++ (show (snd upd)) ++ " friends")
                    put GameState {binzip = newZip, enRem = enRem gameState - fst upd, frRem = frRem gameState - snd upd, turn = turn gameState, die = die gameState}
                    go
              Just Quit ->
                do return ()


main = repl
