# Functional programming final projecct

*Authors: Marcel Chwiałkowski, Aditi Sadwelkar*

We wrote a turn-based game. As a player, you spawn in a binary tree and you are tasked with eliminating all your enemies while eliminating as few of your allies as possible. There are 2 ways of eliminating players:

* kill - this action kills a single enemy in your node
* cut off - this action cuts off the branches of the tree below you, killing everyone there (possibly some allies!)

The game ends when you die or the all enemies die.

## Running the game

The only non-standard library used is pretty-tree. It can be installed via
cabal install pretty-tree.

## Files

### Cmd.hs

Contains the commands available in the game. Just an extended version of *Binary Tree World* Cmd.hs file.

### GameLoop.hs

Main file. Contains the game loop and functions to start and end the game.

* As the game contains a lot of state variables that are modified at every turn, we decided to use StateT to handle everything:

```haskell
data GameState = GameState
  {binzip :: BinZip Int,
   enZips :: [BinZip Int],
   frZips :: [BinZip Int],
   enRem :: Int,
   frRem :: Int,
   frKill :: Int,
   pos :: Int, 
   turn :: Int,
   hp :: Int,
   vision :: Int,
   freeze :: Bool}

type Game = StateT GameState IO
```

* go - function representing the game loop.
* gameSetup - creates a random tree for the game to take place on and places allies and enemies on the tree.
* endGame - gives the player his score and asks him to play again - then restarts the game loop or quits according to the answer.

### treestuff.hs

Contains most of the 'backend'. Some important functions:

* randomTree - creates a random binary tree with empty labels of specified maximal depth
* intractPreOrd - fills in the labels of a given tree
* randomLabels - generates a list with randomized labels for the tree (the result of this function is later given to intractPreOrd)
* moveLeftUpdate, moveRightUpdate, moveDownUpdate - movement for non-playable characters. The signifcant difference between the player movement and those functions is that they modify labels of the tree.
* adjustLabeling - given a binary tree and a zipper on a tree with the same structure, it adjusts the labels within the zipper so that they agree with the labels on the tree
* adjustLabelingRobust - similar, but without the "same structure" guarantee - i.e., the tree given is a subset of the tree on which the zipper is defined. Therefore, if the zipper doesn't exist on the argument tree Nothing is returned.
* createFriendZippers, createEnemyZippers - given tree with labels, extract list of zippers representing friends/enemies' positions.
* killZipper - given a zipper and a list of zippers, removes a zipper from the list based on equality test with the other argument.
* moveInd, moveGroup - moves around a list of zipper on the trees, updating all other zippers at the same time. Used for moving friends and enemies.

Also contains the printing routines for the tree. The vision and position parameters control how much of the tree is visible to the player at every turn. 
By default, the player's vision is 1, which allows them to view immediately to their left and right in the tree. 
When their vision is augmented by meditating by a friend, they can view a level deeper into the tree. 
The whole final tree is displayed at the end of the game. 

### Parser.hs

Just a slightly extended version of the parser for *Binary Tree World.*
