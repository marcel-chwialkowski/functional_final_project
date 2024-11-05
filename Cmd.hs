module Cmd where

data Cmd = Go_Left | Go_Right | Go_Down | Meditate | Quit | Kill | Cut_Off | Continue
  deriving (Show,Read)
