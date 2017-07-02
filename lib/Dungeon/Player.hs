{-# language NamedFieldPuns #-}
{-# language FlexibleContexts #-}

module Dungeon.Player (
        PlayerView(..),
        perceive,
        PlayerAction(..),
        PlayerResult(..),
        MonadPlayer(..)
    ) where

import Control.Monad.State
import Dungeon

data PlayerView = PlayerView
    {
       selfView :: Position,
       treasuresView :: [Position]
    } deriving (Eq,Show)

perceive :: DungeonState -> PlayerView 
perceive DungeonState {player,treasures,traps} = 
    PlayerView { selfView = player, treasuresView = treasures ++ traps }

data PlayerAction = 
      DoNothing
    | MoveNorth
    | MoveEast
    | MoveSouth
    | MoveWest
    deriving (Eq,Show,Enum)

data PlayerResult =
      Win
    | Death
    deriving (Eq,Show,Enum)

class MonadState PlayerView m => MonadPlayer m where
    takeAction :: PlayerAction -> m [PlayerResult] 

