{-# language NamedFieldPuns #-}
{-# language MultiWayIf #-}
{-# language FlexibleContexts #-}
{-# language DefaultSignatures #-}

module Dungeon.Player (
        PlayerView(..),
        perceive,
        PlayerAction(..),
        PlayerResult(..),
        MonadPlayer(..)
    ) where

import Control.Monad.State.Strict
import Dungeon

data PlayerView = PlayerView
    {
       selfView :: !Position,
       treasuresView :: ![Position]
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

move :: PlayerAction -> Position -> Position 
move action (Position{xpos,ypos}) = 
     let (xdelta,ydelta) = case action of
            DoNothing -> (0,0)  
            MoveNorth -> (0,-1)
            MoveEast  -> (1,0)
            MoveSouth -> (0,1)
            MoveWest  -> (-1,0) 
     in Position {xpos = xpos + xdelta, ypos = ypos + ydelta} 

data PlayerResult =
      Win
    | Death
    deriving (Eq,Show,Enum)

class MonadPlayer m where
    viewDungeon :: m PlayerView 
    default viewDungeon :: MonadState DungeonState m => m PlayerView 
    viewDungeon = fmap perceive get

    takeAction :: PlayerAction -> m [PlayerResult] 
    default takeAction :: MonadDungeon m => PlayerAction -> m [PlayerResult]
    takeAction action = do
         DungeonState {player,treasures,traps} <- get
         result <- if | player `elem` treasures -> return [Win]
                      | player `elem` traps -> return [Death]
                      | otherwise -> do
                          modify' $ \s -> s {player = move action player}
                          return []
         get >>= yieldDungeon
         return result

instance Monad m => MonadPlayer (DungeonT m)

instance Monad m => MonadPlayer (ContDungeonT cr m)

