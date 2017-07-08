{-# language NamedFieldPuns #-}
{-# language MultiWayIf #-}

module Dungeon.Player.Prelude (
        approach,
        approachTreasure
    ) where

import Data.List
import Dungeon
import Dungeon.Player

distance :: Position -> Position -> Float
distance (Position x1 y1) (Position x2 y2) = sqrt . fromIntegral $ (x1-x2)^(2::Int) + (y1-y2)^(2::Int)

approach :: MonadPlayer m => Position -> m [PlayerResult] 
approach (Position targetx targety) = 
    let go = do
            PlayerView {selfView = Position {xpos,ypos}} <- viewDungeon
            case (targetx-xpos, targety-ypos) of
                (0,0) -> takeAction DoNothing
                (dx,dy) -> do
                    let movement = if abs dx > abs dy
                                      then if dx > 0 then MoveEast
                                                     else MoveWest
                                      else if dy > 0 then MoveSouth
                                                     else MoveNorth
                    _ <- takeAction movement
                    go
    in go
            
approachTreasure :: MonadPlayer m => Int -> m [PlayerResult] 
approachTreasure k = do
    PlayerView {selfView,treasuresView} <- viewDungeon
    let treasure = sortOn (distance selfView) treasuresView !! k
    approach treasure

