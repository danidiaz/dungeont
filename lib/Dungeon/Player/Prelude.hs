{-# language NamedFieldPuns #-}

module Dungeon.Player.Prelude (
        approach
    ,   approachTreasure
    ,   approachTreasureCont
    ) where

import Data.List
import Dungeon
import Dungeon.Player

import Control.Monad.Cont

distance :: Position -> Position -> Float
distance (Position x1 y1) (Position x2 y2) = 
    sqrt . fromIntegral $ (x1-x2)^(2::Int) + (y1-y2)^(2::Int)

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
            
-- | Approach the ith nearest treasure.
approachTreasure :: MonadPlayer m => Int -> m [PlayerResult] 
approachTreasure i = do
    PlayerView {selfView,treasuresView} <- viewDungeon
    let treasure = sortOn (distance selfView) treasuresView !! i
    approach treasure

-- | Approach the nearest treasure; if you die, go back in time and try with
-- another treasure.
--
-- Previously, I had the problem described here:
--
-- https://stackoverflow.com/questions/44988528/statet-over-cont-why-is-my-state-not-being-reset
--
-- I solved it by not relying on the default MonadCont instance, instead using
-- liftCallCC.
approachTreasureCont :: (MonadPlayerCont m) => m [PlayerResult] 
approachTreasureCont = do
    PlayerView {selfView,treasuresView} <- viewDungeon
    let candidates = sortOn (distance selfView) treasuresView
    (current:rest, retry) <- getPlayerCC candidates
    r <- approach current
    if Death `elem` r
        then retry rest
        else return r
