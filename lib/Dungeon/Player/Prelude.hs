{-# language NamedFieldPuns #-}
{-# language MultiWayIf #-}

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
approachTreasure i = do
    PlayerView {selfView,treasuresView} <- viewDungeon
    let treasure = sortOn (distance selfView) treasuresView !! i
    approach treasure

-- https://stackoverflow.com/questions/5193876/goto-in-haskell-can-anyone-explain-this-seemingly-insane-effect-of-continuation
-- "The continuation returned by getCC' has not only ContT's state at the point
-- of the call, but also the state of any monad above ContT on the stack. When you
-- restore that state by calling the continuation, all of the monads built above
-- ContT return to their state at the point of the getCC' call."
getCC' :: MonadCont m => a -> m (a,a -> m b)
getCC' x0 = callCC (\c -> let f x = c (x, f) in return (x0, f))

approachTreasureCont :: (MonadPlayer m, MonadCont m) => m [PlayerResult] 
approachTreasureCont = do
    PlayerView {selfView,treasuresView} <- viewDungeon
    let nearest : farthest: _ = sortOn (distance selfView) treasuresView
    (target,retryWith) <- getCC' nearest
    r <- approach target
    if Death `elem` r
        then retryWith farthest
        else return r
