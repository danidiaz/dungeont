{-# language NamedFieldPuns #-}

module Main where

import Brick
import Streaming.Prelude
import Control.Monad.Trans.State.Strict

import Dungeon
import Dungeon.Player
import Dungeon.Player.Prelude

ui :: Widget ()
ui = str "Hello, world!"

data S a m r = S !a !(Stream (Of a) m r)

main :: IO ()
main = do
    -- let app = App { ... }
    -- :: s -> [Widget n],
    -- :: s -> [CursorLocation n] -> Maybe (CursorLocation n)
    -- :: s -> BrickEvent n e -> EventM n (Next s)
    -- :: s -> EventM n s
    -- :: s -> AttrMap
    let app =
            App 
            { 
              appDraw = undefined         
            , appChooseCursor = undefined
            , appHandleEvent = undefined
            , appStartEvent = undefined
            , appAttrMap = undefined  
            }
        initialDungeon = 
            DungeonState 
            {
               player = Position 5 5
            ,  treasures = [Position 1 2]
            ,  traps = [Position 8 9]
            }
        initialState = 
            S initialDungeon 
              (flip evalStateT initialDungeon . runDungeonT $ approachTreasure 0)
    _ <- defaultMain app initialState
    return ()
