{-# language ScopedTypeVariables #-}
{-# language NamedFieldPuns #-}

module Main where

import Data.Monoid
import Data.Functor
import Data.Functor.Identity
import Streaming.Prelude
import Control.Monad.Trans.State.Strict

import Brick
import qualified Brick.Main

import Graphics.Vty.Image
import Graphics.Vty.Attributes(defAttr)

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
    let app :: App (S DungeonState Identity ()) () () =
            App 
            { 
              appDraw = const $ [ raw $ vertCat $ [ Graphics.Vty.Image.string defAttr "XXXXoXXXX"
                                                  , Graphics.Vty.Image.string defAttr "skdfjalsd"]
                                ]         
            , appChooseCursor = Brick.Main.neverShowCursor
            , appHandleEvent = \s _ -> Brick.Main.continue s
            , appStartEvent = return
            , appAttrMap = const $ attrMap mempty []
            }
        initialDungeon = 
            DungeonState 
            {
               player = Position 6 5
            ,  treasures = [Position 1 2]
            ,  traps = [Position 8 9]
            }
        initialState = 
            S initialDungeon 
              (void $ flip evalStateT initialDungeon $ runDungeonT $ approachTreasure 0)
    _ <- defaultMain app initialState
    return ()
