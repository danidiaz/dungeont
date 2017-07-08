{-# language ScopedTypeVariables #-}
{-# language NamedFieldPuns #-}

module Main where

import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Streaming.Prelude
import Control.Monad.ST
import Control.Monad.Trans.State.Strict

import Brick
import qualified Brick.Main

import Graphics.Vty.Image
import Graphics.Vty.Attributes(defAttr)
import Graphics.Vty.Input.Events

import Dungeon
import Dungeon.Player.Prelude

renderDungeon :: DungeonState -> [[Char]]
renderDungeon DungeonState {player,treasures,traps} = runST (do
    matrix <- V.replicateM 10 (MV.replicate 10 ' ')
    let drawPos c vec2d Position { xpos, ypos } = do
            MV.unsafeWrite (V.unsafeIndex vec2d ypos) xpos c
    forM_ treasures $ drawPos '$' matrix
    forM_ traps $ drawPos 'X' matrix
    drawPos '@' matrix player
    matrix' <- traverse V.unsafeFreeze matrix
    return $ V.toList . fmap V.toList $ matrix')

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
              appDraw = \(S dungeon _) -> [raw . vertCat . fmap (Graphics.Vty.Image.string defAttr) $ renderDungeon dungeon]
            , appChooseCursor = Brick.Main.neverShowCursor
            , appHandleEvent = \s event -> 
                case event of
                    -- http://hackage.haskell.org/package/brick-0.19/docs/Brick-Types.html#t:BrickEvent
                    -- http://hackage.haskell.org/package/vty-5.15.1/docs/Graphics-Vty-Input-Events.html#t:Key
                    VtyEvent (EvKey (KChar 'q') _) -> Brick.Main.halt s 
                    VtyEvent (EvKey (KChar 'n') _) -> do
                        let S _ stream = s
                        case runIdentity . next $ stream of
                            Left _ -> Brick.Main.halt s
                            Right (dungeon',stream') -> Brick.Main.continue (S dungeon' stream')
                    _ -> Brick.Main.continue s
            , appStartEvent = return
            , appAttrMap = const $ attrMap mempty []
            }
        initialDungeon = 
            DungeonState 
            {
               player = Position 6 7
            ,  treasures = [Position 1 2]
            ,  traps = [Position 8 9]
            }
        initialState = 
            S initialDungeon 
              (void $ flip evalStateT initialDungeon $ runDungeonT $ approachTreasure 0)
    _ <- defaultMain app initialState
    return ()
