{-# language ScopedTypeVariables #-}
{-# language NamedFieldPuns #-}

module Main where

import Data.Foldable
import Data.Functor
import Data.Functor.Identity
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Streaming.Prelude (next,Stream,Of(..))
import Control.Monad.ST

import Brick
import qualified Brick.Main

import Graphics.Vty.Image(string,vertCat)
import Graphics.Vty.Attributes(defAttr)
import Graphics.Vty.Input.Events

import Dungeon
import Dungeon.Player.Prelude (approachTreasure,approachTreasureCont)

import System.Environment

initialDungeonState :: DungeonState
initialDungeonState =
    DungeonState 
    {
       player = Position 6 7
    ,  treasures = [Position 1 2]
    ,  traps = [Position 8 9]
    }

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

renderDungeonAsWidget :: DungeonState -> Widget n
renderDungeonAsWidget =
      raw 
    . vertCat 
    . fmap (Graphics.Vty.Image.string defAttr) 
    . renderDungeon

data S a m r = S !a !(Stream (Of a) m r)

main :: IO ()
main = do
    mode : _ <- getArgs
    let progression = void $ (case mode of
            "normal" -> runDungeonT (approachTreasure 0)
            "magical" -> runContDungeonT approachTreasureCont) initialDungeonState
        -- http://hackage.haskell.org/package/brick-0.19/docs/Brick-Main.html#t:App
        app :: App (S DungeonState Identity ()) () () =
            App 
            { 
              appDraw = \(S dungeon _) -> [renderDungeonAsWidget dungeon]
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
    _ <- defaultMain app (S initialDungeonState progression)
    return ()
