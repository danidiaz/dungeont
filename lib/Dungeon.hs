{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FlexibleContexts #-}

module Dungeon (
        Position(..),
        DungeonState(..),
        MonadDungeon(..),
        -- * Dungeon transformer
        DungeonT(..),
        -- * Magical Cont dungeon transformer
        ContDungeonT(..),
    ) where

import Control.Monad.State
import Control.Monad.Cont
import Streaming.Prelude

data Position = Position { xpos :: Int, ypos :: Int } deriving (Eq,Show)

data DungeonState = Dungeon 
    {
       player :: Position,
       treasures :: [Position],
       traps :: [Position]
    } deriving (Eq,Show)
                    
class MonadState DungeonState m => MonadDungeon m where
    yieldDungeon :: DungeonState -> m ()

newtype DungeonT m r = 
    DungeonT { runDungeonT :: StateT DungeonState (Stream (Of DungeonState) m) r } 
    deriving (Functor,Applicative,Monad,MonadState DungeonState)

instance MonadTrans DungeonT where
    lift = DungeonT . lift . lift

instance Monad m => MonadDungeon (DungeonT m) where
    yieldDungeon = DungeonT . lift . yield
            
newtype ContDungeonT cr m r = 
    ContDungeonT { runContDungeonT :: StateT DungeonState (ContT cr (Stream (Of DungeonState) m)) r } 
    deriving (Functor,Applicative,Monad,MonadState DungeonState,MonadCont)

instance MonadTrans (ContDungeonT cr) where
    lift = ContDungeonT . lift . lift . lift

instance Monad m => MonadDungeon (ContDungeonT cr m) where
    yieldDungeon = ContDungeonT . lift . lift . yield

