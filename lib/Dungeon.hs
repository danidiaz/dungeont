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
        DungeonT,
        runDungeonT,
        -- * Magical Cont dungeon transformer
        ContDungeonT,
        runContDungeonT
    ) where

import Control.Monad.State.Strict
import Control.Monad.Cont
import Streaming.Prelude

-- TODO: Use the linear package.
data Position = Position { xpos :: Int, ypos :: Int } deriving (Eq,Show)

data DungeonState = DungeonState 
    {
       player :: !Position,
       treasures :: ![Position],
       traps :: ![Position]
    } deriving (Eq,Show)
                    
class MonadState DungeonState m => MonadDungeon m where
    yieldDungeon :: DungeonState -> m ()

newtype DungeonT m r = 
    DungeonT { getDungeonT :: StateT DungeonState (Stream (Of DungeonState) m) r } 
    deriving (Functor,Applicative,Monad,MonadState DungeonState)

runDungeonT :: Monad m => DungeonT m r -> DungeonState -> Stream (Of DungeonState) m r
runDungeonT d s = flip evalStateT s . getDungeonT $ d

instance MonadTrans DungeonT where
    lift = DungeonT . lift . lift

instance Monad m => MonadDungeon (DungeonT m) where
    yieldDungeon = DungeonT . lift . yield
            
newtype ContDungeonT cr m r = 
    ContDungeonT { getContDungeonT :: StateT DungeonState (ContT cr (Stream (Of DungeonState) m)) r } 
    deriving (Functor,Applicative,Monad,MonadState DungeonState,MonadCont)

runContDungeonT :: Monad m => ContDungeonT r m r -> DungeonState -> Stream (Of DungeonState) m r
runContDungeonT d s = flip runContT return . flip evalStateT s . getContDungeonT $ d

instance MonadTrans (ContDungeonT cr) where
    lift = ContDungeonT . lift . lift . lift

instance Monad m => MonadDungeon (ContDungeonT cr m) where
    yieldDungeon = ContDungeonT . lift . lift . yield

