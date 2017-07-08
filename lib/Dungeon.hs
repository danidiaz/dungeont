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
        runContDungeonT,
        getDungeonCC
    ) where

import Control.Monad.State.Strict
import Control.Monad.Trans.State.Strict
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

-- https://stackoverflow.com/questions/5193876/goto-in-haskell-can-anyone-explain-this-seemingly-insane-effect-of-continuation
-- "The continuation returned by getCC' has not only ContT's state at the point
-- of the call, but also the state of any monad above ContT on the stack. When you
-- restore that state by calling the continuation, all of the monads built above
-- ContT return to their state at the point of the getCC' call."
getDungeonCC ::  a -> ContDungeonT cr m (a,a -> ContDungeonT cr m b)
getDungeonCC x0 = ContDungeonT $
    liftCallCC callCC (\c -> let f x = c (x, ContDungeonT . f) in return (x0, ContDungeonT . f))

instance MonadTrans (ContDungeonT cr) where
    lift = ContDungeonT . lift . lift . lift

instance Monad m => MonadDungeon (ContDungeonT cr m) where
    yieldDungeon = ContDungeonT . lift . lift . yield

