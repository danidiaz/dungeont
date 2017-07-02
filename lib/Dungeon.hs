{-# language GeneralizedNewtypeDeriving #-}
{-# language StandaloneDeriving #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}

module Dungeon (
        Position(..),
        DungeonState(..),
        DungeonT(..),
        SimpleDungeonT,
        ContDungeonT,
        yieldDungeon
    ) where

import Control.Monad.State
import Control.Monad.Trans.Identity
import Control.Monad.Cont
import Streaming.Prelude

data Position = Position { xpos :: Int, ypos :: Int } deriving (Eq,Show)

data DungeonState = Dungeon 
    {
       player :: Position,
       treasures :: [Position],
       traps :: [Position]
    } deriving (Eq,Show)
                    
newtype DungeonT t m r = 
    DungeonT { runDungeonT :: StateT DungeonState (t (Stream (Of DungeonState) m)) r } 

type SimpleDungeonT = DungeonT IdentityT

type ContDungeonT cr = DungeonT (ContT cr)

deriving instance (Monad (t (Stream (Of DungeonState) m))) => Functor (DungeonT t m)
deriving instance (Monad (t (Stream (Of DungeonState) m))) => Applicative (DungeonT t m)
deriving instance (Monad (t (Stream (Of DungeonState) m))) => Monad (DungeonT t m)
deriving instance (Monad (t (Stream (Of DungeonState) m))) => MonadState DungeonState (DungeonT t m)
deriving instance (MonadCont (t (Stream (Of DungeonState) m))) => MonadCont (DungeonT t m)

yieldDungeon :: (MonadTrans t,Monad (t (Stream (Of DungeonState) m)),Monad m) => DungeonState -> DungeonT t m ()
yieldDungeon = DungeonT . lift . lift . yield 


