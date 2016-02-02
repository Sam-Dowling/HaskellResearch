{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Control.Concurrent.STM
import Control.Monad

data Item = Scroll
          | Wand
          | Banjo
            deriving (Eq, Ord, Show)

newtype Gold = Gold Int
               deriving (Eq, Ord, Show, Num)

newtype HitPoint = HitPoint Int
               deriving (Eq, Ord, Show, Num)

type Inventory = TVar [Item]
type Health = TVar HitPoint
type Balance = TVar Gold

data Player = Player {
            balance :: Balance,
            health :: Health,
            inventory :: Inventory
            }            

basicTransfer qty fromBal toBal = do
        fromQty <- readTVar fromBal
        toQty   <- readTVar toBal
        writeTVar fromBal (fromQty - qty)
        writeTVar toBal   (toQty + qty)


transferTest = do
        alice <- newTVar (12 :: Gold)
        bob   <- newTVar 4
        basicTransfer 3 alice bob
        liftM2 (,) (readTVar alice) (readTVar bob)
