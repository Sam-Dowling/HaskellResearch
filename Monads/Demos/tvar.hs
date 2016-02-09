import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM

type Account = TVar Double

transfer :: Account -> Account -> Double -> STM ()
transfer from to amount = do
    availiable <- readTVar from
    when (amount > availiable) retry
    modifyTVar from (+ (-amount))
    modifyTVar to   (+   amount)

actions :: Account -> Account -> [IO ThreadId]
actions a b = map forkIO [
       atomically (transfer a b 10)
     , atomically (transfer a b (-20))
     , atomically (transfer a b 30)
   ]


main = do
  accountA <- atomically $ newTVar 60
  accountB <- atomically $ newTVar 0

  sequence_ (actions accountA accountB)

  balanceA <- atomically $ readTVar accountA
  balanceB <- atomically $ readTVar accountB
  return balanceA

  print $ "BalanceA " ++ (show balanceA)
  print $ "BalanceB " ++ (show balanceB)
