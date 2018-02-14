{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module VM where

import Next

data VMSt
  = Off
  | Running
  | Stopped
  | Killed
  deriving (Show)

data VM s a where
  OffVM :: a -> VM 'Off a
  RunningVM :: a -> VM 'Running a
  KilledVM :: a -> VM 'Killed a
  StoppedVM :: a -> VM 'Stopped a

deriving instance Show a => Show (VM s a)

-- Here define the FSM:
-- Off -> Running ->  Stopped
--                \
--                 -> Killed
-- Using the *Egress types for valid output states.
class OffEgress s

instance OffEgress 'Running

type instance Next 'Off = OffEgress

class RunningEgress s

instance RunningEgress 'Killed

instance RunningEgress 'Stopped

type instance Next 'Running = RunningEgress

instance FSMFunctor VM where
  fsmFmap f (OffVM x) = OffVM (f x)
  fsmFmap f (RunningVM x) = RunningVM (f x)
  fsmFmap f (KilledVM x) = KilledVM (f x)
  fsmFmap f (StoppedVM x) = StoppedVM (f x)

instance FSMApplicative VM where
  type Init VM = 'Off
  fsmPure = OffVM
  fsmAp (OffVM f) (OffVM x) = OffVM (f x)
  fsmAp (RunningVM f) (RunningVM x) = RunningVM (f x)
  fsmAp (KilledVM f) (KilledVM x) = KilledVM (f x)
  fsmAp (StoppedVM f) (StoppedVM x) = StoppedVM (f x)

instance FSMMonad VM where
  (OffVM x) ->>= f = f x
  (RunningVM x) ->>= f = f x
  (KilledVM x) ->>= f = f x
  (StoppedVM x) ->>= f = f x

-- Now we can compose transitions which will get compile-time assertion on their validity.
-- This FSM in particular has 2 valid paths.
runNKill :: VM 'Off () -> VM 'Killed ()
runNKill vm = vm ->>= RunningVM ->>= KilledVM

runNStop :: VM 'Off () -> VM 'Stopped ()
runNStop vm = vm ->>= RunningVM ->>= StoppedVM

run :: IO ()
run = do
  let vm = OffVM ()
  putStrLn ("Here's our VM: " ++ show vm)
  putStrLn "Do you want to it to stop, or die (stop/die)?"
  resp <- getLine
  case resp of
    "stop" -> do
      let vm' = runNStop vm
      putStrLn ("It's stopped: " ++ show vm')
    "die" -> do
      let vm' = runNKill vm
      putStrLn ("You killed it: " ++ show vm')
    _ -> putStrLn (resp ++ " is not a valid input")
