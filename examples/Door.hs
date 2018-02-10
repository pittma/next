{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Door where

import Next

-- Enumerate our states.
data DoorSt
  = Open
  | Closed
  deriving (Show)

-- This is our type to be FSM-indexed.
data Door s a where
  OpenDoor :: a -> Door 'Open a
  ClosedDoor :: a -> Door 'Closed a

-- Just so we can show our Door.
deriving instance Show a => Show (Door s a)

-- Define our FSM as type family relationships in the context of Next.
--
-- NB(pittma):
--   This is where we break down at the current juncture. Open TF
--   instances are required to be unique.  This results in us
--   only being able to traverse type-level _paths_, not whole FSMs.
type instance Next 'Open = 'Closed

type instance Next 'Closed = 'Open

-- Write our FSM instances.
instance FSMFunctor Door where
  fsmFmap f (OpenDoor x) = OpenDoor (f x)
  fsmFmap f (ClosedDoor x) = ClosedDoor (f x)

instance FSMApplicative Door where
  type Init Door = 'Closed
  fsmPure = ClosedDoor
  fsmAp (OpenDoor f) (OpenDoor x) = OpenDoor (f x)
  fsmAp (ClosedDoor f) (ClosedDoor x) = ClosedDoor (f x)

instance FSMMonad Door where
  (->>=) (OpenDoor x) f = f x
  (->>=) (ClosedDoor x) f = f x

run :: IO ()
run = do
  let d = fsmPure "types" :: Door 'Closed String
  putStrLn ("Here's our door: " ++ show d)
  putStrLn "Should we open it? (y/n)"
  ans <- getLine
  case ans of
    "y"
      -- Now we're going to open our door!
     -> do
      let d' = d ->>= \x -> OpenDoor ("dependent " ++ x ++ "! (kind of)")
      putStrLn "Door opened!"
      print d'
    "n" -> putStrLn "okay, then. Goodbye!"
    _ -> putStrLn (ans ++ " is not a valid input")
