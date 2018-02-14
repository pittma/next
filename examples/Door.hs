{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

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

-- Define our egresses as constraints.  This allows us to have multiple outputs from a single
-- input.
class OpenEgress s

class ClosedEgress s

-- Map our states to their egresses
instance OpenEgress 'Closed

instance ClosedEgress 'Open

-- Define our FSM as type family relationships in the context of Next.
type instance Next 'Open = OpenEgress

type instance Next 'Closed = ClosedEgress

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

openDoor :: String -> Door 'Open String
openDoor x = OpenDoor ("dependent " ++ x ++ "! (kind of)")

closeDoor :: String -> Door 'Closed String
closeDoor = ClosedDoor

run :: IO ()
run = do
  let d = fsmPure "types" :: Door 'Closed String
  putStrLn ("Here's our door: " ++ show d)
  putStrLn "Should we open it? (y/n)"
  ans <- getLine
  case ans of
    "y" -> do
      let d' = d ->>= openDoor ->>= closeDoor
      putStrLn
        "Door opened (and then closed again) and we did some computations in the meantime."
      putStrLn "See for yourself:"
      print d'
    "n" -> putStrLn "okay, then. Goodbye!"
    _ -> putStrLn (ans ++ " is not a valid input")
