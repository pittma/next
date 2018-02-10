Next
====

Next is an adventure in type-level FSMs -- one that has not yet met its destination.

## Overview

The goal of `next` is to be able to construct and traverse a type-level state machine.  This is provided, for now, through the type family `Next` which is used to map valid state transitions.


At the time of this writing, only paths can be validated; `Next` cannot handle vertices with more than one outgoing edge. There is but more discovery to do!


## An Example

The complete, runnable example can be found in [examples](./examples/Door.hs).

```haskell
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

-- Map our transition in terms of Next.
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
```
