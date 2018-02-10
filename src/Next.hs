{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}

module Next where

type family Next (s :: k) :: k

-- There's some unsoundness here, I fear, but it's soundiness with the best intentions.
-- A Functor is meant to be structure preserving, so it does not transition the state.
-- However, this Functor's structure is not properly hidden within a partially applied type.
-- All of of FSMFunctor's type arity is laid bare: k -> * -> *
class FSMFunctor (f :: k -> * -> *) where
  fsmFmap :: (a -> b) -> f s a -> f s b

-- Like its superclass, FSMApplicative is also structure preserving.
class FSMFunctor f =>
      FSMApplicative (f :: k -> * -> *) where
  type Init f :: k
  fsmPure :: a -> f (Init f) a
  fsmAp :: f s (a -> b) -> f s a -> f s b

-- Aha! Finally a use of our eponymous type family.  Like Indexed Monads, FSMMonad
-- is the only one in the FSM F-A -M stack that transitions the state indices.
class FSMApplicative m =>
      FSMMonad (m :: k -> * -> *) where
  (->>=) :: m s a -> (a -> m (Next s) b) -> m (Next s) b

infixl 3 ->>=
