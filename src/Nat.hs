-----------------------------------------------------------------------------
--
-- Module      :  Neuro.Util
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  -
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# LANGUAGE ExistentialQuantification #-}

module Nat (

  Nat(..)
, Nat'
, N0
, N1
, N2
, N3

, SomeNat(..)
, Nat2Integral(..)

) where

-----------------------------------------------------------------------------
-- from https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Nat = Zero | Succ Nat

-- from https://hackage.haskell.org/package/HList-0.4.0.0/docs/src/Data-HList-FakePrelude.html#HNat2Integral


class Nat2Integral (n :: Nat) where
    nat2int :: Integral i => Nat' n -> i


instance Nat2Integral Zero where nat2int _ = 0

instance Nat2Integral n => Nat2Integral (Succ n) where
    nat2int n = nat2int (nPred n) + 1

nPred :: Nat' (Succ n) -> Nat' n; nPred _ = undefined

-----------------------------------------------------------------------------

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2

data Nat' (n :: Nat)

data SomeNat = forall n . (Nat2Integral n) => SomeNat (Nat' n)

-----------------------------------------------------------------------------

