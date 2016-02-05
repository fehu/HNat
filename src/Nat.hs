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

{-# LANGUAGE ExistentialQuantification
           , TypeFamilies
           , UndecidableInstances
           #-}

module Nat (

  Nat(..)
, Nat'
, N0
, N1
, N2
, N3
, N4
, N5
, N6

, nat0
, nat1
, nat2
, nat3
, nat4
, nat5
, nat6

, (:+:)
, (:*:)
, (:^:)

, SomeNat(..)
, Nat2Integral(..)

, NList(..)
, Nats(..)
, NatsSum

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

type family (:+:) (n1 :: Nat) (n2 :: Nat) :: Nat where
    Succ a :+: b = Succ (a :+: b)
    Zero   :+: b = b

type family (:*:) (n1 :: Nat) (n2 :: Nat) :: Nat where
    Succ a :*: b = b :+: (a :*: b)
    Zero :*: b   = Zero

type family (:^:) (n :: Nat) (pow :: Nat) :: Nat where
    a :^: Succ b = a :*: (a :^: b)
    a :^: Zero   = N1

-----------------------------------------------------------------------------

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5

data Nat' (n :: Nat)

nat0 = undefined :: Nat' N0
nat1 = undefined :: Nat' N1
nat2 = undefined :: Nat' N2
nat3 = undefined :: Nat' N3
nat4 = undefined :: Nat' N4
nat5 = undefined :: Nat' N5
nat6 = undefined :: Nat' N6

instance (Nat2Integral n) => Show (Nat' n) where show = show . nat2int

data SomeNat = forall n . (Nat2Integral n) => SomeNat (Nat' n)

-----------------------------------------------------------------------------

data NList = NNil | NCons Nat NList

data family Nats (a :: Nat -> *) (l :: NList)

data instance Nats a NNil           = NatsNil
data instance Nats a (x `NCons` xs) = NatsCons (a x) (Nats a xs)

type family NatsSum (l :: NList) where
    NatsSum (NCons n t) = n :+: NatsSum t
    NatsSum NNil        = N0

-----------------------------------------------------------------------------


