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
           , RankNTypes
           , TypeSynonymInstances
           , ConstraintKinds
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
, N7
, N8
, N9
, N10
, N11
, N12
, N13
, N14
, N15
, N16
, N17
, N18
, N19
, N20

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
, NatRules
, NatRules2
, NatRules3
, NatRules3Pack

, SomeNat(..)
, Nat2Integral(..)

, MonoidLike(..)

, NList(..)
, Nats(..)
, (+::)
, MapNats(..)
, NatsUnzip(..)
, NPair(..)

, MkNats
, NatsSum

) where

import Control.Arrow
import Data.Type.Equality

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


--data instance (n :+: Succ Zero) :~: (Succ n)


type NatRules n = ( (n :+: Succ Zero) ~ Succ n
                  , (n :+: Zero) ~ n

                  , (n :*: Succ Zero) ~ n
                  , (n :*: Zero) ~ Zero
--                  , (Succ n) ~ (n :+: Succ Zero)
--                  , NatRules2 n n
--                  , NatRules3 n n n
                  )

type NatRules2 n1 n2 = ( (n1 :+: n2) ~ (n2 :+: n1)
                       , (n1 :+: Succ n2) ~ Succ (n1 :+: n2)

                       , ((n1 :+: N1) :*: n2) ~ (n2 :+: (n1 :*: n2))
                       )

type NatRules3 a b c = ( ((a :*: c) :+: (b :*: c)) ~ ((a :+: b) :*: c)
                       , ((a :+: b) :+: c) ~ (a :+: (b :+: c))
--                       , (a :+: b) :+: c) ~ (a :+: (b :+: c))
--                       , a :+: b :+: c ~
                       )

type NatRules3Pack n = ( NatRules3 N1 N1 n

                       , NatRules3 N1 N2 n
                       , NatRules3 N2 N1 n
                       , NatRules3 N2 N2 n

                       , NatRules3 N1 N3 n
                       , NatRules3 N3 N1 n
                       , NatRules3 N2 N3 n
                       , NatRules3 N3 N2 n
                       , NatRules3 N3 N3 n


                       , NatRules3 N1 N4 n
                       , NatRules3 N4 N1 n
                       , NatRules3 N2 N4 n
                       , NatRules3 N4 N2 n
                       , NatRules3 N4 N3 n
                       , NatRules3 N3 N4 n
                       , NatRules3 N4 N4 n


                       , NatRules3 N1 N5 n
                       , NatRules3 N5 N1 n
                       , NatRules3 N2 N5 n
                       , NatRules3 N5 N2 n
                       , NatRules3 N3 N5 n
                       , NatRules3 N5 N3 n
                       , NatRules3 N5 N4 n
                       , NatRules3 N4 N5 n
                       , NatRules3 N5 N5 n


                       , NatRules3 N1 N6 n
                       , NatRules3 N6 N1 n
                       , NatRules3 N2 N6 n
                       , NatRules3 N6 N2 n
                       , NatRules3 N3 N6 n
                       , NatRules3 N6 N3 n
                       , NatRules3 N4 N6 n
                       , NatRules3 N6 N4 n
                       , NatRules3 N6 N5 n
                       , NatRules3 N5 N6 n
                       , NatRules3 N6 N6 n
                       )

-----------------------------------------------------------------------------

type N0 = Zero
type N1 = Succ N0
type N2 = Succ N1
type N3 = Succ N2
type N4 = Succ N3
type N5 = Succ N4
type N6 = Succ N5
type N7 = Succ N6
type N8 = Succ N7
type N9 = Succ N8
type N10 = Succ N9
type N11 = Succ N10
type N12 = Succ N11
type N13 = Succ N12
type N14 = Succ N13
type N15 = Succ N14
type N16 = Succ N15
type N17 = Succ N16
type N18 = Succ N17
type N19 = Succ N18
type N20 = Succ N19

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

class MonoidLike a where mempty'  :: a N0 x
                         mappend' :: a n1 x -> a n2 x -> a (n1 :+: n2) x
                         mconcat' :: a m (a n x) -> a (m :*: n) x

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------

data NList = NNil | NCons Nat NList

data family Nats (a :: Nat -> *) (l :: NList)

data instance Nats a NNil           = NatsNil
data instance Nats a (x `NCons` xs) = NatsCons (a x) (Nats a xs)

type family MkNats (n :: Nat) (l :: Nat) tail where
    MkNats n (Succ l') tail = NCons n (MkNats n l' tail)
    MkNats n Zero tail      = tail


infixr 4 +::
(+::) :: a n -> Nats a ns -> Nats a (NCons n ns)
(+::) = NatsCons

-----------------------------------------------------------------------------n)

newtype NPair a b (n :: Nat) = NPair (a n, b n)

class NatsUnzip ns where natsUnzip :: Nats (NPair a b) ns -> (Nats a ns, Nats b ns)

instance NatsUnzip NNil where natsUnzip _ = (NatsNil, NatsNil)
instance (NatsUnzip ns) => NatsUnzip (NCons n ns) where
    natsUnzip (NatsCons (NPair (a,b)) t) = first (a +::) $ second (b +::)
                                         $ natsUnzip t

-----------------------------------------------------------------------------

type NatDepTransform a b = forall (n :: Nat) . a n -> b n

class MapNats ns where mapNats :: NatDepTransform a b -> Nats a ns -> Nats b ns

instance MapNats NNil where mapNats _ _ = NatsNil
instance (MapNats ns) =>MapNats (NCons n ns) where
    mapNats f (NatsCons h t) = f h +:: mapNats f t

-----------------------------------------------------------------------------

type family NatsSum (l :: NList) :: Nat where
    NatsSum (NCons n t) = n :+: NatsSum t
    NatsSum NNil        = N0

-----------------------------------------------------------------------------

