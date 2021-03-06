-----------------------------------------------------------------------------
--
-- Module      :  Nat.Vec
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

{-# LANGUAGE GADTs
           , TypeSynonymInstances
           , FlexibleInstances
           , FlexibleContexts
           , RankNTypes
           , ConstraintKinds
           , BangPatterns
       #-}


module Nat.Vec (

  Vec(..)
, Vec1
, Vec2
, Vec3

, strictVec

, GenVec(..)
, vec2list
, (+:)
, vecZip, vecZip'
, vecsZip
, vecConcat, (+:+)
, vecsConcat
, vecCombine
, vecsFoldr
, vecAccMap
, vecReverse

, VecElem(..)
, vecElem1
, vecElem2
, vecElem3

, FlippedVec(..)
, Vec2d
, v2Nil
--, MergeVec2d(..)
, Vec2dFromNats(..)
, NatsFlatten(..)

, module Nat
, NatsOps

) where

import Nat

-----------------------------------------------------------------------------
-- from https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Vec :: Nat -> * -> * where
  VNil  :: Vec Zero a
  VCons :: a -> Vec n a -> Vec (Succ n) a

newtype FlippedVec a (n :: Nat) = FlippedVec { flippedVec :: Vec n a }


instance (Show a) => Show (Vec n a) where
    show v = let vs = foldr (\ x acc -> acc ++ "," ++ show x) "" v
           in "Vec<" ++ tail vs ++ ">"

strictVec :: Vec n a -> Vec n a
strictVec (VCons !h t) = let !t' = strictVec t
                        in h +: t'
strictVec VNil = VNil

-----------------------------------------------------------------------------

vec2list :: Vec n a -> [a]
vec2list (VCons h t) = h : vec2list t
vec2list VNil = []

-- | Prepend a value to a vec.
infixr 5 +:
(+:) :: a -> Vec n a -> Vec (Succ n) a
(+:) = VCons

-- | Append a value to a vec.
vecAppend :: Vec n a -> a -> Vec (Succ n) a
vecAppend (VCons h t) a = h +: vecAppend t a
vecAppend VNil        a = a +: VNil

vecZip :: Vec n a -> [b] -> Vec n (a, b)
vecZip (VCons a as) (b:bs) = (a, b) +: vecZip as bs
vecZip VNil _ = VNil
vecZip _ []   = error "vecZip: list is shorter than vector"

vecZip' :: [b] -> Vec n a -> Vec n (b, a)
vecZip' (b:bs) (VCons a as) = (b, a) +: vecZip' bs as
vecZip' _ VNil = VNil
vecZip' [] _   = error "vecZip': list is shorter than vector"

vecsZip :: Vec n a -> Vec n b -> Vec n (a, b)
vecsZip (VCons a as) (VCons b bs) = VCons (a, b) $ vecsZip as bs
vecsZip VNil VNil = VNil

vecConcat :: Vec n1 a -> Vec n2 a -> Vec (n1 :+: n2) a
vecConcat (VCons h t) v2 = h +: vecConcat t v2
vecConcat VNil v2 = v2

infixr 4 +:+
(+:+) = vecConcat

vecsConcat :: Vec l (Vec n a) -> Vec (l :*: n) a
vecsConcat (VCons h t) = h +:+ vecsConcat t
vecsConcat VNil = VNil

vecCombine :: (a -> b -> c) -> Vec n a -> Vec n b -> Vec n c
vecCombine f v1 = fmap (uncurry f) . vecsZip v1

vecsFoldr :: (a -> b -> b) -> Vec n b -> [Vec n a] -> Vec n b
vecsFoldr f = foldr (vecCombine f)

vecAccMap :: (a -> b -> a) -> a -> Vec n b -> Vec n a
vecAccMap f a (VCons b bs) = let a' = f a b
                           in a' +: vecAccMap f a' bs
vecAccMap _ _ VNil = VNil

vecReverse :: (NatRules n) => Vec n a -> Vec n a
vecReverse (VCons h t) = vecReverse t +:+ VCons h VNil
vecReverse VNil        = VNil

-----------------------------------------------------------------------------


instance Functor (Vec n) where fmap f (VCons h t) = VCons (f h) (fmap f t)
                               fmap _ VNil = VNil

instance Foldable (Vec n) where foldr _ b0 VNil = b0
                                foldr f b0 (VCons a t) = let res = f a b0
                                                         in foldr f res t

instance MonoidLike Vec where mempty'  = VNil
                              mappend' = (+:+)
                              mconcat' = vecsConcat

-----------------------------------------------------------------------------

class GenVec (n :: Nat) where genVec     :: (Int -> a) -> Nat' n -> Vec n a
                              genVecFrom :: Nat' n -> [a] -> Vec n a

class GenVec' (n :: Nat) where genVec'   :: Int -> (Int -> a) -> Nat' n -> Vec n a

instance GenVec' N0 where genVec' _ _ _ = VNil
instance (GenVec' np, Nat2Integral np) => GenVec' (Succ np) where
    genVec' nm f n = f (nm - nat2int n + 1) +: genVec' nm f undefined

instance GenVec N0 where genVec _ _ = VNil
                         genVecFrom _ _ = VNil
instance (GenVec np, GenVec' np, Nat2Integral np) => GenVec (Succ np) where
    genVec f n = genVec' (nat2int n) f n
    genVecFrom _ (h:t) = h +: genVecFrom undefined t

-----------------------------------------------------------------------------

type Vec1 a = Vec N1 a
type Vec2 a = Vec N2 a
type Vec3 a = Vec N3 a

-----------------------------------------------------------------------------

class VecElem (n :: Nat) (vn :: Nat) where vecElem :: Nat' n -> Vec vn a -> a

instance VecElem N1 N1 where vecElem _ (VCons x _) = x

instance VecElem N1 N2 where vecElem _ (VCons x _) = x
instance VecElem N2 N2 where vecElem _ (VCons _ (VCons x _)) = x

instance VecElem N1 N3 where vecElem _ (VCons x _) = x
instance VecElem N2 N3 where vecElem _ (VCons _ (VCons x _)) = x
instance VecElem N3 N3 where vecElem _ (VCons _ (VCons _ (VCons x _))) = x


vecElem1 :: (VecElem N1 vn) => Vec vn a -> a
vecElem1 = vecElem (undefined :: Nat' N1)

vecElem2 :: (VecElem N2 vn) => Vec vn a -> a
vecElem2 = vecElem (undefined :: Nat' N2)

vecElem3 :: (VecElem N3 vn) => Vec vn a -> a
vecElem3 = vecElem (undefined :: Nat' N3)

-----------------------------------------------------------------------------

type ToVec a b = forall (n :: Nat) . a n -> Vec n b

class Vec2dFromNats ns where vec2dFromNats :: ToVec a b -> Nats a ns -> Vec2d ns b

instance Vec2dFromNats NNil where vec2dFromNats _ _ = v2Nil
instance (Vec2dFromNats ns') => Vec2dFromNats (NCons n ns') where
    vec2dFromNats f (NatsCons h t) = FlippedVec (f h) +:: vec2dFromNats f t


type Vec2d ns a = Nats (FlippedVec a) ns

v2Nil = NatsNil

-----------------------------------------------------------------------------

class NatsFlatten ns where natsFlatten :: ToVec a b -> Nats a ns -> Vec (NatsSum ns) b

instance NatsFlatten NNil where natsFlatten _ _ = VNil
instance (NatsFlatten ns, NatRules2 n (NatsSum ns)) => NatsFlatten (NCons n ns) where
    natsFlatten f (NatsCons h t) =  natsFlatten f t +:+ f h

-----------------------------------------------------------------------------

type NatsOps ns = (MapNats ns, NatsUnzip ns, NatsFlatten ns)
