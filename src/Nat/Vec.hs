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
       #-}


module Nat.Vec (

  Vec(..)
, Vec1
, Vec2
, Vec3

, GenVec(..)
, (+:)
, vec2list
, vecZip, vecZip'


, VecElem(..)
, vecElem1
, vecElem2
, vecElem3

, module Nat

) where

import Nat


-----------------------------------------------------------------------------
-- from https://downloads.haskell.org/~ghc/7.4.1/docs/html/users_guide/kind-polymorphism-and-promotion.html

data Vec :: Nat -> * -> * where
  VNil  :: Vec Zero a
  VCons :: a -> Vec n a -> Vec (Succ n) a

-----------------------------------------------------------------------------

vec2list :: Vec n a -> [a]
vec2list (VCons h t) = h : vec2list t
vec2list VNil = []

vecZip :: Vec n a -> [b] -> Vec n (a, b)
vecZip (VCons a as) (b:bs) = (a, b) +: vecZip as bs
vecZip VNil _ = VNil
vecZip _ []   = error "vecZip: list is shorter than vector"

vecZip' :: [b] -> Vec n a -> Vec n (b, a)
vecZip' (b:bs) (VCons a as) = (b, a) +: vecZip' bs as
vecZip' _ VNil = VNil
vecZip' [] _   = error "vecZip': list is shorter than vector"

-----------------------------------------------------------------------------


instance Functor (Vec n) where fmap f (VCons h t) = VCons (f h) (fmap f t)
                               fmap _ VNil = VNil

instance Foldable (Vec n) where foldr _ b0 VNil = b0
                                foldr f b0 (VCons a t) = let res = f a b0
                                                         in foldr f res t

class GenVec (n :: Nat) where genVec :: (Int -> a) -> Nat' n ->  Vec n a

instance GenVec N0 where genVec _ _ = VNil
instance (GenVec np, Nat2Integral np) => GenVec (Succ np) where
    genVec f n = f (nat2int n) +: genVec f undefined


infixr 5 +:
(+:) :: a -> Vec n a -> Vec (Succ n) a
(+:) = VCons

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




