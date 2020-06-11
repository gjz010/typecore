-- |
-- Module      : GHC.TypeLits.Induction
-- Description : Induction for GHC TypeLits
-- Copyright   : (C) 2017-2018 mniip
-- License     : MIT
-- Maintainer  : mniip@mniip.com
-- Stability   : stable
-- Portability : portable
{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, PolyKinds, GADTs, RankNTypes, StandaloneDeriving, InstanceSigs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module GHC.TypeLits.Induction
        (
                -- * Natural number induction
                induceIsZero,
                inducePeano,
                induceTwosComp,
                induceBaseComp,

                -- * Positive number induction
                induceUnary,
                inducePosBinary,
                inducePosBase
        )
        where

import Data.Proxy
import GHC.TypeLits
import GHC.TypeLits.Singletons
import Prelude
-- | \((\forall n. n > 0 \to P(n)) \to P(0) \to \forall m. P(m)\)
induceIsZero :: KnownNat m => (forall n. KnownNat n => p (1 + n)) -> p 0 -> p m
induceIsZero = go natSingleton
        where
                go :: NatIsZero m -> (forall n. KnownNat n => p (1 + n)) -> p 0 -> p m
                go IsZero y z = z
                go IsNonZero y z = y

-- | \((\forall n. P(n) \to P(n + 1)) \to P(0) \to \forall m. P(m)\)
--
-- For example:
--
-- > inducePeano f z :: p 3 = f (f (f z))
inducePeano :: KnownNat m => (forall n. KnownNat n => p n -> p (1 + n)) -> p 0 -> p m
inducePeano = go natSingleton
        where
                go :: NatPeano m -> (forall n. KnownNat n => p n -> p (1 + n)) -> p 0 -> p m
                go PeanoZero f z = z
                go (PeanoSucc n) f z = f (go n f z)

-- | \((\forall n. P(n) \to P(2n + 1)) \to \\ (\forall n. P(n) \to P(2n + 2)) \to \\ P(0) \to \\ \forall m. P(m)\)
--
-- For example:
--
-- > induceTwosComp f1 f2 z :: p 12 = f2 (f1 (f2 z))
induceTwosComp :: KnownNat m => (forall n. KnownNat n => p n -> p (1 + 2 GHC.TypeLits.* n)) -> (forall n. KnownNat n => p n -> p (2 + 2 GHC.TypeLits.* n)) -> p 0 -> p m
induceTwosComp = go natSingleton
        where
                go :: NatTwosComp m -> (forall n. KnownNat n => p n -> p (1 + 2 GHC.TypeLits.* n)) -> (forall n. KnownNat n => p n -> p (2 + 2 GHC.TypeLits.* n)) -> p 0 -> p m
                go TwosCompZero f g z = z
                go (TwosCompx2p1 n) f g z = f (go n f g z)
                go (TwosCompx2p2 n) f g z = g (go n f g z)

-- | \(\forall b. (\prod_d Q(d)) \to \\ (\forall n. \forall d. d < b \to Q(d) \to P(n) \to P(bn + d + 1)) \to \\ \forall m. P(m)\)
--
-- For example:
--
-- > induceBaseComp (_ :: s q) (_ :: r 10) d f z :: p 123 = (d :: q 2) `f` ((d :: q 1) `f` ((d :: q 0) `f` z)
--
-- The @s q@ parameter is necessary because presently GHC is unable to unify @q@ under the equational constraint @1 + k <= b@.
induceBaseComp :: forall r b q p m c s. (KnownNat b, b ~ (1 + c), KnownNat m) => s q -> r b -> (forall k. (KnownNat k, 1 + k <= b) => q k) -> (forall k n. (KnownNat k, 1 + k <= b, KnownNat n) => q k -> p n -> p (1 + k + b GHC.TypeLits.* n)) -> p 0 -> p m
induceBaseComp _ _ = go natSingleton
        where
                go :: forall m. NatBaseComp Proxy b m -> (forall k. (KnownNat k, 1 + k <= b) => q k) -> (forall k n. (KnownNat k, 1 + k <= b, KnownNat n) => q k -> p n -> p (1 + k + b GHC.TypeLits.* n)) -> p 0 -> p m
                go BaseCompZero q f z = z
                go (BaseCompxBp1p k n) q f z = f q (go n q f z)

-- | \((\forall n. P(n) \to P(n + 1)) \to P(1) \to \forall m > 0. P(m)\)
--
-- For example:
--
-- > induceUnary f o :: p 5 = f (f (f (f o)))
induceUnary :: KnownNat m => (forall n. KnownNat n => p n -> p (1 + n)) -> p 1 -> p (1 + m)
induceUnary = go posSingleton
        where
                go :: Unary m -> (forall n. KnownNat n => p n -> p (1 + n)) -> p 1 -> p m
                go UnaryOne f o = o
                go (UnarySucc n) f o = f (go n f o)

-- | \((\forall n. P(n) \to P(2n)) \to \\ (\forall n. P(n) \to P(2n + 1)) \to \\ P(1) \to \\ \forall m > 0. P(m)\)
--
-- For example:
--
-- > inducePosBinary f0 f1 o :: p 10 = f0 (f1 (f0 o))
inducePosBinary :: KnownNat m => (forall n. KnownNat n => p n -> p (2 GHC.TypeLits.* n)) -> (forall n. KnownNat n => p n -> p (1 + 2 GHC.TypeLits.* n)) -> p 1 -> p (1 + m)
inducePosBinary = go posSingleton
        where
                go :: PosBinary m -> (forall n. KnownNat n => p n -> p (2 GHC.TypeLits.* n)) -> (forall n. KnownNat n => p n -> p (1 + 2 GHC.TypeLits.* n)) -> p 1 -> p m
                go BinOne f g z = z
                go (Bin0 n) f g z = f (go n f g z)
                go (Bin1 n) f g z = g (go n f g z)

-- | \(\forall b. (\prod_d Q(d)) \to \\ (\forall n. \forall d. d < b \to Q(d) \to P(n) \to P(bn + d)) \to \\ (\forall d. d > 0 \to d < b \to Q(d) \to P(d)) \to \\ \forall m > 0. P (m)\)
--
-- For example:
--
-- > inducePosBase (_ :: s q) (_ :: r 10) d f l :: p 123 = (d :: q 3) `f` ((d :: q 2) `f` l (d :: q 1))
--
-- The @s q@ parameter is necessary because presently GHC is unable to unify @q@ under the equational constraint @1 + k <= b@.
inducePosBase :: forall r b q p m c s. (KnownNat b, b ~ (2 + c), KnownNat m) => s q -> r b -> (forall k. (KnownNat k, 1 + k <= b) => q k) -> (forall k n. (KnownNat k, 1 + k <= b, KnownNat n) => q k -> p n -> p (k + b GHC.TypeLits.* n)) -> (forall k n. (KnownNat k, 1 + k <= b, k ~ (1 + n)) => q k -> p k) -> p (1 + m)
inducePosBase _ _ = go posSingleton
        where
                go :: forall m. PosBase Proxy b m  -> (forall k. (KnownNat k, 1 + k <= b) => q k) -> (forall k n. (KnownNat k, 1 + k <= b, KnownNat n) => q k -> p n -> p (k + b GHC.TypeLits.* n)) -> (forall k n. (KnownNat k, 1 + k <= b, k ~ (1 + n)) => q k -> p k) -> p m
                go (BaseLead n) q f g = g q
                go (BaseDigit k n) q f g = f q (go n q f g)