-- |
-- Module      : GHC.TypeLits.Singletons
-- Description : Singletons for GHC TypeLits
-- Copyright   : (C) 2017-2018 mniip
-- License     : MIT
-- Maintainer  : mniip@mniip.com
-- Stability   : stable
-- Portability : portable
{-# LANGUAGE TypeOperators, KindSignatures, DataKinds, PolyKinds, GADTs, RankNTypes, StandaloneDeriving, InstanceSigs, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module GHC.TypeLits.Singletons
        (
                -- * Natural number singletons
                NatSingleton(..),
                NatIsZero(..),
                NatPeano(..),
                NatTwosComp(..),
                NatBaseComp(..),

                -- * Positive number singleton
                PositiveSingleton(..),
                Unary(..),
                PosBinary(..),
                PosBase(..),

                ShowN(..)
        )
        where

import GHC.TypeLits
import Data.Type.Equality
import Unsafe.Coerce
import Data.Proxy
import Data.Kind
import Prelude

-- | A class of singletons capable of representing any 'KnownNat' natural number.
class NatSingleton (p :: Nat -> Type) where
        natSingleton :: KnownNat n => p n

-- | Auxiliary class for implementing 'Show' on higher-kinded singletons.
class ShowN (p :: Nat -> Type) where showsPrecN :: Int -> p n -> ShowS

instance ShowN Proxy where showsPrecN = showsPrec

instance NatSingleton Proxy where
        natSingleton = Proxy

-- | A natural number is either 0 or 1 plus something.
data NatIsZero (n :: Nat) where
        IsZero :: NatIsZero 0
        IsNonZero :: KnownNat n => NatIsZero (1 + n)
deriving instance Show (NatIsZero n)
instance ShowN NatIsZero where showsPrecN = showsPrec

instance NatSingleton NatIsZero where
        natSingleton :: forall n. KnownNat n => NatIsZero n
        natSingleton = case natVal (Proxy :: Proxy n) of
                0 -> (unsafeCoerce :: NatIsZero 0 -> NatIsZero n) IsZero
                n -> case someNatVal (n - 1) of
                        Just (SomeNat (p :: Proxy m)) -> (unsafeCoerce :: NatIsZero (1 + m) -> NatIsZero n) $ IsNonZero
                        Nothing -> error $ "Malformed KnownNat instance: " ++ show n

-- | A natural number is either 0 or a successor of another natural number.
--
-- For example, @3 = 1 + (1 + (1 + 0))@:
--
-- > PeanoSucc (PeanoSucc (PeanoSucc PeanoZero)) :: NatPeano 3
data NatPeano (n :: Nat) where
        PeanoZero :: NatPeano 0
        PeanoSucc :: KnownNat n => NatPeano n -> NatPeano (1 + n)
deriving instance Show (NatPeano n)
instance ShowN NatPeano where showsPrecN = showsPrec

instance NatSingleton NatPeano where
        natSingleton :: forall n. KnownNat n => NatPeano n
        natSingleton = case natSingleton :: NatIsZero n of
                IsZero -> PeanoZero
                IsNonZero -> PeanoSucc natSingleton

-- | A natural number is either 0, or twice another natural number plus 1 or 2.
--
-- For example, @12 = 2 + 2 * (1 + 2 * (2 + 2 * 0))@:
--
-- > TwosCompx2p2 (TwosCompx2p1 (TwosCompx2p2 TwosCompZero)) :: NatTwosComp 12
data NatTwosComp (n :: Nat) where
        TwosCompZero :: NatTwosComp 0
        TwosCompx2p1 :: KnownNat n => NatTwosComp n -> NatTwosComp (1 + 2 GHC.TypeLits.* n)
        TwosCompx2p2 :: KnownNat n => NatTwosComp n -> NatTwosComp (2 + 2 GHC.TypeLits.* n)
deriving instance Show (NatTwosComp n)
instance ShowN NatTwosComp where showsPrecN = showsPrec

instance NatSingleton NatTwosComp where
        natSingleton :: forall n. KnownNat n => NatTwosComp n
        natSingleton = case natVal (Proxy :: Proxy n) of
                0 -> (unsafeCoerce :: NatTwosComp 0 -> NatTwosComp n) TwosCompZero
                n -> case someNatVal ((n - 1) `div` 2) of
                        Just (SomeNat (p :: Proxy m)) -> if even n
                                then (unsafeCoerce :: NatTwosComp (2 + 2 GHC.TypeLits.* m) -> NatTwosComp n) $ TwosCompx2p2 natSingleton
                                else (unsafeCoerce :: NatTwosComp (1 + 2 GHC.TypeLits.* m) -> NatTwosComp n) $ TwosCompx2p1 natSingleton
                        Nothing -> error $ "Malformed KnownNat instance: " ++ show n

-- | A natural number is either 0, or @b@ times another natural number plus a digit in @[1, b]@.
--
-- For example, @123 = (2 + 1) + 10 * ((1 + 1) + 10 * ((0 + 1) + 10 * 0))@:
--
-- > BaseCompxBp1p (natSingleton :: p 2) (BaseCompxBp1p (natSingleton :: p 1) (BaseCompxBp1p (natSingleton :: p 0) BaseCompZero)) :: NatBaseComp p 10 123
data NatBaseComp (p :: Nat -> Type) (b :: Nat) (n :: Nat) where
        BaseCompZero :: (KnownNat b, b ~ (1 + c)) => NatBaseComp p b 0
        BaseCompxBp1p :: (KnownNat b, b ~ (1 + c), KnownNat k, 1 + k <= b, KnownNat n) => p k -> NatBaseComp p b n -> NatBaseComp p b (1 + k + b GHC.TypeLits.* n)
instance ShowN p => Show (NatBaseComp p b n) where
        showsPrec d BaseCompZero = showString "BaseCompZero"
        showsPrec d (BaseCompxBp1p a b) = showParen (d > 10) $ showString "BaseCompxBp1p " . showsPrecN 11 a . showString " " . showsPrec 11 b
instance ShowN p => ShowN (NatBaseComp p b) where showsPrecN = showsPrec

instance (KnownNat b, b ~ (1 + c), NatSingleton p) => NatSingleton (NatBaseComp p b) where
        natSingleton :: forall n. KnownNat n => NatBaseComp p b n
        natSingleton = case natVal (Proxy :: Proxy n) of
                0 -> (unsafeCoerce :: NatBaseComp p b 0 -> NatBaseComp p b n) BaseCompZero
                n -> case someNatVal ((n - 1) `div` base) of
                        Just (SomeNat (p :: Proxy m)) -> case someNatVal ((n - 1) `mod` base) of
                                Just (SomeNat (p :: Proxy k)) -> (unsafeCoerce :: NatBaseComp p b (1 + k + b GHC.TypeLits.* m) -> NatBaseComp p b n) $ case unsafeCoerce Refl :: (1 + k <=? b) :~: True of
                                        Refl -> BaseCompxBp1p (natSingleton :: p k) (natSingleton :: NatBaseComp p b m)
                                Nothing -> error $ "Malformed KnownNat instance: " ++ show base
                        Nothing -> error $ "Malformed KnownNat instance: " ++ show n
                where
                        base = natVal (Proxy :: Proxy b)

-- | A class of singletons capable of representing postive 'KnownNat' natural numbers.
class PositiveSingleton (p :: Nat -> Type) where
        posSingleton :: KnownNat n => p (1 + n)

instance PositiveSingleton Proxy where
        posSingleton = Proxy

-- | A positive number is either 1 or a successor of another positive number.
--
-- For example, @5 = 1 + (1 + (1 + (1 + 1)))@:
--
-- > UnarySucc (UnarySucc (UnarySucc (UnarySucc UnaryOne))) :: Unary 5
data Unary (n :: Nat) where
        UnaryOne :: Unary 1
        UnarySucc :: KnownNat n => Unary n -> Unary (1 + n)
deriving instance Show (Unary n)
instance ShowN Unary where showsPrecN = showsPrec

instance PositiveSingleton Unary where
        posSingleton :: forall n. KnownNat n => Unary (1 + n)
        posSingleton = case natSingleton :: NatIsZero n of
                IsZero -> UnaryOne
                IsNonZero -> UnarySucc posSingleton

-- | A positive number is either 1, or twice another positive number plus 0 or 1.
--
-- For example, @10 = 0 + 2 * (1 + 2 * (0 + 2 * 1))@:
--
-- > Bin0 (Bin1 (Bin0 BinOne)) :: PosBinary 10
data PosBinary (n :: Nat) where
        BinOne :: PosBinary 1
        Bin0 :: KnownNat n => PosBinary n -> PosBinary (2 GHC.TypeLits.* n)
        Bin1 :: KnownNat n => PosBinary n -> PosBinary (1 + 2 GHC.TypeLits.* n)
deriving instance Show (PosBinary n)
instance ShowN PosBinary where showsPrecN = showsPrec

instance PositiveSingleton PosBinary where
        posSingleton :: forall n. KnownNat n => PosBinary (1 + n)
        posSingleton = case natVal (Proxy :: Proxy n) of
                0 -> (unsafeCoerce :: PosBinary 1 -> PosBinary (1 + n)) BinOne
                n -> case someNatVal ((n - 1) `div` 2) of
                        Just (SomeNat (p :: Proxy m)) -> case someNatVal (natVal (Proxy :: Proxy m) + 1) of
                                Just (SomeNat (q :: Proxy l)) -> case unsafeCoerce Refl :: l :~: 1 + m of
                                        Refl -> if even n
                                                then (unsafeCoerce :: PosBinary (1 + 2 GHC.TypeLits.* l) -> PosBinary (1 + n)) $ Bin1 posSingleton
                                                else (unsafeCoerce :: PosBinary (2 GHC.TypeLits.* l) -> PosBinary (1 + n)) $ Bin0 posSingleton
                                Nothing -> error $ "Malformed KnownNat instance: " ++ show n
                        Nothing -> error $ "Malformed KnownNat instance: " ++ show n

-- | A positive number is either a digit in @[1, b)@, or another positive number times @b@ plus a digit in @[0, b)@.
--
-- For example, @123 = 3 + 10 * (2 + 10 * 1)@:
--
-- > BaseDigit (natSingleton :: p 3) (BaseDigit (natSingleton :: p 2) (BaseLead (natSingleton :: p 1))) :: PosBase p 10 123
data PosBase (p :: Nat -> Type) (b :: Nat) (n :: Nat) where
        BaseLead :: (KnownNat k, 1 + k <= b, k ~ (1 + n)) => p k -> PosBase p b k
        BaseDigit :: (KnownNat k, 1 + k <= b, KnownNat n) => p k -> PosBase p b n -> PosBase p b (k + b GHC.TypeLits.* n)
instance ShowN p => Show (PosBase p b n) where
        showsPrec d (BaseLead a) = showParen (d > 10) $ showString "BaseLead " . showsPrecN 11 a
        showsPrec d (BaseDigit a b) = showParen (d > 10) $ showString "BaseDigit " . showsPrecN 11 a . showString " " . showsPrec 11 b
instance ShowN p => ShowN (PosBase p b) where showsPrecN = showsPrec

instance (KnownNat b, b ~ (2 + c), NatSingleton p) => PositiveSingleton (PosBase p b) where
        posSingleton :: forall n. KnownNat n => PosBase p b (1 + n)
        posSingleton = case natVal (Proxy :: Proxy n) of
                n | n < base - 1 -> case someNatVal (n + 1) of
                        Just (SomeNat (p :: Proxy k)) -> case unsafeCoerce Refl :: k :~: (1 + n) of
                                Refl -> case unsafeCoerce Refl :: (1 + (1 + n) <=? b) :~: True of
                                        Refl -> BaseLead natSingleton
                        Nothing -> error $ "Malformed KnownNat instance: " ++ show n
                n -> case someNatVal ((n - base + 1) `div` base) of
                        Just (SomeNat (p :: Proxy m)) -> case someNatVal (natVal (Proxy :: Proxy m) + 1) of
                                Just (SomeNat (q :: Proxy l)) -> case unsafeCoerce Refl :: l :~: 1 + m of
                                        Refl -> case someNatVal ((n - base + 1) `mod` base) of
                                                Just (SomeNat (p :: Proxy k)) -> (unsafeCoerce :: PosBase p b (k + b GHC.TypeLits.* l) -> PosBase p b (1 + n)) $ case unsafeCoerce Refl :: (1 + k <=? b) :~: True of
                                                        Refl -> BaseDigit (natSingleton :: p k) (posSingleton :: PosBase p b l)
                                                Nothing -> error $ "Malformed KnownNat instance: " ++ show base
                                Nothing -> error $ "Malformed KnownNat instance: " ++ show base
                        Nothing -> error $ "Malformed KnownNat instance: " ++ show n
                where
                        base = natVal (Proxy :: Proxy b)