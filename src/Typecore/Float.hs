{-# LANGUAGE DeriveGeneric, DeriveAnyClass, RankNTypes #-}
module Typecore.Float where
import Clash.Prelude hiding (exponent, pack, unpack, isNaN)
import Clash.Class.BitPack hiding (pack, unpack)
import qualified Clash.Class.BitPack as BitPack
import GHC.Generics
import Clash.XException
import Data.Word
import Debug.Trace
import Typecore.Utils
data MyProductType = MyProductType { a :: Int, b :: Bool } deriving (Generic, NFDataX)

data FloatPoint (e::Nat) (m::Nat) = FloatPoint {sign :: Bit, exponent :: BitVector e, mantissa :: BitVector m} deriving (Show, Generic, BitPack, NFDataX)

type Float'=FloatPoint 8 23
type Double'=FloatPoint 11 52


unpack :: (BitPack a, BitSize a ~ n, (e + m + 1) ~ n, KnownNat e, KnownNat m, KnownNat n)=>SNat e->SNat m->a->FloatPoint e m
unpack de dm x = let (slice_e, slice_m) = (let (_, last) = split x in split last)
                        in FloatPoint {sign = msb x, exponent = slice_e, mantissa = slice_m}
unpackFloat :: Float->Float'
unpackFloat = unpack d8 d23
unpackDouble :: Double->Double'
unpackDouble = unpack d11 d52


pack :: (BitPack a, BitSize a ~ n, (e+m+1)~n, KnownNat e, KnownNat m)=>FloatPoint e m->a
pack (FloatPoint s e m)= BitPack.unpack $ (BitPack.pack s) ++# e ++# m


isNaN :: (KnownNat e, KnownNat m)=>FloatPoint e m->Bool
isNaN (FloatPoint _ e m)= (e==(-1)) && (m /= 0)
isInfinity :: (KnownNat e, KnownNat m)=>FloatPoint e m->Bool
isInfinity (FloatPoint _ e m)= (e==(-1)) && (m==0)
unnormalize :: (KnownNat e, KnownNat m)=>FloatPoint e m->(BitVector e, BitVector (m+1))
unnormalize (FloatPoint _ e m) = if e==0 then (1, 0 ++# m) else (e, 1 ++# m)

isPositive :: FloatPoint e m->Bool
isPositive = not . BitPack.unpack . BitPack.pack . sign

isNegative :: FloatPoint e m->Bool
isNegative = BitPack.unpack . BitPack.pack . sign

positiveInf :: (KnownNat e, KnownNat m)=>FloatPoint e m
positiveInf = FloatPoint 0 (-1) 0
negativeInf :: (KnownNat e, KnownNat m)=>FloatPoint e m
negativeInf = FloatPoint 1 (-1) 0

signedInf :: (KnownNat e, KnownNat m)=>Bit->FloatPoint e m
signedInf s = FloatPoint s (-1) 0
nan :: (KnownNat e, KnownNat m)=>FloatPoint e (m+1)
nan = FloatPoint 0 (-1) ((1::BitVector 1) ++# 0)

shiftRKeepLSB :: (KnownNat m)=>BitVector m->Int->(BitVector m, Bit)
shiftRKeepLSB x t = let (a, b)=split ((x ++# (0::BitVector 1)) `shiftR` t) in (a, (msb b))
matchExponents :: (KnownNat e, KnownNat m)=>FloatPoint e m->FloatPoint e m->(BitVector e, (Bit, BitVector (m+2)), (Bit, BitVector (m+2), Bit))
matchExponents f1 f2 = 
    let go (s1, e1, m1) (s2, e2, m2)=let delta=e1-e2; (x, y)=m2 `shiftRKeepLSB` (fromIntegral delta) in (e1, (s1, m1), (s2, x, y))
        (e1, m1)=unnormalize f1
        (e2, m2)=unnormalize f2
        p1=(sign f1, e1, m1 ++#0)
        p2=(sign f2, e2, m2 ++#0)
    in if e1>=e2 then go p1 p2 else go p2 p1

addMatchedExponents :: (KnownNat m)=>((Bit, BitVector (m+2)), (Bit, BitVector (m+2)))->(Bit, BitVector (m+3))
addMatchedExponents ( (s1, m1), (s2, m2)) = 
    let msb' x = BitPack.unpack $ BitPack.pack $ msb x
        prepare s m = (let p = ((0 :: BitVector 2) ++# m) in if s==low then p else -p)
        m3'=(prepare s1 m1)+(prepare s2 m2) 
        m3=snd $ split $ if msb' m3' then -m3' else m3' {- extend_normalizetag_m_suffix -}
        s3=msb' m3'
    in (s3, m3)


tailV :: (KnownNat n)=>BitVector (n+1)->BitVector n
tailV = truncateB

initV :: (KnownNat n)=>BitVector (n+1)->BitVector n
initV = fst . split





renormalize_incr :: (KnownNat e, KnownNat m)=>(Bit, BitVector e, BitVector (m+3))->FloatPoint e m
renormalize_incr (s, e, m) = if e+1==(-1) then signedInf s else FloatPoint s (e+1) (let m'=initV m in tailV $ if lsb m' == high then (initV m') +1 else initV m')
renormalize_normalized :: (KnownNat e, KnownNat m)=>BitVector e->(Bit, BitVector e, BitVector (m+3))->FloatPoint e m
renormalize_normalized index (s, e, m) =
    let i=index+1
        mantissa = initV $ initV $ m `shiftL` (fromIntegral i)
        mantissa' = if lsb mantissa == high then (initV mantissa) +1 else (initV mantissa)
    in FloatPoint s e mantissa'
renormalize_denormalized :: (KnownNat e, KnownNat m)=>(Bit, BitVector e, BitVector (m+3))->FloatPoint e m
renormalize_denormalized (s, e, m) = FloatPoint s 0 (fst $ split $ (m `shiftL` (fromIntegral (e+1))))
renormalize :: (KnownNat e, KnownNat m, (1 <=? m) ~ True)=>(Bit, BitVector e, BitVector (m+3))->FloatPoint e m

renormalize (s, e, m) = 
    let index =  priorityEncoder m in 
        if m==0 then FloatPoint s 0 0 else
        if msb m == high
            then trace "incr" $ renormalize_incr (s, e, m) {- 1x.xxxx, e increases, but no more increment -}
            else if e==1 && (m == ((0 :: BitVector 2) ++# (-1))) then trace "c2" $ FloatPoint s 1 0 {- The special case: 00.1111111, denormalize shifts to normalize. -}
            else if e+1<=(resize index) then trace "denom" $ renormalize_denormalized (s, e, m) {- denormalized case -}
            else if tailV m == (-1) then trace "c4" $ FloatPoint s (e+1) 0 {- The special case: 01.1111111, rounding shifts e -}
            else trace "norm" $ renormalize_normalized (resize index) (s, e, m) {- Normalized float. -}

data FloatAddFlag = FlagNone | FlagNaN | FlagPosInfinity | FlagNegInfinity deriving (Show, Generic, BitPack, NFDataX)
checkAddInput :: (KnownNat e, KnownNat m)=>FloatPoint e m->FloatPoint e m-> FloatAddFlag
checkAddInput f1 f2 = 
    if isNaN f1 || isNaN f2
        then FlagNaN
        else if isInfinity f1 && (not (isInfinity f2))
                then if isPositive f1 then FlagPosInfinity else FlagNegInfinity
                else if isInfinity f2 && (not (isInfinity f1))
                        then if isPositive f2 then FlagPosInfinity else FlagNegInfinity
                        else if isInfinity f1 && isInfinity f2
                                then if isPositive f1 == isPositive f2
                                        then (if isPositive f1 then FlagPosInfinity else FlagNegInfinity)
                                        else FlagNaN
                                else FlagNone

addPipeline :: (HiddenClockResetEnable dom, KnownNat e, KnownNat m, (m'+1) ~ m, KnownNat m')=>Signal dom (Maybe (FloatPoint e m))->Signal dom (Maybe (FloatPoint e m))->Signal dom (Maybe (FloatPoint e m))
addPipeline i1 i2 = 
    let p1 = register Nothing $ liftA2(\f1 f2->do
                j1<-f1
                j2<-f2
                return (j1, j2)) i1 i2
        p2 = register Nothing $ fmap (\p1_i->do
            (f1, f2)<-p1_i
            let flag=checkAddInput f1 f2
            return $ (flag, matchExponents f1 f2)) p1
        p3 = register Nothing $ fmap (\p2_i->do
            (flag, (e, m1, m2))<-p2_i
            return $ (flag, e, addMatchedExponents (m1, m2))) p2
        p4 = register Nothing $ fmap (\p3_i->do
            (flag, e, (s, m))<-p3_i
            let ret = renormalize (s, e, m)
            return $ case flag of
                        FlagNaN -> nan
                        FlagPosInfinity -> positiveInf
                        FlagNegInfinity -> negativeInf
                        FlagNone -> ret) p3
    in p4
test_pipeline_add :: (HiddenClockResetEnable dom)=>Signal dom (Maybe Float, Maybe Float)->Signal dom (Maybe Float)
test_pipeline_add x = fmap (fmap pack) $ addPipeline (fmap ((fmap unpackFloat).fst) x) (fmap ((fmap unpackFloat).snd) x)
test_add :: Float->Float->Float
test_add f1 f2 = 
    let f1'=trace "f1" $ traceShowId $ unpackFloat f1
        f2'=trace "f2" $ traceShowId $ unpackFloat f2
        (e, m1, m2)=trace "matched:" $ traceShowId $ matchExponents f1' f2'
        (s, m)=trace "after adding" $ traceShowId $ addMatchedExponents (m1, m2)
        ret=renormalize (s, e, m)
    in pack $ case checkAddInput f1' f2' of
        FlagNaN -> nan
        FlagPosInfinity -> positiveInf
        FlagNegInfinity -> negativeInf
        FlagNone -> ret