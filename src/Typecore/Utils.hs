module Typecore.Utils where
import Clash.Sized.BitVector
import GHC.TypeLits.Induction
import qualified Clash.Class.BitPack as BitPack
import qualified Clash.Sized.Vector as V
import Clash.Prelude
import Data.Proxy
import Data.Maybe

--slice :: (KnownNat a, KnownNat size)=>BitVector a->Int->BitVector size
--slice vec offset = resize (vec `shiftR` offset)




priorityEncoder :: (KnownNat n, (1 <=? n) ~ True)=>BitVector n->BitVector (CLog 2 n)
priorityEncoder = BitPack.pack . priorityEncoder' . BitPack.unpack 

priorityEncoder' :: (KnownNat n, (1 <=? n) ~ True)=>Vec n Bit->Vec (CLog 2 n ) Bit
priorityEncoder' v = let ret = foldl (\b a->case (b, a) of
                                    (_, (0, _)) -> b
                                    (Nothing, (1, i)) -> Just i
                                    (Just x, _)-> b
                                    _ -> error "Bad Bit") Nothing $ zip v $ indicesI
                        in case ret of
                            Just x -> BitPack.unpack $ BitPack.pack x
                            Nothing -> undefined
newtype PriorityOneHot n = PriorityOneHot {toPriorityEncoder :: BitVector n->BitVector n}
priorityOneHot :: (KnownNat n)=>BitVector n->BitVector n
priorityOneHot = toPriorityEncoder $ 
        inducePeano (\encoder->PriorityOneHot $ 
                \bv->let (b, rest)=split bv
                        in if (msb b) == 1 
                            then b ++# 0
                            else b ++# (toPriorityEncoder encoder rest))
        (PriorityOneHot id)

-- Merging arbit result.
mergeBV :: (KnownNat a, KnownNat b)=>Vec a (BitVector b)->BitVector b
mergeBV = V.v2bv . (V.map reduceOr) . V.transpose . (V.map V.bv2v)

mergeBy :: (KnownNat n, KnownNat (BitSize a), BitPack a)=>BitVector n->Vec n a->a
mergeBy i v=
    let pv=map pack v
        vi=bv2v i
        iv=zip vi pv
        fv=map (\(a, b)->if a==1 then b else 0) iv
    in unpack $ mergeBV fv


mergeMaybe :: (KnownNat n, KnownNat (BitSize a), BitPack a)=>Vec n (Maybe a)->Maybe a
mergeMaybe v = let bs = v2bv $ V.map (\x->if isJust x then 1 else 0) v in mergeBy bs v


arbitWith :: (KnownNat n, KnownNat (BitSize a), BitPack a)=>(BitVector n->BitVector n)->Vec n (Maybe a)->Maybe a
arbitWith f v = let bs = v2bv $ V.map (\x->if isJust x then 1 else 0) v in mergeBy (f bs) v

arbitPriority :: (KnownNat n, KnownNat (BitSize a), BitPack a)=>Vec n (Maybe a)->Maybe a
arbitPriority = arbitWith (priorityOneHot)

-- Arbit with round robin.
arbitRoundRobin :: (HiddenClockResetEnable dom, KnownNat n, KnownNat (BitSize a), BitPack a)=>Signal dom (Vec n (Maybe a))->Signal dom (Maybe a)
arbitRoundRobin = undefined

setMask :: (KnownNat a, KnownNat b, (1<=?b) ~ 'True)=>proxy b->BitVector (DivRU a b)->BitVector a->BitVector a->BitVector a
setMask (p :: proxy b) mask x y = 
    let byte_mask = resize $ V.v2bv $ V.concat $ V.map (V.bv2v . (\x->((if x==1 then (-1) else 0) :: BitVector b))) $ V.bv2v mask
        neg_mask = complement byte_mask
    in (neg_mask .&. x) .|. (byte_mask .&. y)