module Typecore.Utils where
import Clash.Sized.BitVector
import Clash.Prelude
import GHC.TypeLits.Induction
import qualified Clash.Class.BitPack as BitPack
import qualified Clash.Sized.Vector as V
import Clash.Prelude
import Data.Proxy

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
arbit :: (KnownNat n, KnownNat (BitSize a), BitPack a)=>BitVector n->Vec n a->a
arbit i v=
    let pv=map pack v
        vi=bv2v i
        iv=zip vi pv
        fv=map (\(a, b)->if a==1 then b else 0) iv
        fbv=map bv2v fv
        fbvt=transpose fbv
        finalt=map reduceOr fbvt
    in unpack $ v2bv finalt

-- Merging multiple bitvectors using or-operation.
transposeVector :: Vec a (Vec b s)->Vec b (Vec a s)
transposeVector (V.Cons V.Nil _ )= Nil
transposeVector x@(V.Cons (V.Cons a b) xs)  = V.Cons (V.map V.head x) (transposeVector (V.map V.tail x))

mergeResult :: (KnownNat a, KnownNat b)=>Vec a (BitVector b)->BitVector b
mergeResult = V.v2bv . (V.map reduceOr) . transposeVector . (V.map V.bv2v)

setMask :: (KnownNat a, KnownNat b, (1<=?b) ~ True)=>proxy b->BitVector (DivRU a b)->BitVector a->BitVector a->BitVector a
setMask (p :: proxy b) mask x y = 
    let byte_mask = resize $ V.v2bv $ V.concat $ V.map (V.bv2v . (\x->((if x==1 then (-1) else 0) :: BitVector b))) $ V.bv2v mask
        neg_mask = complement byte_mask
    in (neg_mask .&. x) .|. (byte_mask .&. y)