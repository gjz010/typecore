module Typecore.Utils where
    import Clash.Sized.BitVector
    import Clash.Prelude
    --slice :: (KnownNat a, KnownNat size)=>BitVector a->Int->BitVector size
    --slice vec offset = resize (vec `shiftR` offset)