module Lib where
import Typecore.Utils
import Clash.Prelude
someFunc = 1

mappedPOH :: Signal System (BitVector 32)->Signal System (BitVector 32)
mappedPOH = fmap priorityOneHot
{-# ANN topEntity (defSyn "dut") #-}
topEntity :: Clock System -> Reset System->Enable System->Signal System (BitVector 32)->Signal System (BitVector 32)
topEntity = exposeClockResetEnable (fmap priorityOneHot)