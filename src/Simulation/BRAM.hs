{-# LANGUAGE StandaloneDeriving #-}
module Simulation.BRAM where
import Clash.Prelude
import Clash.Sized.Vector
import GHC.TypeLits
import GHC.TypeLits.Extra
import Control.Lens
import Control.Monad.State.Lazy
import Typecore.Utils
import Data.Proxy
import Control.Monad
data BRAM s wd = BRAM {
    _ram :: Vec s (BitVector wd),
    _currentOutputA :: BitVector wd,
    _currentOutputB :: BitVector wd
} deriving (Show, Generic, NFDataX)

data BRAMInput s wd = BRAMInput {
    _enable :: Bit,
    _writeEnable :: Bit,
    _address :: BitVector (CLog 2 s),
    _din :: BitVector wd,
    _byteEnable :: BitVector (DivRU wd 8)
} deriving (Generic, NFDataX)
deriving instance (KnownNat wd, KnownNat s, (1<=?s) ~ True)=>Show (BRAMInput s wd)
data BRAMOutput wd = BRAMOutput {
    _dout :: BitVector wd
} deriving (Show, Generic, NFDataX)
makeLenses ''BRAM
makeLenses ''BRAMInput
makeLenses ''BRAMOutput
-- Block RAM without initializer.
-- Modeled after Xilinx Block RAM generator.
-- Read-first, write values or-ed.
blockRam' :: (HiddenClockResetEnable dom, KnownNat s, KnownNat wd, (1 <=? s) ~ True)=>BRAM s wd->Signal dom (BRAMInput s wd, BRAMInput s wd)->Signal dom (BRAMOutput wd, BRAMOutput wd)
blockRam' = moore (\s (i1, i2)->execState 
        (do
            let operate_read input out = when ((view enable input)==1) $ do
                        r<-use ram
                        let addr = view address input
                        out .= (r !! addr)
            let operate_write input = when ((view enable input) ==1 && (view writeEnable input) ==1) $ do
                        r<-use ram
                        let addr = view address input
                        ram %= (replace addr (setMask d8 (view byteEnable input) (r !! addr) (view din input)))
                    
            operate_read i1 currentOutputA
            operate_read i2 currentOutputB
            operate_write i1
            operate_write i2
        ) s
    )  (\s->(BRAMOutput {_dout = view currentOutputA s}, BRAMOutput{_dout = view currentOutputB s})) 

blockRam :: (HiddenClockResetEnable dom, KnownNat s, KnownNat wd, (1 <=? s) ~ True)=>Signal dom (BRAMInput s wd, BRAMInput s wd)->Signal dom (BRAMOutput wd, BRAMOutput wd)
blockRam = blockRam' (BRAM (repeat 0) 0 0)
