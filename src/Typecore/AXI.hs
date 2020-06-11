module Typecore.AXI where
import Clash.Prelude
--import Clash.Sized.Vector
data AXI4Master = AXI4Master {
    awaddr :: BitVector 64, awlen :: BitVector 8, awsize :: BitVector 3, awburst :: BitVector 2, awvalid :: Bit,
    wdata :: BitVector 64 , wstrb :: BitVector 8, wlast :: Bit, wvalid :: Bit,
    bready :: Bit,
    araddr :: BitVector 64, arlen :: BitVector 8, arsize :: BitVector 3, arburst :: BitVector 2, arvalid :: Bit,
    rready :: Bit
} deriving (Generic, NFDataX, Show)

data AXI4Slave = AXI4Slave {
    awready :: Bit,
    wready :: Bit,
    bresp :: BitVector 2, bvalid :: Bit,
    arready :: Bit,
    rdata :: BitVector 64, rresp :: BitVector 2, rlast :: Bit, rvalid :: Bit
} deriving (Generic, NFDataX, Show)

-- AXI helper.


-- AXI Interconnect implementation.

data AXIInterconnectState (m::Nat) (n::Nat) = AXIInterconnectState deriving (Generic, NFDataX, Show)

initializeAXIInteconnect :: (KnownNat m, KnownNat n)=>(SNat m, SNat n)->AXIInterconnectState m n
initializeAXIInteconnect _ = AXIInterconnectState
axiInterconnect :: (HiddenClockResetEnable dom, KnownNat m, KnownNat n)=>(SNat m, SNat n)->Signal dom (Vec m AXI4Master)->Signal dom (Vec n AXI4Slave)
axiInterconnect (m, n) = mealy undefined (initializeAXIInteconnect (m, n))

-- TODO: implement AXI-interconnect.