module Typecore.Cache where
import Clash.Prelude
import Typecore.Prelude
import Control.Lens
import Typecore.AXI
-- Our ICache works in the following way:
-- Cache line = 32 bytes = 8 long insns = 16 short insns = 4 words
-- Issue width = 4 insns
-- Read operations: can read two consecutive cache lines simultaneously, since this is enough.
-- Write operations: Write a cache line. May implement fast start to save a cycle.
-- With an AXI Master.
-- Cache invalidation: done by an committed iflush.
-- Cache design: set-associative

data ICache (n::Nat) = ICache {
    

} deriving (Generic, NFDataX)

data ICacheInput = ICacheInput {
    _addr :: TWord,
    _axiMemory :: AXI4Master 
} deriving (Generic, NFDataX)

makeLenses ''ICacheInput
makeLenses ''ICache