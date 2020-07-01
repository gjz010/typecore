-- This module provides a way for uncompressing RVC instructions.
module RISCV.RVC where
import Clash.Prelude
import Data.Bits
import RISCV
data UncompressResult = BadInstruction | Compressed (BitVector 32) | Full (BitVector 32)

data InsnType = RV16 (BitVector 16) | RV32 (BitVector 32) | UnknownType

checkType :: BitVector 32->InsnType
checkType v = if v .&. 3 /= 3 then RV16 (truncateB v) else (if v .&. 31 /= 31 then RV32 v else UnknownType)

decodeInstruction :: BitVector 32-> UncompressResult
decodeInstruction (checkType->RV32 x) = Full x
decodeInstruction (checkType->RV16 x) = BadInstruction
decodeInstruction (checkType->UnknownType) = BadInstruction


-- The core method for expanding instructions.
expandInstruction :: BitVector 16 -> Maybe (BitVector 32)
--expandInstruction (inst16 @"c.lwsp"->True) = 
--expandInstruction (inst16 @"@c.nop"->True) =undefined
expandInstruction _ = Nothing