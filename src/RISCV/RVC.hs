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


primeReg :: BitVector 3->RegIndex
primeReg x = (1 :: BitVector 2) ++# x

-- We hope that we don't need to generate RVC codes.

-- The core method for expanding instructions.
expandInstruction :: BitVector 16 -> Maybe (BitVector 32)
-- Stack-Pointer-Based Loads and Stores
expandInstruction i@(inst16 @"c.lwsp"->True) = Just (encodeInst @"lw" (TyI {i_rd=slice d11 d7 i, i_rs1=reg_x2, i_imm=zeroExtend $ (slice d3 d2 i)++#(slice d12 d12 i)++#(slice d6 d4 i) ++# (0 :: BitVector 2)}))
expandInstruction i@(inst16 @"@c.ldsp"->True) = Just (encodeInst @"ld" (TyI {i_rd=slice d11 d7 i, i_rs1=reg_x2, i_imm=zeroExtend $ (slice d4 d2 i)++#(slice d12 d12 i)++#(slice d6 d5 i) ++# (0 :: BitVector 3)}))
expandInstruction i@(inst16 @"c.fldsp"->True) = Just (encodeInst @"fld" (TyI {i_rd=slice d11 d7 i, i_rs1=reg_x2, i_imm=zeroExtend $ (slice d4 d2 i)++#(slice d12 d12 i)++#(slice d6 d5 i) ++# (0 :: BitVector 3)}))
expandInstruction i@(inst16 @"c.swsp"->True) = Just (encodeInst @"sw" (TyS {s_rs2=slice d6 d2 i, s_rs1=reg_x2, s_imm=zeroExtend $ (slice d8 d7 i) ++# (slice d12 d9 i) ++# (0 :: BitVector 2)}))
expandInstruction i@(inst16 @"@c.sdsp"->True) = Just (encodeInst @"sd" (TyS {s_rs2=slice d6 d2 i, s_rs1=reg_x2, s_imm=zeroExtend $ (slice d9 d7 i) ++# (slice d12 d10 i) ++# (0 :: BitVector 3)}))
expandInstruction i@(inst16 @"c.fsdsp"->True) = Just (encodeInst @"fsd" (TyS {s_rs2=slice d6 d2 i, s_rs1=reg_x2, s_imm=zeroExtend $ (slice d9 d7 i) ++# (slice d12 d10 i) ++# (0 :: BitVector 3)}))

-- Register-Based Loads and Stores
expandInstruction i@(inst16 @"c.lw"->True) = Just (encodeInst @"lw" (TyI {i_rd=primeReg $ slice d4 d2 i, i_rs1=primeReg $ slice d9 d7 i, i_imm = zeroExtend $ slice d5 d5 i ++# slice d12 d10 i ++# slice d6 d6 i ++# (0 :: BitVector 2)}))
expandInstruction i@(inst16 @"@c.ld"->True) = Just (encodeInst @"ld" (TyI {i_rd=primeReg $ slice d4 d2 i, i_rs1=primeReg $ slice d9 d7 i, i_imm = zeroExtend $ slice d6 d5 i ++# slice d12 d10 i ++# (0 :: BitVector 3)}))
expandInstruction i@(inst16 @"c.fld"->True) = Just (encodeInst @"fld" (TyI {i_rd=primeReg $ slice d4 d2 i, i_rs1=primeReg $ slice d9 d7 i, i_imm = zeroExtend $ slice d6 d5 i ++# slice d12 d10 i ++# (0 :: BitVector 3)}))
expandInstruction i@(inst16 @"c.sw"->True) = Just (encodeInst @"sw" (TyS {s_rs2=primeReg $ slice d4 d2 i, s_rs1=primeReg $ slice d9 d7 i, s_imm = zeroExtend $ slice d5 d5 i ++# slice d12 d10 i ++# slice d6 d6 i ++# (0 :: BitVector 2)}))
expandInstruction i@(inst16 @"@c.sd"->True) = Just (encodeInst @"sd" (TyS {s_rs2=primeReg $ slice d4 d2 i, s_rs1=primeReg $ slice d9 d7 i, s_imm = zeroExtend $ slice d6 d5 i ++# slice d12 d10 i ++# (0 :: BitVector 3)}))
expandInstruction i@(inst16 @"c.fsd"->True) = Just (encodeInst @"fsd" (TyS {s_rs2=primeReg $ slice d4 d2 i, s_rs1=primeReg $ slice d9 d7 i, s_imm = zeroExtend $ slice d6 d5 i ++# slice d12 d10 i ++# (0 :: BitVector 3)}))

-- Control Transfer Instructions
--expandInstruction 

expandInstruction i@(inst16 @"@c.nop"->True) =undefined
expandInstruction _ = Nothing