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
expandInstruction i@(inst16 @"c.j"->True) = Just (encodeInst @"jal" (TyJ {j_rd = reg_x0, j_imm = signExtend $ slice d12 d12 i ++# slice d8 d8 i ++# slice d10 d9 i ++# slice d6 d6 i ++# slice d7 d7 i ++# slice d2 d2 i ++# slice d11 d11 i ++# slice d5 d3 i ++# (0::BitVector 1)}))
expandInstruction i@(inst16 @"@c.jr"->True) = if slice d11 d7 i ==0 then Nothing {- Reserved -} else Just (encodeInst @"jalr" (TyI {i_rd=reg_x0, i_rs1=slice d11 d7 i, i_imm=0}))
-- We match C.EBREAK first.
expandInstruction i@(inst16 @"@c.ebreak"->True) = Just (encodeInst @"ebreak" (TyR {r_rd=0, r_rs1=0, r_rs2=0}))
-- PC+2, not +4.
expandInstruction i@(inst16 @"@c.jalr"->True) = Just (encodeInst @"jalr" (TyI {i_rs1= slice d11 d7 i, i_rd = reg_x1, i_imm=0}))
expandInstruction i@(inst16 @"c.beqz"->True) = Just (encodeInst @"beq" (TyB {b_rs1 = primeReg $ slice d9 d7 i, b_rs2 = reg_x0, b_imm = signExtend $ slice d12 d12 i ++# slice d6 d5 i ++# slice d2 d2 i ++# slice d11 d10 i ++# slice d4 d3 i ++# (0 :: BitVector 1)}))
expandInstruction i@(inst16 @"c.bnez"->True) = Just (encodeInst @"bne" (TyB {b_rs1 = primeReg $ slice d9 d7 i, b_rs2 = reg_x0, b_imm = signExtend $ slice d12 d12 i ++# slice d6 d5 i ++# slice d2 d2 i ++# slice d11 d10 i ++# slice d4 d3 i ++# (0 :: BitVector 1)}))

-- Integer Computational Instructions
expandInstruction i@(inst16 @"c.li"->True) = Just (encodeInst @"addi" (TyI {i_rs1 = reg_x0, i_rd = slice d11 d7 i, i_imm = signExtend $ slice d12 d12 i ++# slice d6 d2 i}))
expandInstruction i@(inst16 @"@c.addi16sp"->True) = Just (encodeInst @"addi" (TyI {i_rs1=reg_x2, i_rd=reg_x2, i_imm = signExtend $ slice d12 d12 i ++# slice d4 d3 i ++# slice d5 d5 i ++# slice d2 d2 i ++# slice d6 d6 i ++# (0 :: BitVector 4)}))
expandInstruction i@(inst16 @"c.lui"->True) = if (slice d6 d2 i == 0  && slice d12 d12 i == 0) || (slice d11 d7 i == reg_x0 ) then Nothing {- Reserved or HINT -} else Just (encodeInst @"lui" (TyU {u_rd = slice d11 d7 i, u_imm = signExtend $ slice d12 d12 i ++# slice d6 d2 i ++# (0::BitVector 12)}))
expandInstruction i@(inst16 @"@c.nop"->True) = Just (encodeInst @"addi" (TyI {i_rd=0, i_rs1=0, i_imm=0}))
expandInstruction i@(inst16 @"c.addi"->True) = if (slice d6 d2 i == 0  && slice d12 d12 i == 0) then Nothing {- HINT -} else Just (encodeInst @"addi" (TyI {i_rd=slice d11 d7 i, i_rs1=slice d11 d7 i, i_imm = signExtend $ slice d12 d12 i ++# slice d6 d2 i}))
expandInstruction i@(inst16 @"@c.addiw"->True) = if (slice d11 d7 i ==0) then Nothing {- Reserved -} else Just (encodeInst @"addiw" (TyI {i_rd = slice d11 d7 i, i_rs1 = slice d11 d7 i, i_imm = signExtend $ slice d12 d12 i ++# slice d6 d2 i }))
expandInstruction i@(inst16 @"c.addi4spn"->True) = if (slice d12 d5 i ==0) then Nothing {- Reserved -} else Just (encodeInst @"addi" (TyI {i_rd = signExtend $ slice d4 d2 i, i_rs1 = reg_x2, i_imm = zeroExtend $ slice d10 d7 i ++# slice d12 d11 i ++# slice d5 d5 i ++# slice d6 d6 i ++# (0::BitVector 2)}))
expandInstruction i@(inst16 @"c.slli"->True) = if (slice d12 d12 i ==0 && slice d6 d2 i ==0) then Nothing {- HINT -} else Just (encodeInst @"slli" (TySh {sh_rd = slice d11 d7 i, sh_rs1 = slice d11 d7 i, sh_shamt = slice d12 d12 i ++# slice d6 d2 i}))
expandInstruction i@(inst16 @"c.srli"->True) = if (slice d12 d12 i ==0 && slice d6 d2 i ==0) then Nothing {- HINT -} else Just (encodeInst @"srli" (TySh {sh_rd = slice d11 d7 i, sh_rs1 = slice d11 d7 i, sh_shamt = slice d12 d12 i ++# slice d6 d2 i}))
expandInstruction i@(inst16 @"c.srai"->True) = if (slice d12 d12 i ==0 && slice d6 d2 i ==0) then Nothing {- HINT -} else Just (encodeInst @"srai" (TySh {sh_rd = slice d11 d7 i, sh_rs1 = slice d11 d7 i, sh_shamt = slice d12 d12 i ++# slice d6 d2 i}))
expandInstruction i@(inst16 @"c.andi"->True) = Just (encodeInst @"andi" (TyI {i_rs1 = primeReg $ slice d9 d7 i, i_rd = primeReg $ slice d9 d7 i, i_imm = signExtend $ slice d12 d12 i ++# slice d6 d2 i}))
expandInstruction i@(inst16 @"c.mv"->True) = if (slice d11 d7 i ==0) then Nothing {- HINT -} else Just (encodeInst @"add" (TyR {r_rs1=reg_x0, r_rd=slice d11 d7 i, r_rs2=slice d6 d2 i}))
expandInstruction i@(inst16 @"c.add"->True) = if (slice d11 d7 i ==0) then Nothing {- HINT -} else Just (encodeInst @"add" (TyR {r_rs1=slice d11 d7 i, r_rd=slice d11 d7 i, r_rs2=slice d6 d2 i}))
expandInstruction i@(inst16 @"c.and"->True) = Just (encodeInst @"and" (TyR {r_rs1 = primeReg $ slice d9 d7 i, r_rd = primeReg $ slice d9 d7 i, r_rs2 = primeReg $ slice d4 d2 i}))
expandInstruction i@(inst16 @"c.or"->True) = Just (encodeInst @"or" (TyR {r_rs1 = primeReg $ slice d9 d7 i, r_rd = primeReg $ slice d9 d7 i, r_rs2 = primeReg $ slice d4 d2 i}))
expandInstruction i@(inst16 @"c.xor"->True) = Just (encodeInst @"xor" (TyR {r_rs1 = primeReg $ slice d9 d7 i, r_rd = primeReg $ slice d9 d7 i, r_rs2 = primeReg $ slice d4 d2 i}))
expandInstruction i@(inst16 @"c.sub"->True) = Just (encodeInst @"sub" (TyR {r_rs1 = primeReg $ slice d9 d7 i, r_rd = primeReg $ slice d9 d7 i, r_rs2 = primeReg $ slice d4 d2 i}))
expandInstruction i@(inst16 @"c.addw"->True) = Just (encodeInst @"addw" (TyR {r_rs1 = primeReg $ slice d9 d7 i, r_rd = primeReg $ slice d9 d7 i, r_rs2 = primeReg $ slice d4 d2 i}))
expandInstruction i@(inst16 @"c.subw"->True) = Just (encodeInst @"subw" (TyR {r_rs1 = primeReg $ slice d9 d7 i, r_rd = primeReg $ slice d9 d7 i, r_rs2 = primeReg $ slice d4 d2 i}))

-- Illegal Instruction
expandInstruction ((==0) -> True) = Nothing




expandInstruction _ = Nothing


