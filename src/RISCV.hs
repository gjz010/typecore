-- This file is generated automatically.
{-# LANGUAGE AllowAmbiguousTypes #-}
module RISCV where
import Clash.Prelude
type RegIndex = BitVector 5
type Immediate = BitVector 32
type Shamt = BitVector 6
type RoundMode = BitVector 3
type FenceArg = BitVector 4
data InstructionParam = R {rs1 :: RegIndex, rs2 :: RegIndex, rd :: RegIndex}
                    |   I {rs1 :: RegIndex, imm :: Immediate, rd :: RegIndex}
                    |   S {rs1 :: RegIndex, rs2 :: RegIndex, imm :: Immediate}
                    |   B {rs1 :: RegIndex, rs2 :: RegIndex, imm :: Immediate}
                    |   U {imm :: Immediate, rd :: RegIndex}
                    |   J {imm :: Immediate, rd :: RegIndex}
                    |   At {aq :: Bit, rl :: Bit, rs1 :: RegIndex, rs2 :: RegIndex, rd :: RegIndex}
                    |   Fl {rs1 :: RegIndex, rs2 :: RegIndex, rd :: RegIndex, rm :: RoundMode}
                    |   Sh {rs1 :: RegIndex, rd :: RegIndex, shamt :: Shamt}
                    |   R4 {rs1 :: RegIndex, rs2 :: RegIndex, rs3 :: RegIndex, rd :: RegIndex, rm :: RoundMode}
                    |   Fc {pred :: FenceArg, succ :: FenceArg}
                    deriving Show

parse_aqrl :: BitVector 32->(Bit, Bit)
parse_aqrl i = let aqrl=get_aqrl i in (aqrl!1, aqrl!0)
parse_imm_i :: BitVector 32->Immediate
parse_imm_i i = signExtend @_ @12 @20 (slice d31 d31 i ++# slice d30 d25 i ++# slice d24 d21 i ++# slice d20 d20 i)
parse_imm_s :: BitVector 32->Immediate
parse_imm_s i = signExtend @_ @12 @20 (slice d31 d31 i ++# slice d30 d25 i ++# slice d11 d8 i ++# slice d7 d7 i)
parse_imm_b :: BitVector 32->Immediate
parse_imm_b i = signExtend @_ @13 @19 (slice d31 d31 i ++# slice d7 d7 i ++# slice d30 d25 i ++# slice d11 d8 i ++# (0 :: BitVector 1))
parse_imm_u :: BitVector 32->Immediate
parse_imm_u i = (slice d31 d31 i ++# slice d30 d20 i ++# slice d19 d12 i ++# (0 :: BitVector 12))
parse_imm_j :: BitVector 32->Immediate
parse_imm_j i = signExtend @_ @21 @11 (slice d31 d31 i ++# slice d19 d12 i ++# slice d20 d20 i ++# slice d30 d25 i ++# slice d24 d21 i ++# (0 :: BitVector 1))

parseR :: BitVector 32->InstructionParam
parseR i = R {rs1=get_rs1 i, rs2=get_rs2 i, rd=get_rd i}
parseI :: BitVector 32->InstructionParam
parseI i = I {rs1=get_rs1 i, imm=parse_imm_i i, rd=get_rd i}
parseS :: BitVector 32->InstructionParam
parseS i = S {rs1=get_rs1 i, rs2=get_rs2 i, imm=parse_imm_s i}
parseB :: BitVector 32->InstructionParam
parseB i = B {rs1=get_rs1 i, rs2=get_rs2 i, imm=parse_imm_b i}
parseU :: BitVector 32->InstructionParam
parseU i = U {imm=parse_imm_u i, rd=get_rd i}
parseJ :: BitVector 32->InstructionParam
parseJ i = J {imm=parse_imm_u i, rd=get_rd i}
parseAt :: BitVector 32->InstructionParam
parseAt i = let (vaq, vrl)=parse_aqrl i in At {rs1=get_rs1 i, rs2=get_rs2 i, rd=get_rd i, aq=vaq, rl=vrl}
parseFl :: BitVector 32->InstructionParam
parseFl i = Fl {rs1=get_rs1 i, rs2=get_rs2 i, rd=get_rd i, rm=get_rm i}
parseSh :: BitVector 32->InstructionParam
parseSh i = Sh {rs1=get_rs1 i, rd=get_rd i, shamt=get_shamt i}
parseR4 :: BitVector 32->InstructionParam
parseR4 i = R4 {rs1=get_rs1 i, rs2=get_rs2 i, rs3=get_rs3 i, rd=get_rd i, rm=get_rm i}
parseFc :: BitVector 32->InstructionParam
parseFc i = Fc {RISCV.pred=get_pred i, RISCV.succ=get_succ i}

class InstructionMatch (s::Symbol) where
    inst :: BitVector 32->Maybe InstructionParam

data Cause = UnknownCause | MisalignedFetch | FetchAccess | IllegalInstruction | BreakPoint | MisalignedLoad | LoadAccess | MisalignedStore | StoreAccess | UserECall | SupervisorECall | HypervisorECall | MachineECall | FetchPageFault | LoadPageFault | StorePageFault  deriving Show
cause :: (Num a, Eq a)=>a->Cause
cause 0 = MisalignedFetch
cause 1 = FetchAccess
cause 2 = IllegalInstruction
cause 3 = BreakPoint
cause 4 = MisalignedLoad
cause 5 = LoadAccess
cause 6 = MisalignedStore
cause 7 = StoreAccess
cause 8 = UserECall
cause 9 = SupervisorECall
cause 10 = HypervisorECall
cause 11 = MachineECall
cause 12 = FetchPageFault
cause 13 = LoadPageFault
cause 15 = StorePageFault
cause _ = UnknownCause
get_aqrl :: (BitPack a, BitSize a ~ 32)=>a->BitVector 2
get_aqrl = slice d26 d25
get_bimm12hi :: (BitPack a, BitSize a ~ 32)=>a->BitVector 7
get_bimm12hi = slice d31 d25
get_bimm12lo :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_bimm12lo = slice d11 d7
get_imm12 :: (BitPack a, BitSize a ~ 32)=>a->BitVector 12
get_imm12 = slice d31 d20
get_imm12hi :: (BitPack a, BitSize a ~ 32)=>a->BitVector 7
get_imm12hi = slice d31 d25
get_imm12lo :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_imm12lo = slice d11 d7
get_imm20 :: (BitPack a, BitSize a ~ 32)=>a->BitVector 20
get_imm20 = slice d31 d12
get_jimm20 :: (BitPack a, BitSize a ~ 32)=>a->BitVector 20
get_jimm20 = slice d31 d12
get_pred :: (BitPack a, BitSize a ~ 32)=>a->BitVector 4
get_pred = slice d27 d24
get_rd :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_rd = slice d11 d7
get_rm :: (BitPack a, BitSize a ~ 32)=>a->BitVector 3
get_rm = slice d14 d12
get_rs1 :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_rs1 = slice d19 d15
get_rs2 :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_rs2 = slice d24 d20
get_rs3 :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_rs3 = slice d31 d27
get_shamt :: (BitPack a, BitSize a ~ 32)=>a->BitVector 6
get_shamt = slice d25 d20
get_shamtw :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_shamtw = slice d24 d20
get_succ :: (BitPack a, BitSize a ~ 32)=>a->BitVector 4
get_succ = slice d23 d20
get_vseglen :: (BitPack a, BitSize a ~ 32)=>a->BitVector 3
get_vseglen = slice d31 d29
get_zimm :: (BitPack a, BitSize a ~ 32)=>a->BitVector 5
get_zimm = slice d19 d15
instance InstructionMatch "beq" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionMatch "bne" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionMatch "blt" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionMatch "bge" where inst i = if (slice d14 d12 i==5) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionMatch "bltu" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionMatch "bgeu" where inst i = if (slice d14 d12 i==7) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionMatch "jalr" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x19) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "jal" where inst i = if (slice d6 d2 i==0x1b) && (slice d1 d0 i==3) then Just $ parseJ i else Nothing
instance InstructionMatch "lui" where inst i = if (slice d6 d2 i==0x0D) && (slice d1 d0 i==3) then Just $ parseU i else Nothing
instance InstructionMatch "auipc" where inst i = if (slice d6 d2 i==0x05) && (slice d1 d0 i==3) then Just $ parseU i else Nothing
instance InstructionMatch "addi" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "slli" where inst i = if (slice d31 d26 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionMatch "slti" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "sltiu" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "xori" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "srli" where inst i = if (slice d31 d26 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionMatch "srai" where inst i = if (slice d31 d26 i==16) && (slice d14 d12 i==5) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionMatch "ori" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "andi" where inst i = if (slice d14 d12 i==7) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "add" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sub" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sll" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "slt" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sltu" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "xor" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==4) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "srl" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sra" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "or" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==6) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "and" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==7) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "addiw" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "slliw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionMatch "srliw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionMatch "sraiw" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==5) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionMatch "addw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "subw" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sllw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "srlw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sraw" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "lb" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "lh" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "lw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "ld" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "lbu" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "lhu" where inst i = if (slice d14 d12 i==5) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "lwu" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "sb" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionMatch "sh" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionMatch "sw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionMatch "sd" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionMatch "fence" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x03) && (slice d1 d0 i==3) then Just $ parseFc i else Nothing
instance InstructionMatch "fence.i" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x03) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "mul" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "mulh" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==1) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "mulhsu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "mulhu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "div" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==4) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "divu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "rem" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==6) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "remu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==7) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "mulw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "divw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==4) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "divuw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "remw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==6) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "remuw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==7) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "amoadd.w" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoxor.w" where inst i = if (slice d31 d29 i==1) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoor.w" where inst i = if (slice d31 d29 i==2) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoand.w" where inst i = if (slice d31 d29 i==3) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amomin.w" where inst i = if (slice d31 d29 i==4) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amomax.w" where inst i = if (slice d31 d29 i==5) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amominu.w" where inst i = if (slice d31 d29 i==6) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amomaxu.w" where inst i = if (slice d31 d29 i==7) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoswap.w" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==1) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "lr.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d29 i==0) && (slice d28 d27 i==2) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "sc.w" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==3) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoadd.d" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoxor.d" where inst i = if (slice d31 d29 i==1) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoor.d" where inst i = if (slice d31 d29 i==2) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoand.d" where inst i = if (slice d31 d29 i==3) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amomin.d" where inst i = if (slice d31 d29 i==4) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amomax.d" where inst i = if (slice d31 d29 i==5) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amominu.d" where inst i = if (slice d31 d29 i==6) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amomaxu.d" where inst i = if (slice d31 d29 i==7) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "amoswap.d" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==1) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "lr.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d29 i==0) && (slice d28 d27 i==2) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "sc.d" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==3) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionMatch "ecall" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x000) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "ebreak" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x001) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "uret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x002) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x102) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "mret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x302) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "dret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x7b2) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "sfence.vma" where inst i = if (slice d11 d7 i==0) && (slice d31 d25 i==0x09) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "wfi" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x105) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "csrrw" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "csrrs" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "csrrc" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "csrrwi" where inst i = if (slice d14 d12 i==5) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "csrrsi" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "csrrci" where inst i = if (slice d14 d12 i==7) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "hfence.vvma" where inst i = if (slice d11 d7 i==0) && (slice d31 d25 i==0x11) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "hfence.gvma" where inst i = if (slice d11 d7 i==0) && (slice d31 d25 i==0x31) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fadd.s" where inst i = if (slice d31 d27 i==0x00) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsub.s" where inst i = if (slice d31 d27 i==0x01) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmul.s" where inst i = if (slice d31 d27 i==0x02) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fdiv.s" where inst i = if (slice d31 d27 i==0x03) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsgnj.s" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fsgnjn.s" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fsgnjx.s" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==2) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fmin.s" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fmax.s" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fsqrt.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x0B) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fadd.d" where inst i = if (slice d31 d27 i==0x00) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsub.d" where inst i = if (slice d31 d27 i==0x01) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmul.d" where inst i = if (slice d31 d27 i==0x02) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fdiv.d" where inst i = if (slice d31 d27 i==0x03) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsgnj.d" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fsgnjn.d" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fsgnjx.d" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==2) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fmin.d" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fmax.d" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.s.d" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x08) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.d.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x08) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsqrt.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x0B) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fadd.q" where inst i = if (slice d31 d27 i==0x00) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsub.q" where inst i = if (slice d31 d27 i==0x01) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmul.q" where inst i = if (slice d31 d27 i==0x02) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fdiv.q" where inst i = if (slice d31 d27 i==0x03) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsgnj.q" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fsgnjn.q" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fsgnjx.q" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==2) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fmin.q" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fmax.q" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.s.q" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x08) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.q.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x08) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.d.q" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x08) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.q.d" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x08) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fsqrt.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x0B) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fle.s" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "flt.s" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "feq.s" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==2) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fle.d" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "flt.d" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "feq.d" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==2) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fle.q" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "flt.q" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "feq.q" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==2) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.w.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.wu.s" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.l.s" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.lu.s" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmv.x.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fclass.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.w.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.wu.d" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.l.d" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.lu.d" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmv.x.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fclass.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.w.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.wu.q" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.l.q" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.lu.q" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmv.x.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fclass.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.s.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.s.wu" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.s.l" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.s.lu" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmv.w.x" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1E) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.d.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.d.wu" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.d.l" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.d.lu" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmv.d.x" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1E) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "fcvt.q.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.q.wu" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.q.l" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fcvt.q.lu" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionMatch "fmv.q.x" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1E) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionMatch "flw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x01) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "fld" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x01) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "flq" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x01) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionMatch "fsw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x09) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionMatch "fsd" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x09) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionMatch "fsq" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x09) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionMatch "fmadd.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x10) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fmsub.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x11) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fnmsub.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x12) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fnmadd.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x13) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fmadd.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x10) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fmsub.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x11) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fnmsub.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x12) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fnmadd.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x13) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fmadd.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x10) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fmsub.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x11) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fnmsub.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x12) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionMatch "fnmadd.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x13) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
