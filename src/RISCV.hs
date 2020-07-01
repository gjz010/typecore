-- This file is generated automatically.
{-# LANGUAGE AllowAmbiguousTypes, NamedFieldPuns, RankNTypes, FunctionalDependencies #-}
module RISCV where
import Clash.Prelude
import Control.Lens
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

data TyR = TyR {r_rs1 :: RegIndex, r_rs2 :: RegIndex, r_rd :: RegIndex}
data TyI = TyI {i_rs1 :: RegIndex, i_imm :: Immediate, i_rd :: RegIndex}
data TyS = TyS {s_rs1 :: RegIndex, s_rs2 :: RegIndex, s_imm :: Immediate}
data TyB = TyB {b_rs1 :: RegIndex, b_rs2 :: RegIndex, b_imm :: Immediate}
data TyU = TyU {u_imm :: Immediate, u_rd :: RegIndex}
data TyJ = TyJ {j_imm :: Immediate, j_rd :: RegIndex}
data TyAt = TyAt {at_aq :: Bit, at_rl :: Bit, at_rs1 :: RegIndex, at_rs2 :: RegIndex, at_rd :: RegIndex}
data TyFl = TyFl {fl_rs1 :: RegIndex, fl_rs2 :: RegIndex, fl_rd :: RegIndex, fl_rm :: RoundMode}
data TySh = TySh {sh_rs1 :: RegIndex, sh_rd :: RegIndex, sh_shamt :: Shamt}
data TyR4 = TyR4 {r4_rs1 :: RegIndex, r4_rs2 :: RegIndex, r4_rs3 :: RegIndex, r4_rd :: RegIndex, r4_rm :: RoundMode}
data TyFc = TyFc {fc_pred :: FenceArg, fc_succ :: FenceArg}

class TyParam a where
    toSumParam :: a->InstructionParam
instance TyParam TyR where toSumParam (TyR a b c) = R a b c
instance TyParam TyI where toSumParam (TyI a b c) = I a b c
instance TyParam TyS where toSumParam (TyS a b c) = S a b c
instance TyParam TyB where toSumParam (TyB a b c) = B a b c
instance TyParam TyU where toSumParam (TyU a b) = U a b
instance TyParam TyJ where toSumParam (TyJ a b) = J a b
instance TyParam TyAt where toSumParam (TyAt a b c d e) = At a b c d e
instance TyParam TyFl where toSumParam (TyFl a b c d) = Fl a b c d
instance TyParam TySh where toSumParam (TySh a b c) = Sh a b c
instance TyParam TyR4 where toSumParam (TyR4 a b c d e) = R4 a b c d e
instance TyParam TyFc where toSumParam (TyFc a b) = Fc a b

sliceLens a b = lens (slice a b) (flip (setSlice a b))

lens_aq_rl :: (BitPack a, BitSize a ~ 32)=>Lens' a (Bit, Bit)
lens_aq_rl = lens_aqrl . (iso (\x->(x!1,x!0)) (\(a,b)->(pack a) ++# (pack b)))

(++##) :: (KnownNat a, KnownNat b)=>Lens' (BitVector s) (BitVector a)->Lens' (BitVector s) (BitVector b)->Lens' (BitVector s) (BitVector (a+b))
(++##) a b = lens (\x->(view a x) ++# (view b x)) (\s v -> let (v1, v2)=splitAtI $ bv2v v in (set a (v2bv v1) (set b (v2bv v2) s)))

lens_imm_i :: Lens' (BitVector 32) (BitVector 12)
lens_imm_i = (sliceLens d31 d31 ++## sliceLens d30 d25 ++## sliceLens d24 d21 ++## sliceLens d20 d20)
lens_imm_s :: Lens' (BitVector 32) (BitVector 12)
lens_imm_s = (sliceLens d31 d31 ++## sliceLens d30 d25 ++## sliceLens d11 d8 ++## sliceLens d7 d7)
lens_imm_b :: Lens' (BitVector 32) (BitVector 12)
lens_imm_b = (sliceLens d31 d31 ++## sliceLens d7 d7 ++## sliceLens d30 d25 ++## sliceLens d11 d8)
lens_imm_u :: Lens' (BitVector 32) (BitVector 20)
lens_imm_u = (sliceLens d31 d31 ++## sliceLens d30 d20 ++## sliceLens d19 d12)
lens_imm_j :: Lens' (BitVector 32) (BitVector 20)
lens_imm_j = (sliceLens d31 d31 ++## sliceLens d19 d12 ++## sliceLens d20 d20 ++## sliceLens d30 d25 ++## sliceLens d24 d21)



parse_aqrl :: BitVector 32->(Bit, Bit)
parse_aqrl = view lens_aq_rl
parse_imm_i :: BitVector 32->Immediate
parse_imm_i i = signExtend @_ @12 @20 (view lens_imm_i i)
parse_imm_s :: BitVector 32->Immediate
parse_imm_s i = signExtend @_ @12 @20 (view lens_imm_s i)
parse_imm_b :: BitVector 32->Immediate
parse_imm_b i = signExtend @_ @13 @19 (view lens_imm_b i ++# (0 :: BitVector 1))
parse_imm_u :: BitVector 32->Immediate
parse_imm_u i = (view lens_imm_u i ++# (0 :: BitVector 12))
parse_imm_j :: BitVector 32->Immediate
parse_imm_j i = signExtend @_ @21 @11 (view lens_imm_j i ++# (0 :: BitVector 1))

truncateLSB :: (KnownNat a, KnownNat b) => SNat b->BitVector (a+b)->BitVector a
truncateLSB _ = v2bv . fst . splitAtI . bv2v

truncate_imm_i :: Immediate->BitVector 12
truncate_imm_i=truncateB
truncate_imm_s :: Immediate->BitVector 12
truncate_imm_s=truncateB
truncate_imm_b :: Immediate->BitVector 12
truncate_imm_b=truncateB . (truncateLSB d1) 
truncate_imm_u :: Immediate->BitVector 20
truncate_imm_u=(truncateLSB d12)
truncate_imm_j :: Immediate->BitVector 20
truncate_imm_j=truncateB . (truncateLSB d1)


set_imm_i = (set lens_imm_i) . truncate_imm_i
set_imm_s = (set lens_imm_s) . truncate_imm_s
set_imm_b = (set lens_imm_b) . truncate_imm_b
set_imm_u = (set lens_imm_u) . truncate_imm_u
set_imm_j = (set lens_imm_j) . truncate_imm_j

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
parseJ i = J {imm=parse_imm_j i, rd=get_rd i}
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

placeFields :: InstructionParam->BitVector 32->BitVector 32
placeFields (R rs1 rs2 rd) = (set_rs1 rs1) . (set_rs2 rs2) . (set_rd rd)
placeFields (I rs1 imm rd) = (set_rs1 rs1) . (set_imm_i imm) . (set_rd rd)
placeFields (S rs1 rs2 imm) = (set_rs1 rs1) . (set_rs2 rs2) . (set_imm_s imm)
placeFields (B rs1 rs2 imm) = (set_rs1 rs1) . (set_rs2 rs2) . (set_imm_b imm)
placeFields (U imm rd) = (set_imm_u imm) . (set_rd rd)
placeFields (J imm rd) = (set_imm_j imm) . (set_rd rd)
placeFields (At aq rl rs1 rs2 rd) = (set_rs1 rs1) . (set_rs2 rs2) . (set_rd rd) . (set lens_aq_rl (aq, rl))
placeFields (Fl rs1 rs2 rd rm) = (set_rs1 rs1) . (set_rs2 rs2) . (set_rd rd) . (set_rm rm)
placeFields (Sh rs1 rd shamt) = (set_rs1 rs1) . (set_rd rd) . (set_shamt shamt)
placeFields (R4 rs1 rs2 rs3 rd rm) = (set_rs1 rs1) . (set_rs2 rs2) . (set_rs3 rs3) . (set_rd rd). (set_rm rm)
placeFields (Fc pred succ) = (set_pred pred) . (set_succ succ)

-- setR :: InstructionParam->BitVector 32->BitVector 32

data FlattenedInstructionParam = FlattenedInstructionParam {f_rs1 :: RegIndex, f_rs2 :: RegIndex, f_rs3 :: RegIndex, f_rd :: RegIndex,
                                                    f_imm :: Immediate, f_rm :: RoundMode, f_shamt :: Shamt, f_pred :: FenceArg, f_succ :: FenceArg, f_aq :: Bit, f_rl :: Bit} deriving Show
dontCareParam :: FlattenedInstructionParam
dontCareParam = FlattenedInstructionParam {f_rs1 = ($$(bLit ".....")), f_rs2 = ($$(bLit ".....")), f_rs3 = ($$(bLit ".....")),
f_rd = ($$(bLit ".....")), f_imm = ($$(bLit "................................")), f_rm = ($$(bLit "...")), f_shamt = ($$(bLit "......")),
f_pred = ($$(bLit "....")), f_succ = ($$(bLit "....")), f_aq=($$(bLit ".") :: BitVector 1) !0, f_rl=($$(bLit ".") :: BitVector 1)!0}
flattenInstructionParam :: InstructionParam->FlattenedInstructionParam 
flattenInstructionParam R {rs1, rs2, rd}= dontCareParam {f_rs1=rs1, f_rs2=rs2, f_rd=rd}
flattenInstructionParam I {rs1, imm, rd}= dontCareParam {f_rs1=rs1, f_imm=imm, f_rd=rd}
flattenInstructionParam S {rs1, rs2, imm}= dontCareParam {f_rs1=rs1, f_rs2=rs2, f_imm=imm}
flattenInstructionParam B {rs1, rs2, imm}= dontCareParam {f_rs1=rs1, f_rs2=rs2, f_imm=imm}
flattenInstructionParam U {imm, rd}= dontCareParam {f_imm=imm, f_rd=rd}
flattenInstructionParam J {imm, rd}= dontCareParam {f_imm=imm, f_rd=rd}
flattenInstructionParam At {aq, rl, rs1, rs2, rd}= dontCareParam {f_aq=aq, f_rl=rl, f_rs1=rs1, f_rs2=rs2, f_rd=rd}
flattenInstructionParam Fl {rs1, rs2, rd, rm}= dontCareParam {f_rs1=rs1, f_rs2=rs2, f_rd=rd, f_rm=rm}
flattenInstructionParam Sh {rs1, rd, shamt}= dontCareParam {f_rs1=rs1, f_rd=rd, f_shamt=shamt}
flattenInstructionParam R4 {rs1, rs2, rs3, rd, rm}= dontCareParam {f_rs1=rs1, f_rs2=rs2, f_rs3=rs3, f_rd=rd, f_rm=rm}
flattenInstructionParam Fc {RISCV.pred, RISCV.succ}= dontCareParam {f_pred=pred, f_succ=succ}

class InstructionMatch (s::Symbol) where
    inst :: BitVector 32->Maybe InstructionParam
class InstructionGen (s::Symbol) t | s->t where
    encodeInst :: t->BitVector 32

class InstructionMatch16 (s::Symbol) where
    inst16 :: BitVector 16->Bool

reg_x0 :: RegIndex
reg_x0 = 0
reg_x1 :: RegIndex
reg_x1 = 1
reg_x2 :: RegIndex
reg_x2 = 2
reg_x3 :: RegIndex
reg_x3 = 3
reg_x4 :: RegIndex
reg_x4 = 4
reg_x5 :: RegIndex
reg_x5 = 5
reg_x6 :: RegIndex
reg_x6 = 6
reg_x7 :: RegIndex
reg_x7 = 7
reg_x8 :: RegIndex
reg_x8 = 8
reg_x9 :: RegIndex
reg_x9 = 9
reg_x10 :: RegIndex
reg_x10 = 10
reg_x11 :: RegIndex
reg_x11 = 11
reg_x12 :: RegIndex
reg_x12 = 12
reg_x13 :: RegIndex
reg_x13 = 13
reg_x14 :: RegIndex
reg_x14 = 14
reg_x15 :: RegIndex
reg_x15 = 15
reg_x16 :: RegIndex
reg_x16 = 16
reg_x17 :: RegIndex
reg_x17 = 17
reg_x18 :: RegIndex
reg_x18 = 18
reg_x19 :: RegIndex
reg_x19 = 19
reg_x20 :: RegIndex
reg_x20 = 20
reg_x21 :: RegIndex
reg_x21 = 21
reg_x22 :: RegIndex
reg_x22 = 22
reg_x23 :: RegIndex
reg_x23 = 23
reg_x24 :: RegIndex
reg_x24 = 24
reg_x25 :: RegIndex
reg_x25 = 25
reg_x26 :: RegIndex
reg_x26 = 26
reg_x27 :: RegIndex
reg_x27 = 27
reg_x28 :: RegIndex
reg_x28 = 28
reg_x29 :: RegIndex
reg_x29 = 29
reg_x30 :: RegIndex
reg_x30 = 30
reg_x31 :: RegIndex
reg_x31 = 31

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
lens_rd :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_rd = lens (slice d11 d7) (flip (setSlice d11 d7))

get_rd = slice d11 d7
set_rd :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_rd = setSlice d11 d7
lens_rs1 :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_rs1 = lens (slice d19 d15) (flip (setSlice d19 d15))

get_rs1 = slice d19 d15
set_rs1 :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_rs1 = setSlice d19 d15
lens_rs2 :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_rs2 = lens (slice d24 d20) (flip (setSlice d24 d20))

get_rs2 = slice d24 d20
set_rs2 :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_rs2 = setSlice d24 d20
lens_rs3 :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_rs3 = lens (slice d31 d27) (flip (setSlice d31 d27))

get_rs3 = slice d31 d27
set_rs3 :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_rs3 = setSlice d31 d27
lens_aqrl :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 2)
lens_aqrl = lens (slice d26 d25) (flip (setSlice d26 d25))

get_aqrl = slice d26 d25
set_aqrl :: (BitPack a, BitSize a ~ 32)=>BitVector 2->a->a
set_aqrl = setSlice d26 d25
lens_pred :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 4)
lens_pred = lens (slice d27 d24) (flip (setSlice d27 d24))

get_pred = slice d27 d24
set_pred :: (BitPack a, BitSize a ~ 32)=>BitVector 4->a->a
set_pred = setSlice d27 d24
lens_succ :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 4)
lens_succ = lens (slice d23 d20) (flip (setSlice d23 d20))

get_succ = slice d23 d20
set_succ :: (BitPack a, BitSize a ~ 32)=>BitVector 4->a->a
set_succ = setSlice d23 d20
lens_rm :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 3)
lens_rm = lens (slice d14 d12) (flip (setSlice d14 d12))

get_rm = slice d14 d12
set_rm :: (BitPack a, BitSize a ~ 32)=>BitVector 3->a->a
set_rm = setSlice d14 d12
lens_imm20 :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 20)
lens_imm20 = lens (slice d31 d12) (flip (setSlice d31 d12))

get_imm20 = slice d31 d12
set_imm20 :: (BitPack a, BitSize a ~ 32)=>BitVector 20->a->a
set_imm20 = setSlice d31 d12
lens_jimm20 :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 20)
lens_jimm20 = lens (slice d31 d12) (flip (setSlice d31 d12))

get_jimm20 = slice d31 d12
set_jimm20 :: (BitPack a, BitSize a ~ 32)=>BitVector 20->a->a
set_jimm20 = setSlice d31 d12
lens_imm12 :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 12)
lens_imm12 = lens (slice d31 d20) (flip (setSlice d31 d20))

get_imm12 = slice d31 d20
set_imm12 :: (BitPack a, BitSize a ~ 32)=>BitVector 12->a->a
set_imm12 = setSlice d31 d20
lens_imm12hi :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 7)
lens_imm12hi = lens (slice d31 d25) (flip (setSlice d31 d25))

get_imm12hi = slice d31 d25
set_imm12hi :: (BitPack a, BitSize a ~ 32)=>BitVector 7->a->a
set_imm12hi = setSlice d31 d25
lens_bimm12hi :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 7)
lens_bimm12hi = lens (slice d31 d25) (flip (setSlice d31 d25))

get_bimm12hi = slice d31 d25
set_bimm12hi :: (BitPack a, BitSize a ~ 32)=>BitVector 7->a->a
set_bimm12hi = setSlice d31 d25
lens_imm12lo :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_imm12lo = lens (slice d11 d7) (flip (setSlice d11 d7))

get_imm12lo = slice d11 d7
set_imm12lo :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_imm12lo = setSlice d11 d7
lens_bimm12lo :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_bimm12lo = lens (slice d11 d7) (flip (setSlice d11 d7))

get_bimm12lo = slice d11 d7
set_bimm12lo :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_bimm12lo = setSlice d11 d7
lens_zimm :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_zimm = lens (slice d19 d15) (flip (setSlice d19 d15))

get_zimm = slice d19 d15
set_zimm :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_zimm = setSlice d19 d15
lens_shamt :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 6)
lens_shamt = lens (slice d25 d20) (flip (setSlice d25 d20))

get_shamt = slice d25 d20
set_shamt :: (BitPack a, BitSize a ~ 32)=>BitVector 6->a->a
set_shamt = setSlice d25 d20
lens_shamtw :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 5)
lens_shamtw = lens (slice d24 d20) (flip (setSlice d24 d20))

get_shamtw = slice d24 d20
set_shamtw :: (BitPack a, BitSize a ~ 32)=>BitVector 5->a->a
set_shamtw = setSlice d24 d20
lens_vseglen :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector 3)
lens_vseglen = lens (slice d31 d29) (flip (setSlice d31 d29))

get_vseglen = slice d31 d29
set_vseglen :: (BitPack a, BitSize a ~ 32)=>BitVector 3->a->a
set_vseglen = setSlice d31 d29
instance InstructionMatch "beq" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionGen "beq" TyB where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 0) . (setSlice d6 d2 0x18) . (setSlice d1 d0 3)) 0
instance InstructionMatch "bne" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionGen "bne" TyB where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 1) . (setSlice d6 d2 0x18) . (setSlice d1 d0 3)) 0
instance InstructionMatch "blt" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionGen "blt" TyB where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 4) . (setSlice d6 d2 0x18) . (setSlice d1 d0 3)) 0
instance InstructionMatch "bge" where inst i = if (slice d14 d12 i==5) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionGen "bge" TyB where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 5) . (setSlice d6 d2 0x18) . (setSlice d1 d0 3)) 0
instance InstructionMatch "bltu" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionGen "bltu" TyB where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 6) . (setSlice d6 d2 0x18) . (setSlice d1 d0 3)) 0
instance InstructionMatch "bgeu" where inst i = if (slice d14 d12 i==7) && (slice d6 d2 i==0x18) && (slice d1 d0 i==3) then Just $ parseB i else Nothing
instance InstructionGen "bgeu" TyB where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 7) . (setSlice d6 d2 0x18) . (setSlice d1 d0 3)) 0
instance InstructionMatch "jalr" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x19) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "jalr" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 0) . (setSlice d6 d2 0x19) . (setSlice d1 d0 3)) 0
instance InstructionMatch "jal" where inst i = if (slice d6 d2 i==0x1b) && (slice d1 d0 i==3) then Just $ parseJ i else Nothing
instance InstructionGen "jal" TyJ where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d6 d2 0x1b) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lui" where inst i = if (slice d6 d2 i==0x0D) && (slice d1 d0 i==3) then Just $ parseU i else Nothing
instance InstructionGen "lui" TyU where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d6 d2 0x0D) . (setSlice d1 d0 3)) 0
instance InstructionMatch "auipc" where inst i = if (slice d6 d2 i==0x05) && (slice d1 d0 i==3) then Just $ parseU i else Nothing
instance InstructionGen "auipc" TyU where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d6 d2 0x05) . (setSlice d1 d0 3)) 0
instance InstructionMatch "addi" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "addi" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 0) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "slli" where inst i = if (slice d31 d26 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionGen "slli" TySh where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d26 0) . (setSlice d14 d12 1) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "slti" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "slti" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 2) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sltiu" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "sltiu" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 3) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "xori" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "xori" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 4) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "srli" where inst i = if (slice d31 d26 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionGen "srli" TySh where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d26 0) . (setSlice d14 d12 5) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "srai" where inst i = if (slice d31 d26 i==16) && (slice d14 d12 i==5) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionGen "srai" TySh where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d26 16) . (setSlice d14 d12 5) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "ori" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "ori" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 6) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "andi" where inst i = if (slice d14 d12 i==7) && (slice d6 d2 i==0x04) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "andi" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 7) . (setSlice d6 d2 0x04) . (setSlice d1 d0 3)) 0
instance InstructionMatch "add" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "add" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 0) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sub" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sub" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 32) . (setSlice d14 d12 0) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sll" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sll" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 1) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "slt" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "slt" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sltu" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sltu" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "xor" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==4) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "xor" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 4) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "srl" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "srl" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 5) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sra" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sra" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 32) . (setSlice d14 d12 5) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "or" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==6) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "or" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 6) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "and" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==7) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "and" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 7) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "addiw" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "addiw" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 0) . (setSlice d6 d2 0x06) . (setSlice d1 d0 3)) 0
instance InstructionMatch "slliw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionGen "slliw" TySh where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 1) . (setSlice d6 d2 0x06) . (setSlice d1 d0 3)) 0
instance InstructionMatch "srliw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionGen "srliw" TySh where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 5) . (setSlice d6 d2 0x06) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sraiw" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==5) && (slice d6 d2 i==0x06) && (slice d1 d0 i==3) then Just $ parseSh i else Nothing
instance InstructionGen "sraiw" TySh where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 32) . (setSlice d14 d12 5) . (setSlice d6 d2 0x06) . (setSlice d1 d0 3)) 0
instance InstructionMatch "addw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "addw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 0) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "subw" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "subw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 32) . (setSlice d14 d12 0) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sllw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==1) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sllw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 1) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "srlw" where inst i = if (slice d31 d25 i==0) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "srlw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 0) . (setSlice d14 d12 5) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sraw" where inst i = if (slice d31 d25 i==32) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sraw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 32) . (setSlice d14 d12 5) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lb" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "lb" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 0) . (setSlice d6 d2 0x00) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lh" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "lh" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 1) . (setSlice d6 d2 0x00) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "lw" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 2) . (setSlice d6 d2 0x00) . (setSlice d1 d0 3)) 0
instance InstructionMatch "ld" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "ld" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 3) . (setSlice d6 d2 0x00) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lbu" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "lbu" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 4) . (setSlice d6 d2 0x00) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lhu" where inst i = if (slice d14 d12 i==5) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "lhu" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 5) . (setSlice d6 d2 0x00) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lwu" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x00) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "lwu" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 6) . (setSlice d6 d2 0x00) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sb" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionGen "sb" TyS where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 0) . (setSlice d6 d2 0x08) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sh" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionGen "sh" TyS where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 1) . (setSlice d6 d2 0x08) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionGen "sw" TyS where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 2) . (setSlice d6 d2 0x08) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sd" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x08) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionGen "sd" TyS where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 3) . (setSlice d6 d2 0x08) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fence" where inst i = if (slice d14 d12 i==0) && (slice d6 d2 i==0x03) && (slice d1 d0 i==3) then Just $ parseFc i else Nothing
instance InstructionGen "fence" TyFc where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 0) . (setSlice d6 d2 0x03) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fence.i" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x03) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "fence.i" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 1) . (setSlice d6 d2 0x03) . (setSlice d1 d0 3)) 0
instance InstructionMatch "mul" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "mul" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 0) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "mulh" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==1) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "mulh" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 1) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "mulhsu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "mulhsu" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "mulhu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "mulhu" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "div" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==4) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "div" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 4) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "divu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "divu" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 5) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "rem" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==6) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "rem" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 6) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "remu" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==7) && (slice d6 d2 i==0x0C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "remu" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 7) . (setSlice d6 d2 0x0C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "mulw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==0) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "mulw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 0) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "divw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==4) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "divw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 4) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "divuw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==5) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "divuw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 5) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "remw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==6) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "remw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 6) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "remuw" where inst i = if (slice d31 d25 i==1) && (slice d14 d12 i==7) && (slice d6 d2 i==0x0E) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "remuw" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d25 1) . (setSlice d14 d12 7) . (setSlice d6 d2 0x0E) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoadd.w" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoadd.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 0) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoxor.w" where inst i = if (slice d31 d29 i==1) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoxor.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 1) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoor.w" where inst i = if (slice d31 d29 i==2) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoor.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 2) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoand.w" where inst i = if (slice d31 d29 i==3) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoand.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 3) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amomin.w" where inst i = if (slice d31 d29 i==4) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amomin.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 4) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amomax.w" where inst i = if (slice d31 d29 i==5) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amomax.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 5) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amominu.w" where inst i = if (slice d31 d29 i==6) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amominu.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 6) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amomaxu.w" where inst i = if (slice d31 d29 i==7) && (slice d28 d27 i==0) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amomaxu.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 7) . (setSlice d28 d27 0) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoswap.w" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==1) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoswap.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 0) . (setSlice d28 d27 1) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lr.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d29 i==0) && (slice d28 d27 i==2) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "lr.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d29 0) . (setSlice d28 d27 2) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sc.w" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==3) && (slice d14 d12 i==2) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "sc.w" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 0) . (setSlice d28 d27 3) . (setSlice d14 d12 2) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoadd.d" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoadd.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 0) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoxor.d" where inst i = if (slice d31 d29 i==1) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoxor.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 1) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoor.d" where inst i = if (slice d31 d29 i==2) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoor.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 2) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoand.d" where inst i = if (slice d31 d29 i==3) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoand.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 3) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amomin.d" where inst i = if (slice d31 d29 i==4) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amomin.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 4) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amomax.d" where inst i = if (slice d31 d29 i==5) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amomax.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 5) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amominu.d" where inst i = if (slice d31 d29 i==6) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amominu.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 6) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amomaxu.d" where inst i = if (slice d31 d29 i==7) && (slice d28 d27 i==0) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amomaxu.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 7) . (setSlice d28 d27 0) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "amoswap.d" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==1) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "amoswap.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 0) . (setSlice d28 d27 1) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "lr.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d29 i==0) && (slice d28 d27 i==2) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "lr.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d29 0) . (setSlice d28 d27 2) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sc.d" where inst i = if (slice d31 d29 i==0) && (slice d28 d27 i==3) && (slice d14 d12 i==3) && (slice d6 d2 i==0x0B) && (slice d1 d0 i==3) then Just $ parseAt i else Nothing
instance InstructionGen "sc.d" TyAt where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d29 0) . (setSlice d28 d27 3) . (setSlice d14 d12 3) . (setSlice d6 d2 0x0B) . (setSlice d1 d0 3)) 0
instance InstructionMatch "ecall" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x000) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "ecall" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d19 d15 0) . (setSlice d31 d20 0x000) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "ebreak" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x001) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "ebreak" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d19 d15 0) . (setSlice d31 d20 0x001) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "uret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x002) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "uret" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d19 d15 0) . (setSlice d31 d20 0x002) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x102) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sret" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d19 d15 0) . (setSlice d31 d20 0x102) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "mret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x302) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "mret" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d19 d15 0) . (setSlice d31 d20 0x302) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "dret" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x7b2) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "dret" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d19 d15 0) . (setSlice d31 d20 0x7b2) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "sfence.vma" where inst i = if (slice d11 d7 i==0) && (slice d31 d25 i==0x09) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "sfence.vma" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d31 d25 0x09) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "wfi" where inst i = if (slice d11 d7 i==0) && (slice d19 d15 i==0) && (slice d31 d20 i==0x105) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "wfi" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d19 d15 0) . (setSlice d31 d20 0x105) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "csrrw" where inst i = if (slice d14 d12 i==1) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "csrrw" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 1) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "csrrs" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "csrrs" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 2) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "csrrc" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "csrrc" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 3) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "csrrwi" where inst i = if (slice d14 d12 i==5) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "csrrwi" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 5) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "csrrsi" where inst i = if (slice d14 d12 i==6) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "csrrsi" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 6) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "csrrci" where inst i = if (slice d14 d12 i==7) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "csrrci" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 7) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "hfence.vvma" where inst i = if (slice d11 d7 i==0) && (slice d31 d25 i==0x11) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "hfence.vvma" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d31 d25 0x11) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "hfence.gvma" where inst i = if (slice d11 d7 i==0) && (slice d31 d25 i==0x31) && (slice d14 d12 i==0) && (slice d6 d2 i==0x1C) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "hfence.gvma" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d11 d7 0) . (setSlice d31 d25 0x31) . (setSlice d14 d12 0) . (setSlice d6 d2 0x1C) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fadd.s" where inst i = if (slice d31 d27 i==0x00) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fadd.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x00) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsub.s" where inst i = if (slice d31 d27 i==0x01) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fsub.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x01) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmul.s" where inst i = if (slice d31 d27 i==0x02) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fmul.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x02) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fdiv.s" where inst i = if (slice d31 d27 i==0x03) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fdiv.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x03) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnj.s" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnj.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 0) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnjn.s" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnjn.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 1) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnjx.s" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==2) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnjx.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 2) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmin.s" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmin.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x05) . (setSlice d14 d12 0) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmax.s" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmax.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x05) . (setSlice d14 d12 1) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsqrt.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x0B) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fsqrt.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x0B) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fadd.d" where inst i = if (slice d31 d27 i==0x00) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fadd.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x00) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsub.d" where inst i = if (slice d31 d27 i==0x01) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fsub.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x01) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmul.d" where inst i = if (slice d31 d27 i==0x02) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fmul.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x02) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fdiv.d" where inst i = if (slice d31 d27 i==0x03) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fdiv.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x03) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnj.d" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnj.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 0) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnjn.d" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnjn.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 1) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnjx.d" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==2) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnjx.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 2) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmin.d" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmin.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x05) . (setSlice d14 d12 0) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmax.d" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmax.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x05) . (setSlice d14 d12 1) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.s.d" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x08) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.s.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x08) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.d.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x08) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.d.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x08) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsqrt.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x0B) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fsqrt.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x0B) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fadd.q" where inst i = if (slice d31 d27 i==0x00) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fadd.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x00) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsub.q" where inst i = if (slice d31 d27 i==0x01) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fsub.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x01) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmul.q" where inst i = if (slice d31 d27 i==0x02) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fmul.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x02) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fdiv.q" where inst i = if (slice d31 d27 i==0x03) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fdiv.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x03) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnj.q" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnj.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 0) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnjn.q" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnjn.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 1) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsgnjx.q" where inst i = if (slice d31 d27 i==0x04) && (slice d14 d12 i==2) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fsgnjx.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x04) . (setSlice d14 d12 2) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmin.q" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmin.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x05) . (setSlice d14 d12 0) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmax.q" where inst i = if (slice d31 d27 i==0x05) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmax.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x05) . (setSlice d14 d12 1) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.s.q" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x08) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.s.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x08) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.q.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x08) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.q.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x08) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.d.q" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x08) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.d.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x08) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.q.d" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x08) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.q.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x08) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsqrt.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x0B) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fsqrt.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x0B) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fle.s" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fle.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 0) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "flt.s" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "flt.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 1) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "feq.s" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==2) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "feq.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 2) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fle.d" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fle.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 0) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "flt.d" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "flt.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 1) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "feq.d" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==2) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "feq.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 2) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fle.q" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fle.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 0) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "flt.q" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "flt.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 1) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "feq.q" where inst i = if (slice d31 d27 i==0x14) && (slice d14 d12 i==2) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "feq.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d31 d27 0x14) . (setSlice d14 d12 2) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.w.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.w.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x18) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.wu.s" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.wu.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x18) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.l.s" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.l.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 2) . (setSlice d31 d27 0x18) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.lu.s" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x18) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.lu.s" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x18) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmv.x.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmv.x.w" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1C) . (setSlice d14 d12 0) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fclass.s" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==1) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fclass.s" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1C) . (setSlice d14 d12 1) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.w.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.w.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x18) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.wu.d" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.wu.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x18) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.l.d" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.l.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 2) . (setSlice d31 d27 0x18) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.lu.d" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x18) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.lu.d" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x18) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmv.x.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmv.x.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1C) . (setSlice d14 d12 0) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fclass.d" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==1) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fclass.d" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1C) . (setSlice d14 d12 1) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.w.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.w.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x18) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.wu.q" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.wu.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x18) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.l.q" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.l.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 2) . (setSlice d31 d27 0x18) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.lu.q" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x18) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.lu.q" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x18) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmv.x.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmv.x.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1C) . (setSlice d14 d12 0) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fclass.q" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1C) && (slice d14 d12 i==1) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fclass.q" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1C) . (setSlice d14 d12 1) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.s.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.s.w" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.s.wu" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.s.wu" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.s.l" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.s.l" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 2) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.s.lu" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.s.lu" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmv.w.x" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1E) && (slice d14 d12 i==0) && (slice d26 d25 i==0) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmv.w.x" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1E) . (setSlice d14 d12 0) . (setSlice d26 d25 0) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.d.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.d.w" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.d.wu" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.d.wu" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.d.l" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.d.l" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 2) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.d.lu" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.d.lu" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmv.d.x" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1E) && (slice d14 d12 i==0) && (slice d26 d25 i==1) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmv.d.x" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1E) . (setSlice d14 d12 0) . (setSlice d26 d25 1) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.q.w" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.q.w" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.q.wu" where inst i = if (slice d24 d20 i==1) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.q.wu" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 1) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.q.l" where inst i = if (slice d24 d20 i==2) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.q.l" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 2) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fcvt.q.lu" where inst i = if (slice d24 d20 i==3) && (slice d31 d27 i==0x1A) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseFl i else Nothing
instance InstructionGen "fcvt.q.lu" TyFl where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 3) . (setSlice d31 d27 0x1A) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmv.q.x" where inst i = if (slice d24 d20 i==0) && (slice d31 d27 i==0x1E) && (slice d14 d12 i==0) && (slice d26 d25 i==3) && (slice d6 d2 i==0x14) && (slice d1 d0 i==3) then Just $ parseR i else Nothing
instance InstructionGen "fmv.q.x" TyR where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d24 d20 0) . (setSlice d31 d27 0x1E) . (setSlice d14 d12 0) . (setSlice d26 d25 3) . (setSlice d6 d2 0x14) . (setSlice d1 d0 3)) 0
instance InstructionMatch "flw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x01) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "flw" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 2) . (setSlice d6 d2 0x01) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fld" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x01) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "fld" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 3) . (setSlice d6 d2 0x01) . (setSlice d1 d0 3)) 0
instance InstructionMatch "flq" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x01) && (slice d1 d0 i==3) then Just $ parseI i else Nothing
instance InstructionGen "flq" TyI where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 4) . (setSlice d6 d2 0x01) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsw" where inst i = if (slice d14 d12 i==2) && (slice d6 d2 i==0x09) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionGen "fsw" TyS where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 2) . (setSlice d6 d2 0x09) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsd" where inst i = if (slice d14 d12 i==3) && (slice d6 d2 i==0x09) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionGen "fsd" TyS where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 3) . (setSlice d6 d2 0x09) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fsq" where inst i = if (slice d14 d12 i==4) && (slice d6 d2 i==0x09) && (slice d1 d0 i==3) then Just $ parseS i else Nothing
instance InstructionGen "fsq" TyS where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d14 d12 4) . (setSlice d6 d2 0x09) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmadd.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x10) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fmadd.s" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 0) . (setSlice d6 d2 0x10) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmsub.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x11) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fmsub.s" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 0) . (setSlice d6 d2 0x11) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fnmsub.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x12) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fnmsub.s" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 0) . (setSlice d6 d2 0x12) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fnmadd.s" where inst i = if (slice d26 d25 i==0) && (slice d6 d2 i==0x13) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fnmadd.s" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 0) . (setSlice d6 d2 0x13) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmadd.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x10) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fmadd.d" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 1) . (setSlice d6 d2 0x10) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmsub.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x11) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fmsub.d" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 1) . (setSlice d6 d2 0x11) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fnmsub.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x12) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fnmsub.d" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 1) . (setSlice d6 d2 0x12) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fnmadd.d" where inst i = if (slice d26 d25 i==1) && (slice d6 d2 i==0x13) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fnmadd.d" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 1) . (setSlice d6 d2 0x13) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmadd.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x10) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fmadd.q" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 3) . (setSlice d6 d2 0x10) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fmsub.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x11) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fmsub.q" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 3) . (setSlice d6 d2 0x11) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fnmsub.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x12) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fnmsub.q" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 3) . (setSlice d6 d2 0x12) . (setSlice d1 d0 3)) 0
instance InstructionMatch "fnmadd.q" where inst i = if (slice d26 d25 i==3) && (slice d6 d2 i==0x13) && (slice d1 d0 i==3) then Just $ parseR4 i else Nothing
instance InstructionGen "fnmadd.q" TyR4 where encodeInst f = ((placeFields (toSumParam f)) . (setSlice d26 d25 3) . (setSlice d6 d2 0x13) . (setSlice d1 d0 3)) 0
instance InstructionMatch16 "c.addi4spn" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==0) 
instance InstructionMatch16 "c.fld" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==1) 
instance InstructionMatch16 "c.lw" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==2) 
instance InstructionMatch16 "c.flw" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==3) 
instance InstructionMatch16 "c.fsd" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==5) 
instance InstructionMatch16 "c.sw" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==6) 
instance InstructionMatch16 "c.fsw" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==7) 
instance InstructionMatch16 "c.addi" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==0) 
instance InstructionMatch16 "c.jal" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==1) 
instance InstructionMatch16 "c.li" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==2) 
instance InstructionMatch16 "c.lui" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==3) 
instance InstructionMatch16 "c.srli" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d11 d10 i==0) 
instance InstructionMatch16 "c.srai" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d11 d10 i==1) 
instance InstructionMatch16 "c.andi" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d11 d10 i==2) 
instance InstructionMatch16 "c.sub" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==0) && (slice d11 d10 i==3) && (slice d6 d5 i==0) 
instance InstructionMatch16 "c.xor" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==0) && (slice d11 d10 i==3) && (slice d6 d5 i==1) 
instance InstructionMatch16 "c.or" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==0) && (slice d11 d10 i==3) && (slice d6 d5 i==2) 
instance InstructionMatch16 "c.and" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==0) && (slice d11 d10 i==3) && (slice d6 d5 i==3) 
instance InstructionMatch16 "c.subw" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==1) && (slice d11 d10 i==3) && (slice d6 d5 i==0) 
instance InstructionMatch16 "c.addw" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==1) && (slice d11 d10 i==3) && (slice d6 d5 i==1) 
instance InstructionMatch16 "c.j" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==5) 
instance InstructionMatch16 "c.beqz" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==6) 
instance InstructionMatch16 "c.bnez" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==7) 
instance InstructionMatch16 "c.slli" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==0) 
instance InstructionMatch16 "c.fldsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==1) 
instance InstructionMatch16 "c.lwsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==2) 
instance InstructionMatch16 "c.flwsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==3) 
instance InstructionMatch16 "c.mv" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==4) && (slice d12 d12 i==0) 
instance InstructionMatch16 "c.add" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==4) && (slice d12 d12 i==1) 
instance InstructionMatch16 "c.fsdsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==5) 
instance InstructionMatch16 "c.swsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==6) 
instance InstructionMatch16 "c.fswsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==7) 
instance InstructionMatch16 "@c.nop" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==0) && (slice d12 d12 i==0) && (slice d11 d7 i==0) && (slice d6 d2 i==0) 
instance InstructionMatch16 "@c.addi16sp" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==3) && (slice d11 d7 i==2) 
instance InstructionMatch16 "@c.jr" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==4) && (slice d12 d12 i==0) && (slice d6 d2 i==0) 
instance InstructionMatch16 "@c.jalr" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==4) && (slice d12 d12 i==1) && (slice d6 d2 i==0) 
instance InstructionMatch16 "@c.ebreak" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==4) && (slice d12 d12 i==1) && (slice d11 d7 i==0) && (slice d6 d2 i==0) 
instance InstructionMatch16 "@c.srli.rv32" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==0) && (slice d11 d10 i==0) 
instance InstructionMatch16 "@c.srai.rv32" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==4) && (slice d12 d12 i==0) && (slice d11 d10 i==1) 
instance InstructionMatch16 "@c.slli.rv32" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==0) && (slice d12 d12 i==0) 
instance InstructionMatch16 "@c.ld" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==3) 
instance InstructionMatch16 "@c.sd" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==7) 
instance InstructionMatch16 "@c.addiw" where inst16 i = (slice d1 d0 i==1) && (slice d15 d13 i==1) 
instance InstructionMatch16 "@c.ldsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==3) 
instance InstructionMatch16 "@c.sdsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==7) 
instance InstructionMatch16 "@c.lq" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==1) 
instance InstructionMatch16 "@c.sq" where inst16 i = (slice d1 d0 i==0) && (slice d15 d13 i==5) 
instance InstructionMatch16 "@c.lqsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==1) 
instance InstructionMatch16 "@c.sqsp" where inst16 i = (slice d1 d0 i==2) && (slice d15 d13 i==5) 
