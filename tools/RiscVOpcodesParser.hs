{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module RiscVOpcodesParser where
    import Prelude
    import Text.RawString.QQ
    import Data.List
    import Data.Maybe
    import Data.List.Split
    import Debug.Trace
    bitfields :: [(String, (Int, Int))]
    bitfields = [("rd",(11,7)),
                ("rs1",(19,15)),
                ("rs2",(24,20)),
                ("rs3",(31,27)),
                ("aqrl",(26,25)),
                ("pred",(27,24)),
                ("succ",(23,20)),
                ("rm",(14,12)),
                ("imm20",(31,12)),
                ("jimm20",(31,12)),
                ("imm12",(31,20)),
                ("imm12hi",(31,25)),
                ("bimm12hi",(31,25)),
                ("imm12lo",(11,7)),
                ("bimm12lo",(11,7)),
                ("zimm",(19,15)),
                ("shamt",(25,20)),
                ("shamtw",(24,20)),
                ("vseglen",(31,29))]
    fieldExtractor :: (String, (Int, Int))->String
    fieldExtractor (field, (hi, lo)) =  "lens_"++field++" :: (BitPack a, BitSize a ~ 32) => Lens' a (BitVector "++(show $ hi+1-lo)++")\n"
                                        ++ "lens_"++field++" = lens (slice d"++(show hi)++" d"++(show lo)++") (flip (setSlice d"++(show hi)++" d"++(show lo)++"))\n" 
                                        ++"\nget_"++field++" = slice d"++(show hi)++" d"++(show lo)
                                        ++"\nset_"++field++" :: (BitPack a, BitSize a ~ 32)=>BitVector "++(show $ hi+1-lo)++"->a->a"
                                        ++"\nset_"++field++" = setSlice d"++(show hi)++" d"++(show lo)
    causes :: [(Int,String)]
    causes = [(0x00, "MisalignedFetch"),
                        (0x01, "FetchAccess"),
                        (0x02, "IllegalInstruction"),
                        (0x03, "BreakPoint"),
                        (0x04, "MisalignedLoad"),
                        (0x05, "LoadAccess"),
                        (0x06, "MisalignedStore"),
                        (0x07, "StoreAccess"),
                        (0x08, "UserECall"),
                        (0x09, "SupervisorECall"),
                        (0x0A, "HypervisorECall"),
                        (0x0B, "MachineECall"),
                        (0x0C, "FetchPageFault"),
                        (0x0D, "LoadPageFault"),
                        (0x0F, "StorePageFault")]
    causeChecker :: [(Int, String)]->String
    causeChecker list = decl ++ "\n" ++ dtype ++ "\n" ++ (intercalate "\n" (map go list)) ++ "\n" ++ fin where
            decl = "data Cause = UnknownCause | "++(intercalate " | " (map snd list))++"  deriving Show"
            dtype = "cause :: (Num a, Eq a)=>a->Cause"
            go (val,err) = "cause "++(show val)++" = "++err
            fin = "cause _ = UnknownCause"
    header :: String
    header =[r|-- This file is generated automatically.
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
|]

    data InstructionParamType = R | I | S | B | U | J | At | Fl | Sh | R4 | Fc deriving Show
    checkType:: [String]->InstructionParamType
    checkType list = case list of
        (elem "bimm12hi" -> True) -> B
        (elem "jimm20" -> True) -> J
        (elem "imm12" -> True) -> I
        (elem "imm20" -> True) -> U
        (elem "shamt" -> True) -> Sh
        (elem "shamtw" -> True) -> Sh
        (elem "imm12hi" -> True) -> S
        (elem "pred" -> True) -> Fc
        (elem "aqrl" -> True) -> At
        (elem "rs3" -> True) -> R4
        (elem "rm" -> True) -> Fl
        _ -> R

    getBitPattern :: [String]->[String]
    getBitPattern args = trace (show args) $ mapMaybe (\x->if elem '=' x 
                                            then 
                                                let [hi, lo, v]=if elem '.' x then (let [r,v]=splitOn "=" x in splitOn ".." r ++ [v]) else (let [b,v] = splitOn "=" x in [b,b,v])
                                                    in if v=="ignore" then Nothing else Just ("slice d"++(hi)++" d"++(lo)++" i=="++(v))
                                            else Nothing) (tail args)
    getBitSetter :: [String]->[String]
    getBitSetter args = trace (show args) $ mapMaybe (\x->if elem '=' x 
                                            then 
                                                let [hi, lo, v]=if elem '.' x then (let [r,v]=splitOn "=" x in splitOn ".." r ++ [v]) else (let [b,v] = splitOn "=" x in [b,b,v])
                                                    in if v=="ignore" then Nothing else Just ("setSlice d"++(hi)++" d"++(lo)++" "++(v)++"")
                                            else Nothing) (tail args)                                           
    isRVC :: String->Bool
    isRVC (isPrefixOf "c."->True)=True
    isRVC (isPrefixOf "@c."->True)=True
    isRVC _ = False
    matchInsn :: [String]->String
    matchInsn args@(isRVC . head -> True) = "instance InstructionMatch16 \""++(head args)++"\" where inst16 i = ("++(intercalate ") && (" $ getBitPattern args)++") "
    matchInsn args = "instance InstructionMatch \""++(head args)++"\" where inst i = if ("++(intercalate ") && (" $ getBitPattern args)++") then Just $ parse"++(show $ checkType args)++" i else Nothing\n"
                    ++ ("instance InstructionGen \""++(head args)++"\" Ty"++(show $ checkType args)++" where encodeInst f = ((placeFields (toSumParam f)) . (" ++(intercalate ") . (" $ getBitSetter args) ++")) 0")
                    
    registerDecls :: String
    registerDecls = concatMap (\x->"reg_x"++(show x)++" :: RegIndex\nreg_x"++(show x)++" = "++(show x)++"\n") [0..31]

    riscv :: [[String]]->String
    riscv opcodes= intercalate "\n" $ [header, registerDecls, causeChecker causes, intercalate "\n" $ map fieldExtractor bitfields] ++ (map matchInsn opcodes)
    main :: IO ()
    main = do
        input<-getContents
        putStrLn $ riscv $ map words $ filter (not . null) $ map (fst . span (/='#')) $ filter (\x->length x>0) $ lines input
    