{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
module RiscVOpcodesParser where
    import Prelude
    import Text.RawString.QQ
    import Text.Parsec
    import qualified Data.Map.Lazy as M
    import Data.List
    import Data.Maybe
    import Data.List.Split
    bitfields :: M.Map String (Int, Int)
    bitfields = M.fromList [("rd",(11,7)),
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
    fieldExtractor (field, (hi, lo)) = "get_"++field++" :: (BitPack a, BitSize a ~ 32)=>a->BitVector "++(show $ hi+1-lo)
                                        ++"\nget_"++field++" = slice d"++(show hi)++" d"++(show lo)
    
    causes :: M.Map Int String
    causes = M.fromList [(0x00, "MisalignedFetch"),
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
    getBitPattern args = mapMaybe (\x->if elem '=' x 
                                            then let [hi, lo, v]=(let [r,v]=splitOn "=" x in splitOn ".." r ++ [v])
                                                    in if v=="ignore" then Nothing else Just ("slice d"++(hi)++" d"++(lo)++" i=="++(v))
                                            else Nothing) (tail args)
    matchInsn :: [String]->String
    matchInsn args = "instance InstructionMatch \""++(head args)++"\" where inst i = if ("++(intercalate ") && (" $ getBitPattern args)++") then Just $ parse"++(show $ checkType args)++" i else Nothing"
    
    

    riscv :: [[String]]->String
    riscv opcodes= intercalate "\n" $ [header, causeChecker $ M.toList causes, intercalate "\n" $ map fieldExtractor $ M.toList bitfields] ++ (map matchInsn opcodes)
    main :: IO ()
    main = do
        input<-getContents
        putStrLn $ riscv $ map words $ filter (\x->head x/='#') $ filter (\x->length x>0) $ lines input
    