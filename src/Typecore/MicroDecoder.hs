module Typecore.MicroDecoder where
import Typecore.Prelude
import RISCV
import RISCV.RVC
import Clash.Prelude
import Control.Lens hiding ((:>))
import Control.Monad.Reader
{-
The microdecoder that generates the instruction batch.
- Decode RVC instructions.
- Isolate special instructions, making sure they are the last in pack.
- Do branch prediction and prevent two branches from appearing in one pack.
-}

data InsnSplitInput = InsnSplitInput {
    _currentPC :: TWord,
    _input :: BitVector 128
} deriving (Generic, NFDataX)

data InsnSplitResult = InsnSplitResult {
    _nextPC :: TWord, -- The PC for next instruction fetch: this is the right op if there is no branch.
    _insnPack :: Vec 4 (BitVector 32),
    _compressed :: BitVector 4 
} deriving (Generic, NFDataX)

makeLenses ''InsnSplitInput
makeLenses ''InsnSplitResult

decodePack :: BitVector 32->(BitVector 32, Bool)
decodePack insn = case decodeInstruction insn of
    Full i -> (i, False)
    Compressed i->(i, True)
    BadInstruction->(0, False)

insn1 :: Bool->BitVector 128->BitVector 32
insn1 False = slice d63 d32
insn1 True = slice d47 d16
insn2 :: Bool->Bool->BitVector 128->BitVector 32
insn2 False False = slice d95 d64
insn2 True True = slice d63 d32
insn2 _ _ = slice d79 d48
insn3 :: Bool->Bool->Bool->BitVector 128->BitVector 32
insn3 False False False = slice d127 d96
insn3 True False False = slice d111 d80
insn3 False True False = insn3 True False False
insn3 False False True = insn3 True False False
insn3 True True True = slice d79 d48
insn3 _ _ _ = slice d95 d64


{-
Indicate whether such an instruction needs special care when issuing.
Branch: a branch instruction needs to go through a branch predictor.
    Branches have to be the last in the pack.
Jump: a jump instruction needs to go through a branch predictor or RAS.
    Jumps have to be the last in the pack.
Special: special instructions that change the machine state but will not lead to an exception (examples: CSR, counterexamples: EBREAK).
    These instructions have to be executed alone, at the first slot.
-}
data SpecialInsn = Branch | Jump | Special | Normal


isControlTransfer :: BitVector 32->SpecialInsn
isControlTransfer (inst @"beq" -> Just _) = Branch
isControlTransfer (inst @"bne" -> Just _) = Branch
isControlTransfer (inst @"blt" -> Just _) = Branch
isControlTransfer (inst @"bge" -> Just _) = Branch
isControlTransfer (inst @"bltu" -> Just _) = Branch
isControlTransfer (inst @"bgeu" -> Just _) = Branch
isControlTransfer (inst @"jal" -> Just _) = Jump
isControlTransfer (inst @"jalr" -> Just _) = Jump
isControlTransfer (inst @"fence"->Just _) = Special
isControlTransfer (inst @"fence.i"->Just _) = Special
isControlTransfer (inst @"sfence.vma"->Just _) = Special
isControlTransfer (inst @"wfi"->Just _) = Special
isControlTransfer (inst @"csrrw"->Just _) = Special
isControlTransfer (inst @"csrrs"->Just _) = Special
isControlTransfer (inst @"csrrc"->Just _) = Special
isControlTransfer (inst @"csrrwi"->Just _) = Special
isControlTransfer (inst @"csrrsi"->Just _) = Special
isControlTransfer (inst @"csrrci"->Just _) = Special
isControlTransfer _ = Normal



microDecoderS1 :: InsnSplitInput->InsnSplitResult
microDecoderS1 = runReader $ do
    insnpack<-asks (view input)
    let (insn_0, compressed_0) = decodePack (slice d31 d0 insnpack)
    let (insn_1, compressed_1) = decodePack (insn1 compressed_0 insnpack)
    let (insn_2, compressed_2) = decodePack (insn2 compressed_0 compressed_1 insnpack)
    let (insn_3, compressed_3) = decodePack (insn3 compressed_0 compressed_1 compressed_2 insnpack)
    let d x = (if x then 16 else 32)
    let b x = if x then 1 else 0
    let td = (d compressed_0 + d compressed_1 + d compressed_2 + d compressed_3)
    pc<-asks (view currentPC)
    return $ InsnSplitResult {
        _nextPC = pc+td, 
        _insnPack = insn_3 :> insn_2 :> insn_1 :> insn_0 :> Nil, 
        _compressed = v2bv (b compressed_3 :> b compressed_2 :> b compressed_1 :> b compressed_0 :> Nil)
    }
