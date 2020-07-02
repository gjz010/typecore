module Typecore.MicroDecoder where
import RISCV
import RISCV.RVC
{-
The microdecoder that generates the instruction batch.
- Decode RVC instructions.
- Isolate special instructions, making sure they are the last in pack.
- Do branch prediction and prevent two branches from appearing in one pack.
-}

