.PHONY: ghci decoder
ghci:
	stack repl --with-ghc clash --no-load
decoder: src/RISCV.hs
src/RISCV.hs: tools/RiscVOpcodesParser.hs riscv-tools/opcodes/opcodes
	cat riscv-tools/opcodes/opcodes | stack exec runhaskell -- tools/RiscVOpcodesParser.hs > src/RISCV.hs