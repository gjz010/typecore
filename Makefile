.PHONY: ghci decoder verilog
ghci:
	stack repl --with-ghc clash --no-load
decoder: src/RISCV.hs
src/RISCV.hs: tools/RiscVOpcodesParser.hs riscv-tools/opcodes/opcodes
	cat riscv-tools/opcodes/opcodes | stack exec runhaskell -- tools/RiscVOpcodesParser.hs > src/RISCV.hs

verilog:
	stack exec clash -- --verilog -fconstraint-solver-iterations=0  -fclash-spec-limit=10000 -i./src Typecore -outputdir build