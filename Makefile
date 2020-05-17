analysis:
	eval $(opam config env) && cd ControlFlowAnalysis && dune build intabs.exe

all: 
	analysis