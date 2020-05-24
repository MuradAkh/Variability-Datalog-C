analysis:
	eval $(opam config env) && cd ControlFlowAnalysis && dune build cli.exe

all: 
	analysis

clean:
	rm -rf ./_temp
	cd ControlFlowAnalysis && rm -rf ./_build