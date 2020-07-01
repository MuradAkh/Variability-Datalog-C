analysis:
	eval $(opam config env) && cd ControlFlowAnalysis && dune build cli.exe

typechef:
	cd TypechefRunner && sbt compile

all: 
	analysis
	typechef

clean:
	rm -rf ./_temp
	cd ControlFlowAnalysis && rm -rf ./_build

test:
	npm test 