rm -rf _temp
mkdir _temp
cp $2 ./_temp/target.c
touch _temp/output.cfg.ast
cd TypechefRunner && sbt run && cd -
node util/process_strings.js 
echo "--FACTS--"
./ControlFlowAnalysis/_build/default/cli.exe $1
