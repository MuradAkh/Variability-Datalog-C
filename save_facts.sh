rm -rf _temp
mkdir _temp
mkdir dump
touch _temp/output.cfg.ast
rm output.facts
cp $2 ./_temp/target.c
cd TypechefRunner && sbt run && cd ..
node util/process_strings.js 
name=$(basename $2)
./ControlFlowAnalysis/_build/default/cli.exe $1 > ./dump/$name.facts
