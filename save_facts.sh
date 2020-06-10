rm -rf _temp
mkdir _temp
mkdir dump
rm output.facts
cp $2 ./_temp/target.c
cd TypechefRunner && sbt run && cd -
node util/process_strings.js 
name= $(basename $2)
./ControlFlowAnalysis/_build/default/cli.exe $1 > ./dump/$name.facts
