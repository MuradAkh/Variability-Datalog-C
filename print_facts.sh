rm -rf _temp
mkdir _temp
cp $2 ./_temp/target.c
cd TypechefRunner && sbt run && cd -
echo "--FACTS--"
./ControlFlowAnalysis/_build/default/cli.exe $1
