rm -rf _temp
mkdir _temp
cp $2 ./_temp/output.cfg
echo "--FACTS--"
node util/process_strings.js 
./ControlFlowAnalysis/_build/default/cli.exe $1
