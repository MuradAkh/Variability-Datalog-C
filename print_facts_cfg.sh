rm -rf _temp
mkdir _temp
touch _temp/output.ast
cp "$2" ./_temp/output.cfg
cp "$2".ast ./_temp/output.cfg.ast
echo "--FACTS--"
node util/process_strings.js 
./ControlFlowAnalysis/_build/default/cli.exe "$1"
