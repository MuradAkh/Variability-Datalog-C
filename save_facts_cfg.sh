rm -rf _temp
mkdir _temp
mkdir dump
cp $2 ./_temp/output.cfg
echo "--FACTS--"
node util/process_strings.js 
name="$(basename $2)"
./ControlFlowAnalysis/_build/default/cli.exe $1 > dump/$name.facts
