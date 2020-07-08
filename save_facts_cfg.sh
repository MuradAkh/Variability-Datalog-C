rm -rf _temp
mkdir _temp
mkdir dump
cp $2 ./_temp/output.cfg
if [ -e "$2.ast" ] 
    then cp "$2.ast" ./_temp/output.cfg.ast
fi
touch _temp/output.cfg.ast
echo "--FACTS--"
node util/process_strings.js 
name="$(basename $2)"
./ControlFlowAnalysis/_build/default/cli.exe $1 > dump/$name.facts
