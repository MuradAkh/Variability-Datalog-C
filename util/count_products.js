const fs = require('fs');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const regExpCfg = /definedEx\(([^()]+)\)/g;
const regExpAst = /def\(([^()]+)\)/g;

function do_eval() {
    const all_vars = new Set()

    function do_file(file) {
        const regExp = file.includes('.ast') ? regExpAst : regExpCfg
        const ast = fs.readFileSync(`/home/murad/typechefdump/busybox/vars/${file}`, 'utf-8');
        try {
            (ast)
                .match(regExp)
                .map(r => r.split("(")[1].split(")")[0] )
                .forEach(v => all_vars.add(v))
        } catch (e) { }
    }

    if (process.argv[2]) {
        do_file(process.argv[2])
    } else {
        const files = fs.readFileSync('./util/busybox_files.txt', 'utf-8').split('\n');
        for (const file of files) {
            do_file(file + '.ast');
        }
    }

    // console.log(all_vars)
    // console.log(all_vars.size)
    Array.from(all_vars).forEach(v => {
        console.log("#undef " + v)
    })

}

do_eval()