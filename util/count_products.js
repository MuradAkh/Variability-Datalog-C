const fs = require('fs');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const regExp = /definedEx\(([^()]+)\)/g;

function do_eval(){
    const all_vars =  new Set()


    const files = fs.readFileSync('./util/busybox_files.txt', 'utf-8').split('\n');
    for (const file of files) {
        const ast = fs.readFileSync(`./test/cfgs/busybox/${file}`, 'utf-8');
        try{
            (ast).match(regExp).forEach(v => all_vars.add(v))
        }catch(e){}               
    }

    console.log(all_vars)
    console.log(all_vars.size)

}

do_eval()