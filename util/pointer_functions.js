const util = require('util');
const fs = require('fs');
const exec = util.promisify(require('child_process').exec);

function process_line(line){
    [names, target] = line.split(process.argv[2]);
    if(!names.includes(" function ")){
        return ""
    }
    const f_name = names.split(" ")[0]

    if(target.includes("*" + f_name)){
        return f_name
    }else{
        return ""
    }
}

function process_f(output){
    return output
            .split("\n")
            .map(process_line)
            .filter(name => name.length > 0)
            .map(fact => `returns_pointer("${fact}").`)
            .join("\n")
    

}


async function gen_facts(){
    const {stdout, stderr} = await exec(`ctags -x ${process.argv[2]}`)
    console.log(stderr)
    const facts = process_f(stdout)
    console.log(facts)

    fs.writeFileSync(
        `dump/${process.argv[2].replace(/\//g, "_")}.functions`, 
       facts
    ); 

}

gen_facts()
    .then(() => console.log(""))
    .catch((err) => console.error(err))




