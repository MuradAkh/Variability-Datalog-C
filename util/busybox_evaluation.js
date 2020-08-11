const fs = require('fs');
const {performance} = require('perf_hooks');
const util = require('util');
const exec = util.promisify(require('child_process').exec);
const regExpCfg = /definedEx\(([^()]+)\)/g;
const regExpAst = /def\(([^()]+)\)/g;
let total_size = 0

function products(file) {
    const all_vars = new Set()

    const regExp =  regExpCfg
    const ast = fs.readFileSync(`/home/murad/typechefdump/busybox/vars/${file}`, 'utf-8');
    try {
        (ast)
            .match(regExp)
            .map(r => r.split("(")[1].split(")")[0] )
            .forEach(v => all_vars.add(v))
    } catch (e) { }

    return all_vars.size
}

async function do_file(file, type){
    const t0 = performance.now()
    await exec(`sh save_facts_cfg.sh ${process.argv[2]}  /home/murad/typechefdump/busybox/${type}/${file}`);
    const t1 = performance.now()
    await exec(`rm -rf ./SouffleRunner/_temp/`);
    await exec(`mkdir -p ./SouffleRunner/_temp/`);
    await exec(`cp ./dump/${file}.facts ./SouffleRunner/_temp/target.facts`);
    await exec(`cat ./SouffleRunner/${process.argv[2].toLowerCase()}.souffle ./SouffleRunner/_temp/target.facts > ./SouffleRunner/_temp/target.souffle`);
    const u0 = performance.now()
    await exec(`souffle ./SouffleRunner/_temp/target.souffle`);
    const u1 = performance.now()
    const stats = fs.statSync(`./dump/${file}.facts`)
    const fileSizeInBytes = stats["size"]
    total_size += fileSizeInBytes
    
    await exec(`mkdir -p ./dump/souffle/`);
    let outputInBytes = 0;
    switch(process.argv[2].toLowerCase()){
        case "pointer": 
            await exec(`mv ./pointsTo.csv ./dump/souffle/${file}_pointsto.out`);  
            outputInBytes += fs.statSync(`./dump/souffle/${file}_pointsto.out`)["size"]
            break;
        case "reduceable":
            await exec(`mv ./broken.csv ./dump/souffle/${file}_broken.out`);  
            await exec(`mv ./cycle.csv ./dump/souffle/${file}_cycle.out`);            
            outputInBytes += fs.statSync(`./dump/souffle/${file}_broken.out`)["size"]
            outputInBytes += fs.statSync(`./dump/souffle/${file}_cycle.out`)["size"]
            break;
        case "defuse":
            await exec(`mv ./defUse.csv ./dump/souffle/${file}_defuse.out`);  
            outputInBytes += fs.statSync(`./dump/souffle/${file}_defuse.out`)["size"]
            break;
    }

    // console.log(Math.round(t1 - t0), "ms", Math.round(u1 - u0), "ms", fileSizeInBytes, "bytes")
    return [Math.round(t1 - t0), Math.round(u1 - u0), fileSizeInBytes, outputInBytes]
    
}



async function do_eval(){
    let time_extraction = []
    let time_solver = []
    let data_size = []
    let output_size = []

    const files = fs.readFileSync('./util/busybox_files.txt', 'utf-8').split('\n');
    for (const file of files) {
        try{
            let log = [file, products(file)]
            for (type of ["vars", "all_undefined", "all_defined"]){
                log = log.concat(await do_file(file, type))
            }         
            console.log(log.join(','))
            
            time_extraction = time_extraction.concat([
            (log[1]),
            (log[2]),
            ((Math.pow(2, log[1]) * (log[6] + log[10]) / 2))
            ].join(" "))

            time_solver = time_solver.concat([
            (log[1]),
            (log[3]),
            ((Math.pow(2, log[1]) * (log[7] + log[11]) / 2))
            ].join(" "))
            
            data_size = data_size.concat([
            (log[1]),
            (log[4]),
            ((Math.pow(2, log[1]) * (log[8] + log[12]) / 2))
            ].join(" "))

            output_size = output_size.concat([
            (log[1]),
            (log[5]),
            ((Math.pow(2, log[1]) * (log[9] + log[13]) / 2))
            ].join(" "))

            
        }catch(err){
            // if(process.argv[4] === "csv"){
            //     console.log([file, "f",  "f", "f", "f"].join(","))
            // }else{
            //     console.log(`failed: ${file}`)
            // }
            // console.log(err)
            // console.log(`failed: ${file}`)
        }
        await exec("mkdir -p data")
        fs.writeFileSync("./data/extract-" + process.argv[2] + ".csv", time_extraction.join("\n"))
        fs.writeFileSync("./data/solve-" + process.argv[2]  + ".csv" , time_solver.join("\n"))
        fs.writeFileSync("./data/size-" + process.argv[2] + ".csv" , data_size.join("\n"))
        fs.writeFileSync("./data/outputsize-" + process.argv[2] + ".csv" , output_size.join("\n"))
        
    }


    return total_size
}

const t0 = performance.now()
do_eval().then(
    () => {
        console.log("~~ done ~~")
        const t1 = performance.now()
        const milliseconds = Math.round(t1 - t0)
        const minute = Math.floor((milliseconds % (1000 * 60 * 60)) / (1000 * 60));
        const second = Math.floor((milliseconds % (1000 * 60)) / 1000);
        console.log("total time", milliseconds , "millisecond(s)")
        console.log("or aprox", minute , "minute(s)", second, "second(s)")
        console.log("total size", total_size , "bytes")
        console.log("or aprox", Math.floor(total_size / (1024 * 1024)), "mb", Math.floor(total_size % (1024 * 1024) / 1024), "kb" )



    }
)
