const fs = require('fs');
const {performance} = require('perf_hooks');
const util = require('util');
const exec = util.promisify(require('child_process').exec);


async function do_eval(){
    const files = fs.readFileSync('./util/busybox_files.txt', 'utf-8').split('\n');
    let total_size = 0
    for (const file of files) {
        try{
            const t0 = performance.now()
            const out1 = await exec(`sh save_facts_cfg.sh ${process.argv[2]}  /home/murad/typechefdump/busybox/${process.argv[3].toLowerCase()}/${file}`);
            const t1 = performance.now()
            await exec(`rm -rf ./SouffleRunner/_temp/`);
            await exec(`mkdir -p ./SouffleRunner/_temp/`);
            await exec(`cp ./dump/${file}.facts ./SouffleRunner/_temp/target.facts`);
            await exec(`cat ./SouffleRunner/${process.argv[2].toLowerCase()}.souffle ./SouffleRunner/_temp/target.facts > ./SouffleRunner/_temp/target.souffle`);
            const u0 = performance.now()
            const out2 = await exec(`souffle ./SouffleRunner/_temp/target.souffle`);
            const u1 = performance.now()
            const stats = fs.statSync(`./dump/${file}.facts`)
            const fileSizeInBytes = stats["size"]
            total_size += fileSizeInBytes
            console.log(file, Math.round(t1 - t0), "ms", Math.round(u1 - u0), "ms", fileSizeInBytes, "bytes")
            await exec(`mkdir -p ./dump/souffle/`);
            await exec(`mv ./pointsTo.csv ./dump/souffle/${file}.out`);     
        }catch(err){
            console.log(err)
            console.log(`failed: ${file}`)
        }
        
    }

    return total_size
}

const t0 = performance.now()
do_eval().then(
    (bytes) => {
        console.log("~~ done ~~")
        const t1 = performance.now()
        const milliseconds = Math.round(t1 - t0)
        const minute = Math.floor((milliseconds % (1000 * 60 * 60)) / (1000 * 60));
        const second = Math.floor((milliseconds % (1000 * 60)) / 1000);
        console.log("total time", milliseconds , "millisecond(s)")
        console.log("or aprox", minute , "minute(s)", second, "second(s)")
        console.log("total size", bytes , "bytes")
        console.log("or aprox", Math.floor(bytes / (1024 * 1024)), "mb", Math.floor(bytes % (1024 * 1024) / 1024), "kb" )



    }
)
