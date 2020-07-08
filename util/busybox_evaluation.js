const fs = require('fs');
const {performance} = require('perf_hooks');
const util = require('util');
const exec = util.promisify(require('child_process').exec);


async function do_eval(){

    const files = fs.readFileSync('./util/busybox_files.txt', 'utf-8').split('\n');
    for (const file of files) {
        try{
            const t0 = performance.now()
            const out1 = await exec(`sh save_facts_cfg.sh ${process.argv[2]}  ./test/cfgs/busybox/${file}`);
            await exec(`rm -rf ./SouffleRunner/_temp/`);
            await exec(`mkdir ./SouffleRunner/_temp/`);
            await exec(`cp ./dump/${file}.facts ./SouffleRunner/_temp/target.facts`);
            await exec(`cat ./SouffleRunner/${process.argv[2].toLowerCase()}.souffle ./SouffleRunner/_temp/target.facts > ./SouffleRunner/_temp/target.souffle`);
            const out2 = await exec(`(time souffle ./SouffleRunner/_temp/target.souffle) 2>&1 | cat `);
            const t1 = performance.now()
            console.log(file, Math.round(t1 - t0), "ms")
            await exec(`mkdir -p ./dump/souffle/`);
            await exec(`mv ./pointsTo.csv ./dump/souffle/${file}.out`);     
        }catch(err){
            console.log(`failed: ${file}`)
        }
        
    }
}

const t0 = performance.now()
do_eval().then(
    () => {
        console.log("~~ done ~~")
        const t1 = performance.now()
        const milliseconds = Math.round(t1 - t0)
        const minute = Math.floor((milliseconds % (1000 * 60 * 60)) / (1000 * 60));
        const second = Math.floor((milliseconds % (1000 * 60)) / 1000);
        console.log("total", milliseconds , "millisecond(s)")
        console.log("or aprox", minute , "minute(s)", second, "second(s)")

    }
)
