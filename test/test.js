const util = require('util');
const assert = require('assert');
const fs = require('fs');

const exec = util.promisify(require('child_process').exec);
// const { basicTest, cliTest } = require('./TestUtils.js')

const testC = async name => {
  const {stderr, stdout} = await exec(`sh ./save_facts.sh POINTER  ./test/regression/target/${name}.c`)
  const expected = fs.readFileSync(`./test/regression/expected/${name}.c.facts`, 'utf-8').split("\n"); 
  const current = new Set(stdout.split("\n"))
  const difference = new Set(expected.filter(x => !current.has(x)))
  assert.equal(difference.size, 0)
}

const testCfg =  async name => {

}


describe('Basic Functionality', function () {
  this.timeout(90000);

  before(async () => {
    // this.test = basicTest(["countCFG"])
    // await exec(`make countCFG`)
  })

  it('empty', async () => testC('empty'));
  it('loops', async () => testC('loops'));
  it('badstrings', async () => testC('badstrings'));
})

describe('Pointer Analysis', function () {
    this.timeout(90000);

    before(async () => {
      // this.test = basicTest(["countCFG"])
      // await exec(`make countCFG`)
    })
  
    it('malloc', async () => testC('malloc'));
    it('assigns', async () => testC('malloc'));
    it('functions', async () => testC('malloc'));
})

describe('Dominator', () => {
  before(async () => {
    // this.test = basicTest(["countCFG"])
    // await exec(`make countCFG`)
  })

  it('dummy', async () => {});
})
