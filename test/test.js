const util = require('util');
const assert = require('assert');
const fs = require('fs');
const exec = util.promisify(require('child_process').exec);
// const { basicTest, cliTest } = require('./TestUtils.js')

const makeTest = (extension, shellfile) => {
  return async name => {
    const {stderr, stdout} = await exec(`sh ${shellfile} POINTER  ./test/regression/target/${name}.${extension}`)
    const expected = fs.readFileSync(`./test/regression/expected/${name}.${extension}.facts`, 'utf-8').split("\n"); 
    const actual = fs.readFileSync(`./dump/${name}.${extension}.facts`, 'utf-8').split("\n"); 
    const current = new Set(actual)
    const difference = new Set(expected.filter(x => !current.has(x)))
    // const {stderr1, stdout1} = await exec(`rm ./dump/${name}.${extension}.facts`)
    assert.equal(difference.size, 0)
  }
}

const testC = makeTest('c', './save_facts.sh')

const testCfg =  makeTest('cfg', './save_facts_cfg.sh')


describe('Basic Functionality', function () {
  this.timeout(90000);

  before(async () => {
    // this.test = basicTest(["countCFG"])
    // await exec(`make countCFG`)
  })

  it('empty', async () => testCfg('empty'));
  it('loops', async () => testCfg('loops'));
  it('badstrings', async () => testC('badstrings'));
})

describe('Pointer Analysis', function () {

    before(async () => {
      // this.test = basicTest(["countCFG"])
      // await exec(`make countCFG`)
    })
  
    it('malloc', async () => testCfg('malloc'));
    it('assigns', async () => testCfg('malloc'));
    it('functions', async () => testCfg('malloc'));
})

describe('Dominator', () => {
  before(async () => {
    // this.test = basicTest(["countCFG"])
    // await exec(`make countCFG`)
  })

  it('dummy', async () => {});
})
