const fs = require('fs'); 
const regex = /\".*?\"/g;

fs.writeFileSync(
    '_temp/output.cfg', 
    fs.readFileSync('_temp/output.cfg', 'utf-8')
    .replace(/\\\"/g, "")
    .replace(/%/g, "_") 
    .replace(regex, "\"\"")
    .replace(/\'.*?\'/g, "\'A\'") 
); 


fs.writeFileSync(
    '_temp/output.cfg.ast', 
     fs.readFileSync('_temp/output.cfg.ast', 'utf-8')
    .replace(/\\\"/g, "")
    .replace(/\\\'/g, "")
    .replace(/%/g, "_") 
    .replace(regex, "\"\"")
    .replace(/\'.\'/g, "\'A\'") 
); 

