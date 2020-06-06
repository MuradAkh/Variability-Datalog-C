const fs = require('fs'); 
const file = fs.readFileSync('_temp/output.cfg', 'utf-8'); 
const regex = /\".*?\"/g;

fs.writeFileSync(
    '_temp/output.cfg', 
    file
    .replace(/\\\"/g, "")
    .replace(regex, "\"\"")
    .replace(/%/g, "_") 
    .replace(/\'.*?\'/g, "\'A\'") 
); 

