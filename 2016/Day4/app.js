"use strict"

///// Author: Chi-Kit Pao

const inputUrl = 'input.txt'
let isBrowser = true;
if (typeof window === 'undefined') {
    // Node.js
    isBrowser = false;
    getNodeJsInput(inputUrl).then(value => processInput(value, isBrowser));
} else {
    // Web API and Node.js
    fetch(inputUrl).then(response => {
        response.text().then(value => processInput(value, isBrowser))
    });
}

async function getNodeJsInput(url) {
    const fs = await import('fs');
    return fs.readFileSync(url).toString();    
}

function output(string, isBrowser) {
    console.log(string);
    if(isBrowser) {
        let bodys = document.getElementsByTagName('body');
        let p = document.createElement('p');
        p.textContent = string;
        bodys[0].appendChild(p);
    }
}

function isValidRoom(code, checkSum){
    // Array for character counts since we also want to sort them and
    // we must also consider count = 0.
    let characterCounts = [];
    // String.fromCharCode(
    let ordStart = 'a'.charCodeAt(0);
    let ordEnd = 'z'.charCodeAt(0);
    for(let ord = ordStart; ord <= ordEnd; ++ord){
        characterCounts.push({char: String.fromCharCode(ord), count: 0});
    }
    for(let char of code){
        if(char == '-')
            continue;
        let index = char.charCodeAt(0) - ordStart;
        try{
            characterCounts[index].count += 1;
        } catch (error){
            console.log(error)
        }
    }
    characterCounts.sort((a, b) => {
        if (a.count == b.count)
            return b.char - a.char;
        return b.count - a.count;
    });
    
    let testValue = '';
    for(let i = 0; i < 5; ++i)
        testValue += characterCounts[i].char;
    
    return testValue == checkSum;
}

function decrypt(text, key){
    let name = '';
    const ordStart = 'a'.charCodeAt(0);
    for(let char of text) {
        if(char == '-') {
            name += ' ';
        } else {
            const charCode = char.charCodeAt(0);
            const newChar = String.fromCodePoint(((charCode - ordStart) + key) % 26 + ordStart);
            name += newChar;
        }
    }
    return name;
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);

     // Question 1
     let sectorIds = [];
     let wantedSectorId = undefined;
     for(let inputLine of inputLines){
        // Example input: gbc-frperg-pubpbyngr-znantrzrag-377[rgbnp] 
        let re = /([-a-z]+)(\d+)\[([a-z]+)\]/;
        let group = inputLine.match(re);
        if(!group) // null
            continue;
        // group[0] is the whole string, group[1] is the first group, etc.
        let sectorId = parseInt(group[2]);
        if(isValidRoom(group[1], group[3]))
        {
            sectorIds.push(sectorId);
            let roomName = decrypt(group[1], sectorId);
            if(roomName.indexOf('north') != -1)
                console.log(roomName);
            if(roomName.trim().localeCompare('northpole object storage') == 0)
                wantedSectorId = sectorId;
        }
     }
    
    output('Day 4', isBrowser);
    output('Question 1: What is the sum of the sector IDs of the real rooms?', isBrowser);
    output('Answer: ' + sectorIds.reduce((a, b) => a + b), isBrowser);
    output('Question 2: What is the sector ID of the room where North Pole objects are stored?', isBrowser);
    output('Answer: ' + ((typeof wantedSectorId === 'undefined') ? 'n/a': wantedSectorId), isBrowser);
}
