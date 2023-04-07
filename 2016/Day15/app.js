"use strict"

///// Author: Chi-Kit Pao

const inputUrl = 'input.txt'
let isBrowser = true;
if (typeof window === 'undefined') {
    // Node.js
    isBrowser = false;
    getNodeJsInput(inputUrl).then(value => processInput(value, isBrowser));
} else {
    // Web API
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

class Disc{
    constructor(disc, positionCount, starttime, startpos){
        this.number = disc;
        this.positionCount = positionCount;
        this.pos0 = Math.abs((starttime < 0) ? (startpos % positionCount + positionCount) : (startpos % positionCount))
    }
};

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    let discs = new Array();
    for(let inputLine of inputLines){
        if(!inputLine)
            continue;
        // Example line: 'Disc #1 has 13 positions; at time=0, it is at position 1.'
        let found = inputLine.match(/Disc #(?<disc>\d+) has (?<pos>\d+) positions; at time=(?<starttime>\d+), it is at position (?<startpos>\d+)./);
        discs.push(new Disc(parseInt(found.groups.disc), parseInt(found.groups.pos), parseInt(found.groups.starttime), parseInt(found.groups.startpos)));
    }
    // Searching for (number + pos0 + duration) mod positionCount = 0 for all discs.
    // We can find LCM for positionCounts, or just multiply them together (which will be at worst multiples of LCM).
    let commonMulitple = discs.reduce(((a, c) => (a * c.positionCount)), 1);
    let answer1;
    for(let n = 0; n < commonMulitple; ++n){
        if(discs.every(disc => (((disc.number + disc.pos0 + n) % disc.positionCount)==0))){
            answer1 = n;
            break;
        }
    }

    // Answer 2
    discs.push(new Disc(discs.length + 1, 11, 1, 0)); // disc index is one-based!
    commonMulitple *= 11;
    let answer2;
    for(let n = 0; n < commonMulitple; ++n){
        if(discs.every(disc => (((disc.number + disc.pos0 + n) % disc.positionCount)==0))){
            answer2 = n;
            break;
        }
    }

    output('Day 15', isBrowser);
    output('Question 1: What is the first time you can press the button to get a capsule?', isBrowser);
    output('Answer: ' + answer1, isBrowser);
    output('Question 2: With this new disc, and counting again starting from time=0 with the configuration in your puzzle input, what is the first time you can press the button to get another capsule?', isBrowser);
    output('Answer: ' + answer2, isBrowser);
}
