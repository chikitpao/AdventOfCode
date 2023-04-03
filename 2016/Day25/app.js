"use strict"

///// Author: Chi-Kit Pao

import * as ab from '../util/assembunny.mjs';

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

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);

    let interpreter = new ab.AssemBunny();
    let firstAnswer = null;

    for(let a = 0; ; ++a){
        interpreter.reset()
        interpreter.check = [0, 1, 0, 1]; // => 4 for my input
        interpreter.check = [0, 1, 0, 1, 0, 1, 0, 1]; // => 180 for my input
        interpreter.registers['a'] = a;
        interpreter.run(inputLines);
        if(interpreter.check.length == interpreter.output.length
            && interpreter.check.every((element, index) => element == interpreter.output[index])){
            firstAnswer = a;
            break;
        }
    }
    
    output('Day 25', isBrowser);
    output('Question 1: What is the lowest positive integer that can be used to initialize register a and cause the code to output a clock signal of 0, 1, 0, 1... repeating forever?', isBrowser);
    output('Answer: ' + firstAnswer, isBrowser);
}
