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

function factorial(n){
    if(n <= 1)
        return 1;
    return n * factorial(n-1);
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);

    let myInputConstant = 85 * 92; // Constant from my input file
    let interpreter = new ab.AssemBunny();
    interpreter.registers['a'] = 7;
    interpreter.run(inputLines);
    let firstAnswer =  interpreter.registers['a'];
    console.log(`First Answer = factorial(7) + myInputConstant = ${factorial(7) + myInputConstant}`);
    let secondAnswer = factorial(12) + myInputConstant;
    
    output('Day 23', isBrowser);
    output('Question 1: After executing the assembunny code in your puzzle input, what value is left in register a?', isBrowser);
    output('Answer: ' + firstAnswer, isBrowser);
    output('Question 2: What value should actually be sent to the safe?', isBrowser);
    output('Answer: ' + secondAnswer, isBrowser);
}
