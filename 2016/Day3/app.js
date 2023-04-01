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

function isValidTriangle(numbers){
    return (numbers[0] < (numbers[1] + numbers[2])) 
        && (numbers[1] < (numbers[0] + numbers[2])) 
        && (numbers[2] < (numbers[0] + numbers[1]));
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    
    // Question 1
    let validTriangles = 0
    for(let inputLine of inputLines){
        let re = /\d+/g;
        let group = inputLine.match(re);
        if(!group) // null
            continue;
        if(isValidTriangle(group.map(str => parseInt(str))))
            validTriangles++;   
    }

    // Question 2
    let validTrianglesV2 = 0;
    let index = 0
    while(index < inputLines.length) {
        if(!inputLines[index])
            break;

        let re = /\d+/g;
        let numbers1 = inputLines[index++].match(re).map(str => parseInt(str));
        let numbers2 = inputLines[index++].match(re).map(str => parseInt(str));
        let numbers3 = inputLines[index++].match(re).map(str => parseInt(str));
        for(let i = 0; i < 3; ++i) {
            if(isValidTriangle([numbers1[i], numbers2[i], numbers3[i]]))
                validTrianglesV2++;
        }

    }
    
    output('Day 3', isBrowser);
    output('Question 1: In your puzzle input, how many of the listed triangles are possible?', isBrowser);
    output('Answer: ' + validTriangles, isBrowser);
    output('Question 2: In your puzzle input, and instead reading by columns, how many of the listed triangles are possible?', isBrowser);
    output('Answer: ' + validTrianglesV2, isBrowser);
}
