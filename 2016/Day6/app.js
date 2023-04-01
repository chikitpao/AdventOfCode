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

class Alphabet{
    constructor(name){
        this.name = name;
        this.count = 0
    }
}
class Alphabets{
    constructor(){
        this.data = new Array(26);
        let startOrd = 'a'.charCodeAt(0);
        for(let ord = startOrd, i = 0; ord <= 'z'.charCodeAt(0); ++ord, ++i) {
            this.data[i] = new Alphabet(String.fromCharCode(ord));
        }
    }
}
Alphabets.prototype.addCount = function (alpha) {
    this.data[alpha.charCodeAt(0) - 'a'.charCodeAt(0)].count += 1;
};
Alphabets.prototype.getMaxCount = function () {
    let result = undefined;
    for(let ord = 'a'.charCodeAt(0), i = 0; ord <= 'z'.charCodeAt(0); ++ord, ++i) {
        if(!result || (result.count < this.data[i].count))
            result = this.data[i];
    }
    return result;
};
Alphabets.prototype.getMinCount = function () {
    let result = undefined;
    for(let ord = 'a'.charCodeAt(0), i = 0; ord <= 'z'.charCodeAt(0); ++ord, ++i) {
        if(!result || (result.count > this.data[i].count))
            result = this.data[i];
    }
    return result;
};

let rowArray;

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    let columnCount = 0;
    for(let line of inputLines) {
        if(!line)
            continue;
        if(!columnCount){
            columnCount = line.length;
            rowArray = new Array(columnCount);
            for(let i = 0; i < columnCount; ++i)
                rowArray[i] = new Alphabets();
        }
        else if(line.length != columnCount)
            throw "Inconsistent column count!"

        for(let i = 0; i < columnCount; ++i)
            rowArray[i].addCount(line[i]);
    }
    let result1 = '';
    let result2 = '';
    for(let i = 0; i < columnCount; ++i){
        result1 += rowArray[i].getMaxCount().name;
        result2 += rowArray[i].getMinCount().name;
    }
    
    output('Day 6', isBrowser);
    output('Question 1: Given the recording in your puzzle input, what is the error-corrected version of the message being sent?', isBrowser);
    output('Answer: ' + result1, isBrowser);
    output('Question 2: Given the recording in your puzzle input and this new decoding methodology, what is the original message that Santa is trying to send?', isBrowser);
    output('Answer: ' + result2, isBrowser);
}
