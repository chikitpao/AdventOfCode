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

function output(string, isBrowser, isPreformatted) {
    console.log(string);
    if(isBrowser) {
        let bodys = document.getElementsByTagName('body');
        let p = (isPreformatted) ? document.createElement('pre') : document.createElement('p')
        p.textContent = string;
        bodys[0].appendChild(p);
    }
}

class MyScreen{
    constructor(width, height) {
        this.width = width;
        this.height = height;
        this.data = new Array(height);
        for(let i = 0; i < height; ++i){
            this.data[i] = new Array(width);
            this.data[i].fill(0);
        }
    }
}
MyScreen.prototype.createRect = function (width, height){
    for(let i = 0; i < height; ++i){
        for(let j = 0; j < width; ++j){
            this.data[i][j] = 1;
        }
    }
}
MyScreen.prototype.rotateColumn = function (column, count){
    let newValues = new Array(this.height);
    for(let i = 0; i < this.height; ++i)
         newValues[i] = this.data[i][column];
    for(let i = 0; i < count; ++i)
        newValues.unshift(newValues.pop());
    for(let i = 0; i < this.height; ++i)
        this.data[i][column] = newValues[i];
}
MyScreen.prototype.rotateRow = function (row, count){
    for(let i = 0; i < count; ++i)
        this.data[row].unshift(this.data[row].pop());
}
MyScreen.prototype.getCount = function (){
    let count = 0;
    this.data.forEach((value) => count += value.reduce((a, b) => a + b));
    return count;
}
MyScreen.prototype.print = function (isBrowser){
    this.data.forEach((value) => output(value.map((v) => (v ? '#' : ' ')).join(''), isBrowser, isBrowser));
}

function processInput(input, isBrowser){
    let screen = new MyScreen(50, 6);
    
    const inputLines = input.split(/\r?\n/);
    for(let inputLine of inputLines) {
        if(!inputLine)
            continue;

        // Line examples:
        // rect 1x1
        // rotate row y=0 by 5
        // rotate column x=0 by 1
        if(inputLine.startsWith('rect')){
            let result = inputLine.match(/rect (\d+)x(\d+)/);
            screen.createRect(parseInt(result[1]), parseInt(result[2]));
        } else if(inputLine.startsWith('rotate row')) {
            let result = inputLine.match(/rotate row y=(\d+) by (\d+)/);
            screen.rotateRow(parseInt(result[1]), parseInt(result[2]));
        } else if(inputLine.startsWith('rotate column')) {
            let result = inputLine.match(/rotate column x=(\d+) by (\d+)/);
            screen.rotateColumn(parseInt(result[1]), parseInt(result[2]));
        } else
            throw "Unknown line pattern found!" + inputLine;
    }
    
    output('Day 8', isBrowser);
    output('Question 1: There seems to be an intermediate check of the voltage used by the display: after you swipe your card, if the screen did work, how many pixels should be lit?', isBrowser);
    output('Answer: ' + screen.getCount(), isBrowser);
    output('Question 2: After you swipe your card, what code is the screen trying to display?', isBrowser);
    output('Answer: ', isBrowser);
    screen.print(isBrowser);
}
