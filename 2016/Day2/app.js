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

function getNextButton(x, y, inputLine) {
    let newX = x, newY = y;
    for(let ch of inputLine){
        switch(ch){
            case 'U':
                if(newY > -1)
                    newY--;
                break;
            case 'R':
                if(newX < 1)
                    newX++;
                break;
            case 'D':
                if(newY < 1)
                newY++;
                break;
             case 'L':
                if(newX > -1)
                    newX--;
                break;
            default:
                throw 'Unknown input: ' + ch;
        }
    }

    return {x: newX, y: newY};
}

function getNextButtonV2(x, y, inputLine) {
    let newX = x, newY = y;
    function isValidButton(nextX, nextY){
        return (Math.abs(nextX) + Math.abs(nextY) <= 2)
    }
    for(let ch of inputLine){
        switch(ch){
            case 'U':
                if(isValidButton(newX, newY - 1))
                    newY--;
                break;
            case 'R':
                if(isValidButton(newX + 1, newY))
                    newX++;
                break;
            case 'D':
                if(isValidButton(newX, newY + 1))
                newY++;
                break;
             case 'L':
                if(isValidButton(newX - 1, newY))
                    newX--;
                break;
            default:
                throw 'Unknown input: ' + ch;
        }
    }

    return {x: newX, y: newY};
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);

    // Question 1
    let x = 0, y = 0;
    let outputString = '';
    for(let inputLine of inputLines) {
        if(!inputLine.length)
            continue;
        ({x, y} = getNextButton(x, y, inputLine));
        outputString += (3 * y + x + 5);
    }
    // Question 2
    x = 0, y = 0;
    const buttonsV2 = 'XX1XXX234X56789XABCXXXDXX';
    let outputString2 = '';
    for(let inputLine of inputLines) {
        if(!inputLine.length)
            continue;
        ({x, y} = getNextButtonV2(x, y, inputLine));
        outputString2 += buttonsV2[5 * (y + 2) + (x + 2)];
    }
    
    output('Day 2', isBrowser);
    output('Question 1: What is the bathroom code?', isBrowser);
    output('Answer: ' + outputString, isBrowser);
    output('Question 2: Using the same instructions in your puzzle input, what is the correct bathroom code?', isBrowser);
    output('Answer: ' + outputString2, isBrowser);
}