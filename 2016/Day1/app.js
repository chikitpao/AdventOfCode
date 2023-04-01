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

function processInput(input, isBrowser){
    let x = 0;
    let y = 0;
    let direction = 0; // clockwise, 0: north
    let firstDuplicate = undefined;
    let visited = new Set();
    visited.add('0,0');
    
    function addVisitedPlace(startX, startY, endX, endY) {
        if(firstDuplicate)
            return;
        if(startX == endX) {
            for(let y = startY; y <= endY; ++y) {
                let newPositionString = startX + ',' + y;
                if (visited.has(newPositionString)){
                    firstDuplicate = {x: startX, y : y};
                    return;
                }
                else
                    visited.add(newPositionString);
            }
        } else if(startY == endY) {
            for(let x = startX; x <= endX; ++x) {
                let newPositionString = x + ',' + startY;
                if (visited.has(newPositionString)){
                    firstDuplicate = {x: x, y: startY};
                    return;
                }
                else
                    visited.add(newPositionString);
            }
        } else
            throw "Invalid arguments";
    }
    
    let updateFunctions = [
        (steps) => { addVisitedPlace(x, y + 1, x, y + steps); y += steps; },
        (steps) => { addVisitedPlace(x + 1, y, x + steps, y); x += steps; },
        (steps) => { addVisitedPlace(x, y - steps, x, y - 1); y -= steps; },
        (steps) => { addVisitedPlace(x - steps, y, x - 1, y); x -= steps; }
    ];
    
    let substrings = input.split(',').map((substring) => substring.trim());
    for(let string of substrings) {
        if(string[0] == 'R'){
            direction = (direction + 1) % 4;
        } else if (string[0] == 'L') {
            direction -= 1;
            if(direction < 0)
                direction = 3;
        } else {
            throw "Unknown instruction: " + string;
        }
        updateFunctions[direction](parseInt(string.substring(1)));
    }
    
    output('Day 1', isBrowser);
    output('Question 1: How many blocks away is Easter Bunny HQ?', isBrowser);
    output('Answer: ' + Math.abs(x + y), isBrowser);
    output('Question 2: How many blocks away is the first location you visit twice?', isBrowser);
    output('Answer: ' + ((!firstDuplicate) ? 'n/a' : Math.abs(firstDuplicate.x + firstDuplicate.y)), isBrowser);
}
