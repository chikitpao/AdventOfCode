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

function swapPosition(passwordArray, pos1, pos2){
    [passwordArray[pos1], passwordArray[pos2]] = [passwordArray[pos2], passwordArray[pos1]];
}

function rotate(passwordArray, rotateLeft, steps){
    if (rotateLeft) {
        for (let i = 0; i < steps; ++i)
            passwordArray.push(passwordArray.shift());
    } else {
        for (let i = 0; i < steps; ++i)
            passwordArray.unshift(passwordArray.pop());
    }
}

function reverse(passwordArray, startPos, endPos){
    let steps = Math.floor((endPos - startPos + 1) / 2);
    for (let i = 0; i < steps; ++i)
        [passwordArray[startPos + i], passwordArray[endPos - i]] = [passwordArray[endPos - i], passwordArray[startPos + i]];
}

function scramble(inputLines, startPassword){
    let passwordArray = Array.from(startPassword);

    for(let inputLine of inputLines){
        if(!inputLine)
            continue;
        let found = inputLine.match(/swap position (\d+) with position (\d+)/);
        if(found){
            swapPosition(passwordArray, parseInt(found[1]), parseInt(found[2]));
            continue;
        }
        found = inputLine.match(/swap letter ([a-z]) with letter ([a-z])/);
        if(found){
            let pos1 = passwordArray.indexOf(found[1]);
            let pos2 = passwordArray.indexOf(found[2]);
            swapPosition(passwordArray, pos1, pos2);
            continue;
        }
        found = inputLine.match(/rotate (left|right) (\d+) step/);
        if(found){
            rotate(passwordArray, (found[1] == 'left'), parseInt(found[2]));
            continue;
        }
        found = inputLine.match(/rotate based on position of letter ([a-z])/);
        if(found){
            let index = passwordArray.indexOf(found[1]);
            let steps = index + ((index >= 4) ? 2 : 1);
            rotate(passwordArray, false, steps);
            continue;
        }
        found = inputLine.match(/reverse positions (\d+) through (\d+)/);
        if(found){
            reverse(passwordArray, parseInt(found[1]), parseInt(found[2]));
            continue;
        }
        found = inputLine.match(/move position (\d+) to position (\d+)/);
        if(found){
            let temp = passwordArray.splice(parseInt(found[1]), 1);
            passwordArray.splice(parseInt(found[2]), 0, temp[0]);
            continue;
        }
        throw "Unknown input line: " + inputLine
    }
    return passwordArray.join('');
}

function unscramble(inputLines, scrambledPassword){
    let passwordArray = Array.from(scrambledPassword);
    for(let line = inputLines.length - 1; line >= 0; line--){
        let inputLine = inputLines[line];
        if(!inputLine)
            continue;
        let found = inputLine.match(/swap position (\d+) with position (\d+)/);
        if(found){
            // the same as in scramble
            swapPosition(passwordArray, parseInt(found[1]), parseInt(found[2]));
            continue;
        }
        found = inputLine.match(/swap letter ([a-z]) with letter ([a-z])/);
        if(found){
            // the same as in scramble
            let pos1 = passwordArray.indexOf(found[1]);
            let pos2 = passwordArray.indexOf(found[2]);
            swapPosition(passwordArray, pos1, pos2);
            continue;
        }
        found = inputLine.match(/rotate (left|right) (\d+) step/);
        if(found){
            // swap left and right
            rotate(passwordArray, (found[1] != 'left'), parseInt(found[2]));
            continue;
        }
        found = inputLine.match(/rotate based on position of letter ([a-z])/);
        if(found){
            // position before scramble -> position after scramble
            // 0 -> 1, 1 -> 3, 2 -> 5, 3 -> 7,
            // 4 -> 10 (=2), 5 -> 12 (=4), 6 -> 14 (=6), 7 -> 16 (=0) 
            // Unscramble means to reverse the action. Here are the steps to left-shift:
            let steps = [1, 1, 6, 2, 7, 3, 0, 4];
            let index = passwordArray.indexOf(found[1]);
            rotate(passwordArray, true, steps[index]);
            continue;
        }
        found = inputLine.match(/reverse positions (\d+) through (\d+)/);
        if(found){
            // the same as in scramble
            reverse(passwordArray, parseInt(found[1]), parseInt(found[2]));
            continue;
        }
        found = inputLine.match(/move position (\d+) to position (\d+)/);
        if(found){
            // swap positions
            let temp = passwordArray.splice(parseInt(found[2]), 1);
            passwordArray.splice(parseInt(found[1]), 0, temp[0]);
            continue;
        }
        throw "Unknown input line: " + inputLine
    }
    return passwordArray.join('');
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);

    let answer1 = scramble(inputLines, 'abcdefgh');
    let answer2 = unscramble(inputLines, 'fbgdceah');

    output('Day 21', isBrowser);
    output('Question 1: Given the list of scrambling operations in your puzzle input, what is the result of scrambling abcdefgh?', isBrowser);
    output('Answer: ' + answer1, isBrowser);
    output('Question 2: What is the un-scrambled version of the scrambled password fbgdceah?', isBrowser);
    output('Answer: ' + answer2, isBrowser);
}
