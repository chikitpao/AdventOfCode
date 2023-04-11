"use strict"

///// Author: Chi-Kit Pao

import {CircularBuffer} from '../util/CircularBuffer.mjs';

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

function countRowSafeTiles(row){
    return (row.match(/\./g) || []).length;
}

function generateTile(left, center, right){
    if((left == '^' && center == '^' && right == '.')
    || (left == '.' && center == '^' && right == '^')
    || (left == '^' && center == '.' && right == '.')
    || (left == '.' && center == '.' && right == '^'))
        return '^';
    return '.';
}

function generateNextLine(row, tileCount)
{
    let buffer = new Array();
    for(let i = 0; i < tileCount; ++i){
        if(i == 0)
            buffer.push(generateTile('.', row[i], row[i+1]));
        else if(i == tileCount - 1)
            buffer.push(generateTile(row[i-1], row[i], '.'));
        else
            buffer.push(generateTile(row[i-1], row[i], row[i+1]));

    }
    return buffer.join('');
}

// Normal counting
function countTotalSafeTiles(row, rowCount){
    const tileCount = row.length;
    let count = 0;
    count += countRowSafeTiles(row);
    let newRow = row;
    for(let i = 0; i < rowCount - 1; ++i){
        newRow = generateNextLine(newRow, tileCount);
        count += countRowSafeTiles(newRow);
    }
    return count;
}

// Find cycle while counting
function countTotalSafeTiles2(row, rowCount){
    const tileCount = row.length;
    let history = new Array();
    let count = countRowSafeTiles(row);
    history.push([row, count, count]);
    let newRow = row;
    for(let i = 0; i < rowCount - 1; ++i){
        newRow = generateNextLine(newRow, tileCount);
        if(newRow == row){
            let cycleLength = history.length;
            let cycleCount = Math.floor(rowCount / cycleLength);
            let remainder = rowCount % cycleLength;
            let accumulatedCount = history[history.length-1][2];
            let result = accumulatedCount * cycleCount;
            if(remainder > 0)
                result += history[remainder - 1][2];
            return result; 
        }
        let currentCount = countRowSafeTiles(newRow);
        let accumulatedCount = history[history.length-1][2];
        history.push([newRow, currentCount, accumulatedCount + currentCount]);
    }
    return history[history.length-1][2];
}

function processInput(input, isBrowser){

    const inputLines = input.split(/\r?\n/);
    const inputLine = inputLines[0];

    output('Day 18', isBrowser);
    output('Question 1: Starting with the map in your puzzle input, in a total of 40 rows (including the starting row), how many safe tiles are there?', isBrowser);
    let answer1 = countTotalSafeTiles(inputLine, 40);
    output('Answer: ' + answer1, isBrowser);
    
    let start = Date.now();
    let answer2 = countTotalSafeTiles(inputLine, 400000);
    output('Question 2: How many safe tiles are there in a total of 400000 rows?', isBrowser);
    output('Answer: ' + answer2, isBrowser);
    let end = Date.now();
    // Execution time for Answer 2 (stupid counting): 3211 ms
    output(`Execution time for Answer 2 (stupid counting): ${end - start} ms`, isBrowser);

    start = Date.now();
    answer2 = countTotalSafeTiles2(inputLine, 400000);
    output('Question 2: How many safe tiles are there in a total of 400000 rows?', isBrowser);
    output('Answer: ' + answer2, isBrowser);
    end = Date.now();
    // Execution time for Answer 2 (find cycle): 3490 ms
    // Somehow slower than "stupid counting"!
    output(`Execution time for Answer 2 (find cycle): ${end - start} ms`, isBrowser);
}
