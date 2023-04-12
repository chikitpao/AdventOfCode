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

class Node{
    constructor(x, y, size, used, avail, use){
        this.x = x;
        this.y = y;
        this.size = size;
        this.used = used;
        this.avail = avail;
        this.use = use;
    }
}


let nodes = new Array();
let grid;
let maxX = 0;
let maxY = 0;
let HEIGHT = 0;
let WIDTH = 0;

function initializeData(inputLines){
    // Header: Filesystem              Size  Used  Avail  Use%
    // Example Line: /dev/grid/node-x0-y0     88T   67T    21T   76%
    for(let inputLine of inputLines){
        if(!inputLine)
            continue;
        let found = inputLine.match(/\/dev\/grid\/node-x(?<x>\d+)-y(?<y>\d+)\s+(?<size>\d+)T\s+(?<used>\d+)T\s+(?<avail>\d+)T\s+(?<use>\d+)%/);
        if(!found)
            throw "Invalid input line: " + inputLine
        let x = parseInt(found.groups.x);
        let y = parseInt(found.groups.y);
        let size = parseInt(found.groups.size);
        let used = parseInt(found.groups.used);
        let avail = parseInt(found.groups.avail);
        let use = parseInt(found.groups.use);
        if(x > maxX)
            maxX = x;
        if(y > maxY)
            maxY = y;
        // x 24 y 22 is empty
        if(used == 0)
            console.log(`x ${x} y ${y} is empty`);
        nodes.push(new Node(x, y, size, used, avail, use));
    }

    // maxX = 31, maxY = 27
    // HEIGHT 28, WIDTH 32
    HEIGHT = maxY + 1;
    WIDTH = maxX + 1;
    grid = Array.from({length: HEIGHT}, () => new Array(WIDTH));
    for(let i = 0; i < nodes.length; ++i){
        grid[nodes[i].y][nodes[i].x] = nodes[i];
    }
}

function getViablePairCount(){
    let viablePairCount = 0;
    for(let i = 0; i < nodes.length; ++i){
        for(let j = 0; j < nodes.length; ++j){
            if(i == j)
                continue;
            if(nodes[i].used == 0)
                continue;
            if(nodes[i].used <= nodes[j].avail){
                viablePairCount++;
            }
        }
    }
    return viablePairCount;
}

function checkNeighborCapacity(){
    for(let i = 0; i < HEIGHT; ++i){
        for(let j = 0; j < WIDTH; ++j){
            if(j + 1 < WIDTH){
                if(grid[i][j].size < grid[i][j + 1].used){
                    // Check capable for data of right neighbor.
                    // Logged for x 9 y 11 to x 31 y 11
                    // Note: No log for first row.
                    console.log(`x ${j} y ${i} cannot hold data of x ${j+1} y ${i} (right).`);
                }
            }
            if(j > 0){
                if(grid[i][j].size < grid[i][j - 1].used){
                    // Check capable for data of left neighbor.
                    // This is not logged.
                    console.log(`x ${j} y ${i} cannot hold data of x ${j-1} y ${i} (left).`);
                }
            }
            if(i + 1 < HEIGHT){
                if(grid[i][j].size < grid[i + 1][j].used){
                    // Check capable for data of lower neighbor.
                    // Logged for x 8 y 12.
                    console.log(`x ${j} y ${i} cannot hold data of x ${j} y ${i+1} (lower).`);
                }
            }
            if(i > 0){
                if(grid[i][j].size < grid[i - 1][j].used){
                    // Check capable for data of upper neighbor.
                    // Logged for x 9 y 13 to x31 y 13.
                    console.log(`x ${j} y ${i} cannot hold data of x ${j} y ${i-1} (upper).`);
                }
            }
        }
    }
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    inputLines.splice(0, 2);
    initializeData(inputLines);

    // Part 1
    let viablePairCount = getViablePairCount();

    // Part 2
    checkNeighborCapacity();
    // With the knowledge of empty node (x 24, y 22) and data found by function checkNeighborCapacity, we can move 
    // empty node / goal data the following way:
    // #1 Move data left of empty node, in a range from (x 23, y 22) to (x 8, y 22). => 16 steps.
    // #2 Move data above empty node, in a range (x 8, y 21) to (x 8, y 0). => 22 steps.
    // #3 Move data right of empty node, in a range (x 9, y 0) to (x 31, y 0). => 23 steps. 
    //    Goal data has been moved to (x 30, y 0) now. Empty node is (x 31, y 0).
    // #4 Repeat following operations 30 times: 
    // #4.1 Move empty node 1x down, 2x left, 1x up (empty node was behind goal data, afterwards in front of goal data). => 4 steps.
    // #4.2 Move goal data left to empty node. => 1 step.
    // In the end, goal data is at node (x 0, y 0) and empty node is (x 1, y 0).
    let steps = 16 + 22 + 23 + 30 * (4 + 1);

    output('Day 22', isBrowser);
    output('Question 1: How many viable pairs of nodes are there?', isBrowser);
    output('Answer: ' + viablePairCount, isBrowser);
    output('Question 2: What is the fewest number of steps required to move your goal data to node-x0-y0?', isBrowser);
    output('Answer: ' + steps, isBrowser);
}
