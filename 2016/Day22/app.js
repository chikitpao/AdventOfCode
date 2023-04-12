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

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    inputLines.splice(0, 2);

    let maxX = 0;
    let maxY = 0;

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

    // maxX = 31, maxY = 27;
    const HEIGHT = maxY + 1;
    const WIDTH = maxX + 1;
    let grid = Array.from({length: HEIGHT}, () => new Array(WIDTH));
    for(let i = 0; i < nodes.length; ++i){
        grid[nodes[i].y][nodes[i].x] = nodes[i];
    }

    // Part 1
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

    // TODO: Finish part 2

    output('Day 22', isBrowser);
    output('Question 1: How many viable pairs of nodes are there?', isBrowser);
    output('Answer: ' + viablePairCount, isBrowser);
    output('Question 2: What is the fewest number of steps required to move your goal data to node-x0-y0?', isBrowser);
    output('Answer: ...', isBrowser);
}
