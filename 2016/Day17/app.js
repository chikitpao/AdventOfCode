"use strict"

///// Author: Chi-Kit Pao

let isBrowser = true;
if (typeof window === 'undefined') {
    // Node.js
    isBrowser = false;
    getNodeJsCryptoModule().then(function (crypto){
        let createHash = function(string){
            var hash = crypto.createHash('md5');
            hash.update(string);
            return hash.digest('hex');
        };
        processInput(isBrowser, createHash);
    });
} else {
    // Web API
    output('Day 14', isBrowser);
    let errorMsg = "Error: It seems that MD5 is not supported by Web API anymore!";
    output(errorMsg, isBrowser);
    throw errorMsg;
}

async function getNodeJsCryptoModule() {
    const crypto = await import('crypto');
    return crypto;
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

let mazeString = 
`#########
#S| | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | |  
####### V`;



class MazeStep{
    static directions = [[-1, 0], [1, 0], [0, -1], [0, 1]]; // up, down, left, right
    static directionNames = ['U', 'D', 'L', 'R']; // up, down, left, right
    static bCode = 'b'.charCodeAt(0);
    static fCode = 'f'.charCodeAt(0);
    constructor(maze, path, row, column, g){
        this.maze = maze;
        this.path = path;
        this.row = row;
        this.column = column;
        this.g = g;
        this.h = (this.maze.endPosition[0] - row) + (this.maze.endPosition[1] - column);
        this.f = this.g + this.h;
    }
    findNextSteps(){
        let nextSteps = [];
        let hash = this.maze.createHash(this.maze.passcode + this.path);
        for(let entry of MazeStep.directions.entries()){
            let neighborRow = this.row + entry[1][0];
            let neighborColumn = this.column + entry[1][1];
            let nextRow = this.row + 2 * entry[1][0];
            let nextColumn = this.column + 2 * entry[1][1];
            if(this.maze.map[neighborRow][neighborColumn] == '#') // wall
                continue;
            if(this.#isOpen(hash[entry[0]])){
                const directionName = MazeStep.directionNames[entry[0]];
                nextSteps.push(new MazeStep(this.maze, this.path + directionName, nextRow, nextColumn, this.g + 2))
            }
        }
        return nextSteps;
    }
    #isOpen(char){
        const code = char.charCodeAt(0);
        return code >= MazeStep.bCode && code <= MazeStep.fCode;
    }
}

class Maze{
    constructor(createHash, passcode){
        this.createHash = createHash;
        this.passcode = passcode;
        this.map = mazeString.split(/\r?\n/)
        this.columnCount = this.map[0].length;
        this.rowCount = 0;
        this.startPosition;
        this.endPosition;

        for(let entry of this.map.entries()){
            if(!entry[1])
                continue;
            this.rowCount++;
            if(!this.startPosition){
                let index = entry[1].indexOf('S');
                if(index != -1)
                    this.startPosition = [entry[0], index];
            }
            if(!this.endPosition){
                let index = entry[1].indexOf('V');
                if(index != -1)
                    this.endPosition = [entry[0] - 1, index - 1];
            }
        }
    }
    findShortestPath(){
        // using A* search algorithm
        let candidates = [];
        let startStep = new MazeStep(this, '', this.startPosition[0], this.startPosition[1],
            0);
        candidates.push(startStep);
        
        while(candidates.length > 0){
            let currentStep = candidates.shift();
            let nextSteps = currentStep.findNextSteps();
            for(let entry of nextSteps.entries()){
                let nextStep = entry[1];
                if(nextStep.row == this.endPosition[0] && nextStep.column == this.endPosition[1]){
                    return nextStep.path;
                }
            }
            this.#addSteps(candidates, nextSteps);
        }
        return '';
    }
    findLongestPath(){
        let candidates = [];
        let paths = [];
        let startStep = new MazeStep(this, '', this.startPosition[0], this.startPosition[1],
            0);
        candidates.push(startStep);
        
        while(candidates.length > 0){
            let currentStep = candidates.shift();
            let nextSteps = currentStep.findNextSteps();
            for(let entry of nextSteps.entries()){
                let nextStep = entry[1];
                if(nextStep.row == this.endPosition[0] && nextStep.column == this.endPosition[1]){
                    paths.push(nextStep);
                    nextSteps[entry[0]] = null;
                }
            }
            this.#addSteps(candidates, nextSteps);
        }
        if(paths.length > 0){
            paths.sort((a, b) => a.path.length - b.path.length);
            return paths[paths.length - 1].path;
        }

        return '';
    }
    #addSteps(candidates, nextSteps){
        for(let nextStep of nextSteps){
            if(!nextStep)
                continue;
            if(candidates.length == 0){
                candidates.push(nextStep)
            } else {
                for(let i = 0; i < candidates.length; ++i){
                    if(i + 1 < candidates.length && nextStep.h <= candidates[i].h){
                        candidates.splice(i, 0, nextStep);
                        break;
                    }
                    if(i + 1 >= candidates.length){
                        candidates.push(nextStep);
                        break;
                    }
                }
            }
        }
    }
}

function processInput(isBrowser, createHash){
    output('Day 17', isBrowser);
    
    let start = Date.now();
    let maze = new Maze(createHash, 'edjrjqaa');
    output(`Question 1: Given your vault's passcode, what is the shortest path (the actual path, not just the length) to reach the vault?`, isBrowser);
    const answer1 = maze.findShortestPath();
    output('Answer: ' + answer1, isBrowser);
    let end = Date.now();
    // Execution time: 3 ms
    output(`Execution time: ${end - start} ms`);

    start = Date.now();
    maze = new Maze(createHash, 'edjrjqaa');
    output('Question 2: What is the length of the longest path that reaches the vault?', isBrowser);
    const answer2 = maze.findLongestPath().length;
    output('Answer: ' + answer2, isBrowser);
    end = Date.now();
    // Execution time: 220 ms
    output(`Execution time: ${end - start} ms`);
}
