"use strict"

///// Author: Chi-Kit Pao

const WIDTH = 100;
const HEIGHT = 100;
let map;
let states;
let counter = 0;

class State{
    constructor(steps, x, y){
        // x is column, y is row
        this.steps = steps;
        this.pos = {x: x, y: y};
        this.hash = State.#getHash(x, y);
    }
    tryNextSteps(endState) {
        let newStates = new Array();
        let moves = [[0, -1], [1, 0], [0, 1], [-1, 0]];
        for(let move of moves){
            let newX = this.pos.x + move[0];
            let newY = this.pos.y + move[1];
            if(this.#isValidPosition(newX, newY)){
                if(newX == endState.pos.x && newY == endState.pos.y){
                    endState.steps = this.steps + 1;
                    break;
                }
                let hash = State.#getHash(newX, newY);
                if(!states.has(hash)){
                    let state = new State(this.steps + 1, newX, newY);
                    states.set(state.hash, state);
                    newStates.push(state);
                }
            }
        }
        return newStates;
    }
    static #getHash(x, y){
        return y * 1000 + x;
    }
    #isValidPosition(x, y){
        if(x < 0 || y < 0)
            return false;
        if(x >= WIDTH || y >= HEIGHT)
            return false;

        if(map[y][x] == ' ')
            return true;

        return false;
    }
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

function calculateSteps(puzzleInput){
    states = new Map(); // State Objects
    map = Array.from({length: HEIGHT}, () => new Array(WIDTH));
    for(let y = 0; y < HEIGHT; ++y){
        for(let x = 0; x < WIDTH; ++x){
            let v1 = x*x + 3*x + 2*x*y + y + y*y + puzzleInput;
            let v2 = Number(v1).toString(2);
            let bitCount = 0;
            v2.split('').forEach(v => { if(v == '1') bitCount++; });
            map[y][x] = (bitCount % 2) ? '#' : ' ';
        }
    }

    let initialState = new State(0, 1, 1);
    states.set(initialState.hash, initialState);
    let endState = new State(undefined, 31, 39);
    states.set(endState.hash, endState);

    // Try BFS since distances are equal (=1) between states.
    let candidates = new Array();
    candidates.push(initialState);
    while(candidates.length > 0){
        let candidate = candidates.shift();
        if(candidate.steps <= 50)
            counter++;
        map[candidate.pos.y][candidate.pos.x] = 'O';
        let newStates = candidate.tryNextSteps(endState);
        if(endState.steps !== undefined)
            break;
        candidates.push(...newStates);
    }

    return endState;
}

solveQuiz((typeof window !== 'undefined'));

function solveQuiz(isBrowser){
    const start = Date.now();
    const puzzleInput = 1358;
    let endState = calculateSteps(puzzleInput);
    
    output('Day 13', isBrowser);
    output('Question 1: What is the fewest number of steps required for you to reach 31,39?', isBrowser);
    output('Answer: ' + endState.steps, isBrowser);
    output('Question 2: How many locations (distinct x,y coordinates, including your starting location) can you reach in at most 50 steps?', isBrowser);
    output('Answer: ' + counter, isBrowser);

    const end = Date.now();
    // Execution time: 117 ms
    output(`Execution time: ${end - start} ms`, isBrowser);
}
