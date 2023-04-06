"use strict"

///// Author: Chi-Kit Pao
// Advent of Code Day 11, part 2.
// Rewritten program from part 1.
// Used some tips by p_tseng (URL: https://www.reddit.com/r/adventofcode/comments/5hoia9/2016_day_11_solutions/)

const MAX_FLOORS = 4; // F1 to F4 => 0 to 3
const MAX_ELEMENTS = 7;

class State{
    constructor(steps, floor, objects){
        this.steps = steps;
        this.elevatorFloor = floor;
        // Row: floor of generator, column: floor of microchip. Content: How many elements with this constellation.
        // Index calculated by row * MAX_FLOORS + column.
        if(objects !== undefined){
            this.objects = objects;
        } else {
            this.objects = new Array.from({ length: (MAX_FLOORS * MAX_FLOORS) }, () => 0);
        }
        this.hash = State.createHash(this.elevatorFloor, this.objects);
    }
    static parseObjectString(str){
        let result = new Array(MAX_FLOORS * MAX_FLOORS);
        for(let i = 1; i < str.length; ++i)
            result[i-1] = parseInt(str[i]);
        return result;
    }
    static createHash(floor, objects){
        let objectsHash = objects.map(v => v.toString());
        return floor.toString() + objectsHash.join('');
    }
    static getIndex(gen, chip){
        return gen * MAX_FLOORS + chip;
    }
    tryNextSteps(endState){
        let result = new Array();
        let newStateHashsList = new Array();

        if(isNaN(this.steps))
            throw "steps is NaN!";

        // Try moving up
        if(this.elevatorFloor + 1 < MAX_FLOORS){
            let nextFloor = this.elevatorFloor + 1;
            newStateHashsList.push(...this.#calculatePossibleNewStates(this, this.elevatorFloor, nextFloor));
        }
        // Try moving down
        if(this.elevatorFloor > 0n){
            let nextFloor = this.elevatorFloor - 1;
            newStateHashsList.push(...this.#calculatePossibleNewStates(this, this.elevatorFloor, nextFloor));
        }
        for(let entry of newStateHashsList){
            if(entry.localeCompare(endState.hash) == 0){
                endState.steps = this.steps + 1;
                break;
            }
            if(!states.has(entry))
            {
                let newState = new State(this.steps + 1, parseInt(entry[0]), State.parseObjectString(entry));
                states.set(entry, newState);
                result.push(newState);
            }
        }
        return result; 
    }
    #calculatePossibleNewStates(currentState, currentFloor, nextFloor) {
        let newStateHashsList = new Array();
        // 1. Only transport one object
        // 2.Transport two objects of the same type (microchips or generators)
        for(let i = 0; i < MAX_FLOORS; i++){
            // 1.1 generator: [or, c] := -1 -> [nr, c] += 1 (in the same column)
            let generatorIndex = State.getIndex(currentFloor, i);
            if(this.objects[generatorIndex] > 0 ){
                let newArray = Array.from(currentState.objects);
                newArray[generatorIndex]--;
                newArray[State.getIndex(nextFloor, i)]++;
                let forbidden = this.#isForbiddenState(newArray, currentFloor, nextFloor);
                if(!forbidden){
                    newStateHashsList.push(State.createHash(nextFloor, newArray));
                }
                // 2.2 two generators: [or, c] := -1 -> [nr, c] += 1 (in the same column)
                // Check another generator to move
                // NOTE: It's possible that moving one generator is an invalid move but moving
                // two is valid (when there are two chips on the next floor).
                for(let j = i; j < MAX_FLOORS; ++j){
                    if(newArray[State.getIndex(currentFloor, j)] > 0){
                        let newArray2 = Array.from(newArray);
                        newArray2[State.getIndex(currentFloor, j)]--;
                        newArray2[State.getIndex(nextFloor, j)]++;
                        forbidden = this.#isForbiddenState(newArray2, currentFloor, nextFloor);
                        if(!forbidden)
                            newStateHashsList.push(State.createHash(nextFloor, newArray2));
                    }
                }
            }
            // 1.2 microchip: [r, oc] := -1 -> [r, nc] += 1 (in the same row)
            let microchipIndex = State.getIndex(i, currentFloor);
            if(this.objects[microchipIndex] > 0){
                let newArray = Array.from(currentState.objects);
                newArray[microchipIndex]--;
                newArray[State.getIndex(i, nextFloor)]++;
                let forbidden = this.#isForbiddenState(newArray, currentFloor, nextFloor);
                if(!forbidden){
                    newStateHashsList.push(State.createHash(nextFloor, newArray));
                    // 2.1 two microchips: [r, oc] := -1 -> [r, nc] += 1 (in the same row)
                    // Check another microchip to move
                    for(let j = i; j < MAX_FLOORS; ++j){
                        if(newArray[State.getIndex(j, currentFloor)] > 0){
                            let newArray2 = Array.from(newArray);
                            newArray2[State.getIndex(j, currentFloor)]--;
                            newArray2[State.getIndex(j, nextFloor)]++;
                            forbidden = this.#isForbiddenState(newArray2, currentFloor, nextFloor);
                            if(!forbidden)
                                newStateHashsList.push(State.createHash(nextFloor, newArray2));
                        }
                    }
                }
            }
        }
        
        // 3.Transport generators and microchips for the same element.
        // [or, oc] := -1 -> [nr, nc] += 1 (on main diagonal)
        let index = State.getIndex(currentFloor, currentFloor);
        if(this.objects[index] > 0){
            let newArray = Array.from(currentState.objects);
            newArray[index]--;
            newArray[State.getIndex(nextFloor, nextFloor)]++;
            let forbidden = this.#isForbiddenState(newArray, currentFloor, nextFloor);
            if(!forbidden)
                newStateHashsList.push(State.createHash(nextFloor, newArray));
        }
        return newStateHashsList;
    }
    #isForbiddenState(objects, currentFloor, nextFloor){
        function isForbidden(objects, floor){
            let hasUnprotectedChip = false;
            for(let i = 0; i < MAX_FLOORS; ++i){
                if(i == floor)
                    continue;
                if(objects[i * MAX_FLOORS + floor] > 0){
                    hasUnprotectedChip = true;
                    break;
                }
            }
            if(!hasUnprotectedChip)
                return false;
            for(let i = 0; i < MAX_FLOORS; ++i){
                if(objects[floor * MAX_FLOORS + i] > 0){
                    // found generator
                    return true;
                }
            }
            return false;
        }
        if(isForbidden(objects, currentFloor))
           return true;
        if(isForbidden(objects, nextFloor))
           return true;
        return false;
    }
}

let states;

function output(string, isBrowser) {
    console.log(string);
    if(isBrowser) {
        let bodys = document.getElementsByTagName('body');
        let p = document.createElement('p');
        p.textContent = string;
        bodys[0].appendChild(p);
    }
}

function calculateSteps(v2){
    states = new Map(); // State Objects
    let array = Array(MAX_FLOORS * MAX_FLOORS).fill(0);
    array[0] = (v2) ? 5 : 3;
    array[1] = 2;
    let initialState = new State(0, 0, array);
    states.set(initialState.hash, initialState);
    array = Array(MAX_FLOORS * MAX_FLOORS).fill(0);
    array[array.length - 1] = (v2) ? 7 : 5;
    let endState = new State(undefined, 3, array);
    states.set(endState.hash, endState);

    // Try BFS since distances are equal (=1) between states.
    let candidates = new Array();
    candidates.push(initialState);
    while(candidates.length > 0){
        let candidate = candidates.shift();
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
    // Initial State
    // F1 (E): GPo, GTh, MTh, GPr, GRu, MRu, GCo, MCo, GEl*, MEl*, GDi*, MDi*
    // F2: MPo, MPr
    // F3: - 
    // F4: -
    // End State
    // F1: -
    // F2: - 
    // F3: - 
    // F4 (E): GPo, MPo, GTh, MTh, GPr, MPr, GRu, MRu, GCo, MCo, GEl*, MEl*, GDi*, MDi*
    // *: Added in part 2
    
    let endState1 = calculateSteps(false);
    let endState2 = calculateSteps(true);

    output('Day 11', isBrowser);
    output('Question 1: In your situation, what is the minimum number of steps required to bring all of the objects to the fourth floor?', isBrowser);
    output('Answer: ' + endState1.steps, isBrowser);
    output('Question 2: What is the minimum number of steps required to bring all of the objects, including these four new ones, to the fourth floor?', isBrowser);
    output('Answer: ' + endState2.steps, isBrowser);

    const end = Date.now();
    // Execution time: 117 ms
    output(`Execution time: ${end - start} ms`, isBrowser);
}
