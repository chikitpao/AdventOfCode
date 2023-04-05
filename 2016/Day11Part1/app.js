"use strict"

///// Author: Chi-Kit Pao

const MAX_FLOORS = 4; // F1 to F4 => 0 to 3

class Elements{
    constructor(){
        // Number for elements
        this.po = 0n;
        this.th = 1n;
        this.pr = 2n;
        this.ru = 3n;
        this.co = 4n;
        this.max = 5n;

        this.microchips = Array.from({ length: Number(this.max) }, (v, i) => (1n << BigInt(i)));
        this.generators = Array.from({ length: Number(this.max) }, (v, i) => (1n << (BigInt(i) + this.max)));

        this.conversions = new Map();
        this.conversions.set('Po', this.po);
        this.conversions.set('Th', this.th);
        this.conversions.set('Pr', this.pr);
        this.conversions.set('Ru', this.ru);
        this.conversions.set('Co', this.co);
    }
    stringToBigInt(str){
        return this.conversions.get(str);
    }
    generateFloorBits(floor, floorObjects){
        let bits = 0n;
        for(const value of floorObjects){
            bits |= (value << (BigInt(floor) * 2n * elements.max))
        }
        return bits;
    }
}
let elements = Object.freeze(new Elements())

function G(symbol){
    return elements.generators[elements.stringToBigInt(symbol)]; 
}
function M(symbol){
    return elements.microchips[elements.stringToBigInt(symbol)];
}

class Elevator{
    static floorToBits(floor){
        return floor << (BigInt(MAX_FLOORS) * 2n * elements.max); 
    }
    static bitsToFloor(bits){
        return bits >> (BigInt(MAX_FLOORS) * 2n * elements.max);
    }
    static moveFloor(floor, bits){
        let floorBits = floor << (BigInt(MAX_FLOORS) * 2n * elements.max);
        let objectBits = bits & ((1n << (BigInt(MAX_FLOORS) * 2n * elements.max)) - 1n);
        return floorBits |= objectBits;
    }
    static getFloorObjectBits(floor, bits){
        let result = bits & (((1n << (2n * elements.max)) - 1n) << (floor * 2n * elements.max));
        result = result >> (floor * 2n * elements.max);
        return result;
    }
    static resetFloorBits(stateBits, floor, bit){
        return stateBits &= ~(bit << (floor * 2n * elements.max));
    }
    static setFloorBits(stateBits, floor, bit){
        return stateBits |= (bit << (floor * 2n * elements.max));
    }
}

let possibleMoves = new Array();
function initializePossibleMoves()
{
   // Possible moves with elevator:
    // 1. Only transport one object
    for(let i = 0n; i < 2n * elements.max; ++i)
        possibleMoves.push(1n << i);
    // 2.Transport two objects of the same type (microchips or generators)
    for(let i = 0n; i < elements.max; ++i){
        for(let j = i + 1n; j < elements.max; ++j){
            possibleMoves.push((1n << i) | (1n << j));
            possibleMoves.push((1n << (i + elements.max)) | (1n << (j + elements.max)));
        }
    }
    // 3.Transport generators and microchips for the same element.
    for(let i = 0n; i < elements.max; ++i)
        possibleMoves.push((1n << i) | (1n << (i + elements.max)));
}


class State{
    constructor(steps, elevatorFloor, floors){
        this.steps = steps;
        this.bits = (elevatorFloor !== undefined) ? Elevator.floorToBits(elevatorFloor) : 0n;
        if(floors !== undefined){
            for(let i = 0n; i < MAX_FLOORS; ++i){
                this.bits |= elements.generateFloorBits(i, floors[i]);
            }
        }
    }
    static createDirect(steps, bits){
        let state = new State(steps);
        state.bits = bits;
        return state;
    }
    tryNextSteps(endState){
        let result = new Array();
        let currentFloor = Elevator.bitsToFloor(this.bits);
        let newStateBitsList = new Array();

        if(isNaN(this.steps))
            throw "steps is NaN!";

        // Try moving up
        if(currentFloor + 1n < BigInt(MAX_FLOORS)){
            let nextFloor = currentFloor + 1n;
            newStateBitsList.push(...this.#calculatePossibleNewStates(this, currentFloor, nextFloor));
        }
        // Try moving down
        if(currentFloor > 0n){
            let nextFloor = currentFloor - 1n;
            let nextFloorObjects = Elevator.getFloorObjectBits(nextFloor, this.bits)
            if(nextFloorObjects != 0n)
                newStateBitsList.push(...this.#calculatePossibleNewStates(this, currentFloor, nextFloor));
        }
        for(let entry of newStateBitsList){
            if(entry == endState.bits){
                endState.steps = this.steps + 1;
                break;
            }
            if(!states.has(entry))
            {
                let newState = State.createDirect(this.steps + 1, entry);
                states.set(entry, newState);
                result.push(newState);
            }
        }
        return result; 
    }
    #calculatePossibleNewStates(currentState, currentFloor, nextFloor){
        let newStateBitsList = new Array();
        let currentFloorObjects = Elevator.getFloorObjectBits(currentFloor, currentState.bits);
        
        for(let move of possibleMoves){
            let newStateBits = Elevator.moveFloor(nextFloor, currentState.bits);
            if(nextFloor != Elevator.bitsToFloor(newStateBits))
                throw "" + nextFloor + " " + Elevator.bitsToFloor(nextStatBit);
            if((currentFloorObjects & move) == move){
                newStateBits = Elevator.resetFloorBits(newStateBits, currentFloor, move);
                newStateBits = Elevator.setFloorBits(newStateBits, nextFloor, move);
                if(prohibitedStates.has(newStateBits))
                    continue;
                if(this.#isProhibitedState(newStateBits, currentFloor))
                {
                    prohibitedStates.add(newStateBits);
                    continue;
                }
                if(this.#isProhibitedState(newStateBits, nextFloor))
                {
                    prohibitedStates.add(newStateBits);
                    continue;
                }
                newStateBitsList.push(newStateBits);
            }
        }
        return newStateBitsList;
    }
    #isProhibitedState(stateBits, floor){
        let floorObjects = Elevator.getFloorObjectBits(floor, stateBits);
        let hasGenerator = false;
        for(let i = 0n; i < elements.max; ++i){
            if(floorObjects & (1n << (i + elements.max))){
                hasGenerator = true;
                // Remove microchip bit since protected.
                floorObjects &= ~(1n << i);
            }
        }
        if(hasGenerator){
            for(let i = 0n; i < elements.max; ++i){
                if(floorObjects & (1n << i))
                    return false; // Found unprotected chip.
            }
        }
        
        return false;
    }
}
let states = new Map(); // State Objects
let prohibitedStates = new Set(); // state bits

function output(string, isBrowser) {
    console.log(string);
    if(isBrowser) {
        let bodys = document.getElementsByTagName('body');
        let p = document.createElement('p');
        p.textContent = string;
        bodys[0].appendChild(p);
    }
}

solveQuiz((typeof window !== 'undefined'));

function solveQuiz(isBrowser){
    const start = Date.now();
    initializePossibleMoves();
 
    // Initial State
    // F1 (E): GPo, GTh, MTh, GPr, GRu, MRu, GCo, MCo
    // F2: MPo, MPr
    // F3: - 
    // F4: -
    // End State
    // F1: -
    // F2: - 
    // F3: - 
    // F4 (E): GPo, MPo, GTh, MTh, GPr, MPr, GRu, MRu, GCo, MCo
    
    let initialState = new State(0, 0n,
        [[G('Po'), G('Th'), M('Th'), G('Pr'), G('Ru'), M('Ru'), G('Co'), M('Co')], 
        [M('Po'), M('Pr')], 
        [],
        []]);
    states.set(initialState.bits, initialState);
    let endState = new State(undefined, 3n,
        [[], 
        [], 
        [],
        [G('Po'), M('Po'), G('Th'), M('Th'), G('Pr'), M('Pr'), G('Ru'), M('Ru'), G('Co'), M('Co')]]);
    states.set(endState.bits, endState);

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

    output('Day 11, Part 1', isBrowser);
    output('Question 1: In your situation, what is the minimum number of steps required to bring all of the objects to the fourth floor?', isBrowser);
    output('Answer: ' + endState.steps, isBrowser);

    const end = Date.now();
    // Execution time: 914472 ms
    output(`Execution time: ${end - start} ms`);
}
