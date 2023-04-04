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

class HashData{
    constructor(index, hash, triplet){
        this.index = index;
        this.hash = hash;
        this.triplet = triplet;
    }
}

class CircularBuffer{
    constructor(capacity){
        this.capacity = capacity;
        this.data = new Array(capacity);
        this.start = 0;
        this.end = 0; // exclusive
        this.full = false;
    }
    get(index){
        if(this.isEmpty())
            return undefined;
        if(index >= this.size())
            return undefined;
        if(!this.data[(this.start + index) % this.capacity])
            throw "get returns invalid data!"
        return this.data[(this.start + index) % this.capacity];
    }
    pop(){
        if(this.isEmpty())
            return undefined;
        let result = this.data[this.start];
        this.start++;
        this.start %= this.capacity;
        this.full = false;
        return result;
    }
    push(value){
        if(this.full)
            throw "Buffer is full";
        
        this.data[this.end] = value;
        this.end++;
        this.end %= this.capacity;
        if(this.end == this.start)
            this.full = true;
    }
    size(){
        if(this.full)
            return this.capacity;
        if(this.start <= this.end)
            return this.end - this.start;
        return this.end + this.capacity - this.start;
    }
    isEmpty(){
        return !this.full && (this.start == this.end);
    }
    isFull(){
        return this.full;
    }
}

function getFirstTriplet(hexdigest){
    for(let i = 0; i + 2 < hexdigest.length; ++i){
        if(hexdigest[i] == hexdigest[i + 1] && hexdigest[i] == hexdigest[i + 2])
            return hexdigest.substring(i, i + 3);
    }
    return '';
}

function hasQuintuplet(hexdigest, triplet){
    for(let i = 0; i + 4 < hexdigest.length; ++i){
        if(triplet[0] == hexdigest[i] 
            && hexdigest[i] == hexdigest[i + 1] && hexdigest[i] == hexdigest[i + 2]
            && hexdigest[i] == hexdigest[i + 3] && hexdigest[i] == hexdigest[i + 4])
            return true;
    }
    return false;
}

function findPassword(puzzleInput, createHash, nth)
{
    let test_var = 0;
    const candiatesCapacity = 1200;
    let candidates = new CircularBuffer(candiatesCapacity);
    let keys = new Array();
   
    while(true){
        let data = puzzleInput + test_var.toString();
        let hexdigest = createHash(data);
        let triplet = getFirstTriplet(hexdigest);
        candidates.push(new HashData(test_var, hexdigest, triplet));
        if(candidates.size() >= 1001){
            let front = candidates.pop();
            if(front.triplet){
                for(let i = 0; i < 1000; ++i){
                    let testData = candidates.get(i);
                    if(hasQuintuplet(testData.hash, front.triplet)){
                        keys.push(front);
                        break;
                    }
                }
            }
        }

        if(keys.length >= nth)
            return keys[nth - 1];
        test_var += 1;
    }
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

function processInput(isBrowser, createHash){
    const start = Date.now();

    function createStrechedKey(string){
        let result = string;
        for(let i = 0; i < 2017; ++i)
            result = createHash(result);
        return result;
    }

    const puzzleInput = 'cuanljph';
    output('Day 14', isBrowser);
    output('Question 1: Given the actual salt in your puzzle input, what index produces your 64th one-time pad key?', isBrowser);
    output('Answer: ' + findPassword(puzzleInput, createHash, 64).index, isBrowser);
    output('Question 2: Given the actual salt in your puzzle input and using 2016 extra MD5 calls of key stretching, what index now produces your 64th one-time pad key?', isBrowser);
    output('Answer: ' + findPassword(puzzleInput, createStrechedKey, 64).index, isBrowser);

    const end = Date.now();
    output(`Execution time: ${end - start} ms`);
}
