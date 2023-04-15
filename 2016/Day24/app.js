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

class MazeStep{
    constructor(steps, row, column){
        this.steps = steps;
        this.row = row;
        this.column = column;
    }
}

class Maze{
    constructor(map){
        this.map = map;
        this.markers = new Array(10);
        this.maxMarker = -1;
        this.routesBetweenMarkers; // Filled when user calls findShortestRoute.
        this.#findMarkers();
    }
    findShortestRoute(v2=false){
        this.#findShortestRoutesBetweenMarkers();
        return this.#findShortestRoute(v2);
    }
    #bfs(startIndex, endIndex){
        let startMarker = this.markers[startIndex];
        let endMarker = this.markers[endIndex];
        let visited = new Set();
        let candidates = new Array();
        candidates.push(new MazeStep(0, startMarker[0], startMarker[1]));
        let directions = [[-1, 0], [0, 1],[1, 0],[0, -1]]; // up, right, down, left
        while(candidates.length > 0){
            let candidate = candidates.shift();
            for(let dir of directions){
                let newRow = candidate.row + dir[0];
                let newColumn = candidate.column + dir[1];
                let newHash = newRow * this.map[0].length + newColumn;
                if(this.map[newRow][newColumn] != '#' && !visited.has(newHash)){
                    if(newRow == endMarker[0] && newColumn == endMarker[1])
                        return candidate.steps + 1;
                    visited.add(newHash);
                    candidates.push(new MazeStep(candidate.steps + 1, newRow, newColumn));
                }
            }
        }
        throw "Cannot find route between markers"
    }
    #findMarkers(){
        const charCode0 = '0'.charCodeAt(0);
        const charCode9 = '9'.charCodeAt(0);
        for(let rowEntry of this.map.entries()){
            for(let j = 0; j < rowEntry[1].length; ++j){
                let charCode = rowEntry[1][j].charCodeAt(0);
                if(charCode >= charCode0 && charCode <= charCode9){
                    let digit = charCode - charCode0;
                    this.markers[digit] = [rowEntry[0], j]
                    if(digit > this.maxMarker)
                        this.maxMarker = digit;
                }
            }
        }
    }
    #findShortestRoutesBetweenMarkers(){
        this.routesBetweenMarkers = Array.from({length: this.maxMarker + 1}, () => new Array(this.maxMarker + 1));
        for(let i = 0; i <= this.maxMarker; ++i){
            for(let j = 0; j <= this.maxMarker; ++j){
                if(i == j){
                    this.routesBetweenMarkers[i][j] = 0;
                    continue;
                }
                if(j < i){
                    this.routesBetweenMarkers[i][j] = this.routesBetweenMarkers[j][i];
                    continue;
                }
                this.routesBetweenMarkers[i][j] = this.#bfs(i, j);
            }
        }
    }
    #findShortestRoute(v2){
        let shortestRoutes = new Array(this.maxMarker);
        let elements = Array.from({length: this.maxMarker}, (v, i) => i + 1);
        let permutations = getPermutations(elements);
        // Version 1: Find shortest routes consisting all of 1 to 7, starting with 1 to 7 respectively.
        // Version 2: Find shortest routes from 0 to (all of 1 to 7) back to 0.
        for(let permutation of permutations){
            let start = permutation[0];
            let distance = 0;
            if(v2){
                permutation.unshift(0);
                permutation.push(0);
            }
            for(let i = 0; i + 1 < permutation.length; ++i){
                distance += this.routesBetweenMarkers[permutation[i]][permutation[i+1]];
            }
            if((shortestRoutes[start-1] === undefined) || (distance < shortestRoutes[start-1][0]))
                shortestRoutes[start-1] = [distance, permutation];
        }
        // Version 1: Add 0 to routes and update total distance.
        if(!v2){
            for(let i = 1; i <= this.maxMarker; ++i){
                shortestRoutes[i-1][0] += this.routesBetweenMarkers[0][i];
                shortestRoutes[i-1][1].unshift(0);
            }
        }
        let minimum;
        for(let i = 1; i <= this.maxMarker; ++i){
            if(minimum === undefined || shortestRoutes[i-1][0] < shortestRoutes[minimum-1][0])
                minimum = i;
        }
        return shortestRoutes[minimum-1];
    }
}

function generatePermuationIndices(resultIndices, tempIndexArray, elementCount, recursionCount, currentIndex){
    if(tempIndexArray.some((value) => value == currentIndex))
        return;
    tempIndexArray[recursionCount] = currentIndex;
    if(recursionCount + 1 >= elementCount){
        resultIndices.push([...tempIndexArray]);
        tempIndexArray[recursionCount] = -1;
        return;
    }
    for(let i = 0; i < elementCount; ++i){
        generatePermuationIndices(resultIndices, tempIndexArray, elementCount, recursionCount + 1, i);
    }
    tempIndexArray[recursionCount] = -1;
}

function getPermutations(elements){
    let resultIndices = new Array();
    let tempIndexArray = new Array(elements.length).fill(-1);
    for(let i = 0; i < elements.length; ++i){
        generatePermuationIndices(resultIndices, tempIndexArray, elements.length, 0, i);
    }
    let resultElements = new Array();
    resultIndices.forEach((value) => resultElements.push(value.map(value => elements[value])));
    return resultElements;
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    let map = new Array();
    for(let inputLine of inputLines){
        if(!inputLine)
            continue;
        map.push(inputLine);
    }
    let maze = new Maze(map);
    
    output('Day 24', isBrowser);
    output('Question 1: Given your actual map, and starting from location 0, what is the fewest number of steps required to visit every non-0 number marked on the map at least once?', isBrowser);
    let shortestRoute = maze.findShortestRoute();
    output(`ShortestRoutes: ${shortestRoute[1]}`, isBrowser);
    output('Answer: ' + shortestRoute[0], isBrowser);
    output('Question 2: What is the fewest number of steps required to start at 0, visit every non-0 number marked on the map at least once, and then return to 0?', isBrowser);
    let newShortestRoute = maze.findShortestRoute(true);
    output(`ShortestRoutes: ${newShortestRoute[1]}`, isBrowser);
    output('Answer: ' + newShortestRoute[0], isBrowser);
}
