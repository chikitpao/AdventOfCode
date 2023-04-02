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

function decompressV1(inputLine){
    let filePointer = 0;
    let decompressedData = '';
    while(filePointer < inputLine.length){
        if(inputLine[filePointer] == '('){
            let found = inputLine.substring(filePointer).match(/\((\d+)x(\d+)\)/)
            // Example result
            // (3) ['(172x1)', '172', '1', index: 0, input: '(172x1)(3x7)XPJ(70x4)(40x7)WKQANMDILIQOOWQZDN…JTRSFEBTFFDTDRZ(13x7)RHQRORLJVNLWN(6x2)INZFFT', groups: undefined]
            if(found.length != 3)
                throw 'Problem while matching RegExp';
            
            // Move file pointer behind decompression expression
            filePointer += found[0].length;
            
            let length = parseInt(found[1]);
            let repeat = parseInt(found[2]);
            let text = inputLine.substring(filePointer, filePointer + length);
            for(let i = 0; i < repeat; ++i)
                decompressedData += text;

            // Move file pointer behind processed text
            filePointer += length;
        } else {
            decompressedData += inputLine[filePointer++];
        }
    }
    return decompressedData;
}

// Recursive function
function decompressV2(inputLine){
    let decompressedDataSize = 0;
    let filePointer = 0;

    function updateDataSize(size, update)
    {
        let newSize = size + update;
        if(newSize < size)
            throw "Integer overflow";
        return newSize;
    }

    while(filePointer < inputLine.length){
        if(inputLine[filePointer] == '('){
            let found = inputLine.substring(filePointer).match(/\((\d+)x(\d+)\)/)
            // Example result
            // (3) ['(172x1)', '172', '1', index: 0, input: '(172x1)(3x7)XPJ(70x4)(40x7)WKQANMDILIQOOWQZDN…JTRSFEBTFFDTDRZ(13x7)RHQRORLJVNLWN(6x2)INZFFT', groups: undefined]
            if(found.length != 3)
                throw 'Problem while matching RegExp';
            
            // Move file pointer behind decompression expression
            filePointer += found[0].length;
            
            let length = parseInt(found[1]);
            let repeat = parseInt(found[2]);
            let text = inputLine.substring(filePointer, filePointer + length);

            let realTextLength = decompressV2(text);
            decompressedDataSize = updateDataSize(decompressedDataSize, repeat * realTextLength);

            // Move file pointer behind processed text
            filePointer += length;
        } else {
            decompressedDataSize = updateDataSize(decompressedDataSize, 1);
            filePointer++;
        }
    }
    return decompressedDataSize;
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    let decompressedData = decompressV1(inputLines[0]);
    let decompressedDataV2Size = decompressV2(inputLines[0]);

    output('Day 9', isBrowser);
    output('Question 1: What is the decompressed length of the file (your puzzle input)?', isBrowser);
    output('Answer: ' + decompressedData.length, isBrowser);
    output('Question 2: What is the decompressed length of the file using this improved format?', isBrowser);
    output('Answer: ' + decompressedDataV2Size, isBrowser);
}
