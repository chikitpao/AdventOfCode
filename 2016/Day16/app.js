"use strict"

///// Author: Chi-Kit Pao

let isBrowser = (typeof window !== 'undefined');
solveQuiz(isBrowser);

function output(string, isBrowser) {
    console.log(string);
    if(isBrowser) {
        let bodys = document.getElementsByTagName('body');
        let p = document.createElement('p');
        p.textContent = string;
        bodys[0].appendChild(p);
    }
}

function calculateData(input, length){
    let result = input;
    while(result.length < length){
        let flipped = Array.from({length: result.length}, (v, i) => ((result.substring(i,i+1) == '1') ? '0' : '1'));
        let reverse = flipped.reverse().join('');
        result = result + '0' + reverse;
    }
    return result.substring(0, 0 + length);
}

function calculateCheckSum(data){
    console.assert(!(data.length % 2))
    let checkSum = data;
    do{
        let temp = ''
        for(let i = 0; i < checkSum.length; i+=2)
        {
            switch(checkSum.substring(i, i+2)){
                case '00':
                case '11':
                    temp += '1';
                    break;
                case '01':
                case '10':
                    temp += '0';
                    break;
            }
        }
        checkSum = temp;
    }while(!(checkSum.length % 2));
    return checkSum;
}

function solveQuiz(isBrowser){
    const start = Date.now();
    let puzzleInput = '01111001100111011';
    let length1 = 272;
    let data = calculateData(puzzleInput, length1);
    let checkSum1 = calculateCheckSum(data);
    
    output('Day 16', isBrowser);
    output('Question 1: The first disk you have to fill has length 272. Using the initial state in your puzzle input, what is the correct checksum?', isBrowser);
    output(`Answer: ${checkSum1}`, isBrowser);

    output('Question 2: The second disk you have to fill has length 35651584. Again using the initial state in your puzzle input, what is the correct checksum for this disk?', isBrowser);
    let length2 = 35651584;
    data = calculateData(puzzleInput, length2);
    let checkSum2 = calculateCheckSum(data);
    output(`Answer: ${checkSum2}`, isBrowser);

    const end = Date.now();
    output(`Execution time: ${end - start} ms`, isBrowser);
}
