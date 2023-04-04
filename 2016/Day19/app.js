"use strict"

///// Author: Chi-Kit Pao

let isBrowser = (typeof window !== 'undefined');

function output(string, isBrowser) {
    console.log(string);
    if(isBrowser) {
        let bodys = document.getElementsByTagName('body');
        let p = document.createElement('p');
        p.textContent = string;
        bodys[0].appendChild(p);
    }
}

// Problem is also known as "Josephus problem". See:
// - Wikipedia page "Josephus problem" (URL: https://en.wikipedia.org/wiki/Josephus_problem)
// - Youtube video "The Josephus Problem - Numberphile" (URL: https://www.youtube.com/watch?v=uCsD3ZGzMgE).
function getSafePlaceV1(n){
	let l = n - (1 << Math.floor(Math.log2(n)));
	return 2 * l  + 1;
}

function getSafePlaceV2(n){
    if(n < 3)
        return 1;
    if(n == 3)
        return 3;
    
    // Not sure about precision with (log(n)/log(3)) and with (test / 3),
    // so test with (test * 3).
    let test;
    for(test = 3; n >= (test * 3); test*=3){
        if(n == (test * 3))  // power of three
            return n;
    }
    // test < n < test * 3
    if(n - test <= test)
        return n - test;
    
    return 2 * n - 3 * test; 
}

const input = 3012210;
output('Day 19', isBrowser);
output('Question 1: With the number of Elves given in your puzzle input, which Elf gets all the presents?', isBrowser);
output('Answer: ' + getSafePlaceV1(input), isBrowser);
output('Question 2: With the number of Elves given in your puzzle input, which Elf now gets all the presents?', isBrowser);
output('Answer: ' + getSafePlaceV2(input), isBrowser);
