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

function isValidIp(ranges, ip){
    const boundary = 2**32;
    if(ip >= boundary)
        return false;
    for(let range of ranges){
        if(ip >= range[0] && ip <= range[1])
            return false;
    }
    return true;
}

function countValidIps(ranges, candidates)
{
    let validIps = 0;
    for(let c of candidates){
        let ip = c;
        while(isValidIp(ranges, ip)){
            validIps++;
            ip++;
        }
    }
    return validIps;
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    let ranges = new Array();
    let candidates = new Array();
    for(let inputLine of inputLines){
        if(!inputLine)
            continue;
        let found = inputLine.match(/(\d+)-(\d+)/);
        if(!found)
            throw "Cannot parse input: " + inputLine;
        let end = parseInt(found[2]);
        ranges.push([parseInt(found[1]), end]);
        candidates.push(end + 1);
    }
    ranges.sort((a, b) => a[0] - b[0]);
    candidates = candidates.filter(value => isValidIp(ranges, value));
    if(isValidIp(ranges, 0))
        candidates.unshift(0);
    candidates.sort((a, b) => a - b);
    let lowestFreeIp = candidates[0];
    let allowed = countValidIps(ranges, candidates);

    output('Day 20', isBrowser);
    output('Question 1: Given the list of blocked IPs you retrieved from the firewall (your puzzle input), what is the lowest-valued IP that is not blocked?', isBrowser);
    output('Answer: ' + lowestFreeIp, isBrowser);
    output('Question 2: How many IPs are allowed by the blacklist?', isBrowser);
    output('Answer: ' + allowed, isBrowser);
}
