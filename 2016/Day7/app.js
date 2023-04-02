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

function evaluateTls(inputLine){
    if(!evaluateTlsPattern(/(?<=\[)[a-z]*?(?=\])/g, inputLine, true)) // between square brackets inside
        return false;
    if(!evaluateTlsPattern(/(?<=\])[a-z]*?(?=\[)/g, inputLine, false) // between square brackets outside
        && !evaluateTlsPattern(/^[a-z]*?(?=\[)/g, inputLine, false) // begin
        && !evaluateTlsPattern(/(?<=\])[a-z]*?$/g, inputLine, false)) // end
        return false;
    return true;
}

function evaluateTlsPattern(regExp, inputLine, prohibit)
{
    let results = inputLine.match(regExp);
    if(!results)
        return prohibit;
    for(let result of results) {
        for(let i = 0; i <= result.length - 4; ++i) {
            let sub = result.substring(i, i + 4);
            // Found sequence
            if(sub[0] != sub[1] && sub[0] == sub[3] && sub[1] == sub[2])
                return !prohibit;
        }
    }
    return prohibit;
}

function evaluateSsl(inputLine){
    let babPatterns = getAbaBabPatterns(/(?<=\[)[a-z]*?(?=\])/g, inputLine); // between square brackets inside
    if(!babPatterns.length)
        return false;
    let abaPatterns = getAbaBabPatterns(/(?<=\])[a-z]*?(?=\[)/g, inputLine); // between square brackets outside
    abaPatterns = abaPatterns.concat(getAbaBabPatterns(/^[a-z]*?(?=\[)/g, inputLine)); // begin
    abaPatterns = abaPatterns.concat(getAbaBabPatterns(/(?<=\])[a-z]*?$/g, inputLine)); // end
    if(!abaPatterns.length)
        return false;

    for(let bab of babPatterns){
        for(let aba of abaPatterns){
            if(bab[0] == aba[1] && bab[1] == aba[0])
                return true;
        }
    }
    return false;
}

function getAbaBabPatterns(regExp, inputLine)
{
    let patterns = [];
    let results = inputLine.match(regExp);
    if(!results)
        return patterns;
    for(let result of results) {
        for(let i = 0; i <= result.length - 3; ++i) {
            let sub = result.substring(i, i + 3);
            // Found sequence
            if(sub[0] != sub[1] && sub[0] == sub[2])
                patterns.push(sub);
        }
    }
    return patterns;
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    let tlsCount = 0, sslCount = 0;
    for(let inputLine of inputLines){
        if(!inputLine) // null
            continue;
        // Example input: itgslvpxoqqakli[arktzcssgkxktejbno]wsgkbwwtbmfnddt[zblrboqsvezcgfmfvcz]iwyhyatqetsreeyhh
        if(evaluateTls(inputLine))
            tlsCount++;
        if(evaluateSsl(inputLine))
            sslCount++;
    }
    
    output('Day 7', isBrowser);
    output('Question 1: How many IPs in your puzzle input support TLS?', isBrowser);
    output('Answer: ' + tlsCount, isBrowser);
    output('Question 2: How many IPs in your puzzle input support SSL?', isBrowser);
    output('Answer: ' + sslCount, isBrowser);
}
