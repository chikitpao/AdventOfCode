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
    output('Day 5', isBrowser);
    let errorMsg = "Error: It seems that MD5 is not supported by Web API anymore!";
    output(errorMsg, isBrowser);
    throw errorMsg;
}

async function getNodeJsCryptoModule() {
    const crypto = await import('crypto');
    return crypto;
}

function findPassword(start, doorId, createHash)
{
    let test_var = 0;
    let password = '';
    while(true){
        let data = doorId + test_var.toString();
        let hexdigest = createHash(data);
        if(hexdigest.startsWith(start)) {
            password += hexdigest[start.length];
            if(password.length >= 8)
                return password;
        }
        test_var += 1;
    }
}

function findPasswordV2(start, doorId, createHash)
{
    let test_var = 1
    let password = 'XXXXXXXX';
    let filled = 0;
    while(true){
        let data = doorId + test_var.toString();
        let hexdigest = createHash(data);
        if(hexdigest.startsWith(start)) {
            let pos = parseInt(hexdigest[start.length]);
            if(!isNaN(pos)){
                let value = hexdigest[start.length +1];
                if(pos < 8 && password[pos] == 'X') {
                    password = password.substring(0, pos) + value + password.substring(pos+1);
                    filled++;
                    if(filled >= 8)
                        return password;
                }
            }
        }
        test_var += 1
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

    const doorId = 'cxdnnyjw';
    output('Day 5', isBrowser);
    output('Question 1: Given the actual Door ID, what is the password?', isBrowser);
    output('Answer: ' + findPassword('00000', doorId, createHash), isBrowser);
    output('Question 2: Given the actual Door ID and this new method, what is the password?', isBrowser);
    output('Answer: ' + findPasswordV2('00000', doorId, createHash), isBrowser);

    const end = Date.now();
    output(`Execution time: ${end - start} ms`);
}
