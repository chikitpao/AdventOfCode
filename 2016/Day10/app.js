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


class Bot{
    static robotsMap = new Map();
    static botV1 = null;
    constructor(id){
        this.id = id;
        this.input = new Array(2); // ok since no value 0
        this.lowOut = null;
        this.highOut = null;
    }
    receive(value){
        if(!this.input[0])
            this.input[0] = value;
        else if(!this.input[1])
            this.input[1] = value;
        else
            throw 'Bot' + this.id + ' cannot receive value ' + value + '!';
    }
    tryTransmit(){
        if(!this.input[0] || !this.input[1])
            return false;
        
        if(this.input[0] < this.input[1]){
            if(this.input[0] == 17 && this.input[1] == 61)
                Bot.botV1 = this;
            this.lowOut.receive(this.input[0]);
            this.highOut.receive(this.input[1]);
            this.input[0] = this.input[1] = null; 
        } else {
            if(this.input[0] == 61 && this.input[1] == 17)
                Bot.botV1 = this;
            this.lowOut.receive(this.input[1]);
            this.highOut.receive(this.input[0]);
            this.input[0] = this.input[1] = null;
        }
        return true;
    }
    static getOrCreate(id){
        if(this.robotsMap.has(id))
            return this.robotsMap.get(id);
        let bot = new Bot(id);
        this.robotsMap.set(id, bot);
        return bot;
    }
}

class Output{
    static outputsMap = new Map();
    constructor(id){
        this.id = id;
        this.value = null;  // ok since no value 0
    }
    receive(value){
        if(!this.value)
            this.value = value;
        else
            throw 'Output' + this.id + ' cannot receive value ' + value + '!';
    }
    static getOrCreate(id){
        if(this.outputsMap.has(id))
            return this.outputsMap.get(id);
        let out = new Output(id);
        this.outputsMap.set(id, out);
        return out;
    }
}

function processInput(input, isBrowser){
    const inputLines = input.split(/\r?\n/);
    // Example lines:
    // value 61 goes to bot 209
    // bot 200 gives low to bot 40 and high to bot 141
    // bot 194 gives low to output 9 and high to bot 74
    // bot 169 gives low to output 19 and high to output 11
    let giveInstructions = new Array();
    for(let inputLine of inputLines){
        if(!inputLine)
            continue;
        if(inputLine.startsWith('value')){
            let found = inputLine.match(/value (?<value>\d+) goes to bot (?<bot>\d+)/);
            let botId = parseInt(found.groups.bot);
            let bot = Bot.getOrCreate(botId);
            bot.receive(parseInt(found.groups.value))
            //console.log(found.groups);
        } else if (inputLine.startsWith('bot')){
            let found = inputLine.match(/bot (?<bot>\d+) gives low to (?<output1>(bot \d+|output \d+)) and high to (?<output2>(bot \d+|output \d+))/);
            giveInstructions.push([parseInt(found.groups.bot), found.groups.output1, found.groups.output2]);
        }
        else
            throw 'Unknown input: ' + inputLine;
    }
    for(const instruction of giveInstructions){
        let botId = instruction[0];
        let bot = Bot.getOrCreate(botId);
        
        let connectOutput = function(out, ){
            if(out.startsWith('bot ')){
                return Bot.getOrCreate(parseInt(out.substring(4)));
            } else if (out.startsWith('output ')){
                return Output.getOrCreate(parseInt(out.substring(7)));
            } else {
                throw "Unknown bot output" + out;
            }
        }
        bot.lowOut = connectOutput(instruction[1]);
        bot.highOut = connectOutput(instruction[2]);
    }

    while(true){
        let doneSomething = false;
        for(let bot of Bot.robotsMap.values()){
            if(bot.tryTransmit())
                doneSomething = true;
        }
        if(!doneSomething)
            break;
    }

    let answer2 = Output.outputsMap.get(0).value * Output.outputsMap.get(1).value * Output.outputsMap.get(2).value;
    
    output('Day 10', isBrowser);
    output('Question 1: Based on your instructions, what is the number of the bot that is responsible for comparing value-61 microchips with value-17 microchips?', isBrowser);
    output('Answer: Bot with number ' + Bot.botV1.id, isBrowser);
    output('Question 2: What do you get if you multiply together the values of one chip in each of outputs 0, 1, and 2?', isBrowser);
    output('Answer: ' + answer2, isBrowser);
}
