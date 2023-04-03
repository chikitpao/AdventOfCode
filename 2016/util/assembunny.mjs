class AssemBunny{
    constructor(){
        this.registers = { ['a']: 0, ['b']: 0, ['c']: 0, ['d']: 0};
        this.lineCount = 0;
        this.currentLine = 0;
        this.buffer = new Array();
        this.check = null;
    }
    reset(){
        this.registers = { ['a']: 0, ['b']: 0, ['c']: 0, ['d']: 0};
        this.lineCount = 0;
        this.currentLine = 0;
        this.output = new Array();
        this.check = null;
    }
    run(inputLines){
        this.lineCount = inputLines.length;
        this.currentLine = 0;
        while(this.#runSingleLine(inputLines));
    }
    // returns true to continue execution, false when finished
    #runSingleLine(inputLines){
        if(this.currentLine >= this.lineCount)
            return false;
        let instruction = inputLines[this.currentLine];
        if(!instruction){
            this.currentLine++;
            return false;
        }
        let result = instruction.match(/cpy (?<src>((-?\d+)|a|b|c|d)) (?<dest>(a|b|c|d))/);
        if(result)
        {
            let src = parseInt(result.groups.src)
            this.registers[result.groups.dest] = (!isNaN(src)) ? src : this.registers[result.groups.src];
            this.currentLine++;
            return true;
        }
        result = instruction.match(/inc (?<arg>(a|b|c|d))/);
        if(result)
        {
            this.registers[result.groups.arg]++;
            this.currentLine++;
            return true;
        }
        result = instruction.match(/dec (?<arg>(a|b|c|d))/);
        if(result)
        {
            this.registers[result.groups.arg]--;
            this.currentLine++;
            return true;
        }
        result = instruction.match(/jnz (?<arg>(-?\d+|a|b|c|d)) (?<offset>-?\d+)/);
        if(result)
        {
            let arg = parseInt(result.groups.arg);
            let value  = (!isNaN(arg)) ? arg : this.registers[result.groups.arg];
            this.currentLine += ((value) ? parseInt(result.groups.offset) : 1);
            return true;
        }
        result = instruction.match(/out (?<arg>(-?\d+|a|b|c|d))/);
        if(result)
        {
            let arg = parseInt(result.groups.arg);
            this.output.push((!isNaN(arg)) ? arg : this.registers[result.groups.arg]);
            if(this.check && this.output.length >= this.check.length){
                return false;
            }
            this.currentLine++;
            return true;
        }
        throw "Unknown instruction: " + instruction;
    }
}

export {AssemBunny};