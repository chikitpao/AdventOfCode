class AssemBunny{
    constructor(){
        this.registers = { ['a']: 0, ['b']: 0, ['c']: 0, ['d']: 0};
        this.lineCount = 0;
        this.currentLine = 0;
        this.buffer = new Array();
        this.check = null;
        this.lineMap = new Map();
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
        // TODO Remove debug code
        if(this.lineMap.has(this.currentLine)){
            this.lineMap.set(this.currentLine, this.lineMap.get(this.currentLine) + 1);
        }else{
            this.lineMap.set(this.currentLine, 1);
        }
        if(instruction.startsWith('cpy ')){
            let result = instruction.match(/cpy (?<src>((-?\d+)|a|b|c|d)) (?<dest>(a|b|c|d))/);
            if(result)
            {
                let src = parseInt(result.groups.src)
                this.registers[result.groups.dest] = (!isNaN(src)) ? src : this.registers[result.groups.src];
                this.currentLine++;
                return true;
            } else {
                // invalid instruction, maybe caused by tgl instruction
                this.currentLine++;
                return true;
            }
        } else if (instruction.startsWith('inc ')) {
            let result = instruction.match(/inc (?<arg>(a|b|c|d))/);
            if (result) {
                this.registers[result.groups.arg]++;
                this.currentLine++;
                return true;
            }else {
                // invalid instruction, maybe caused by tgl instruction
                this.currentLine++;
                return true;
            }
        } else if (instruction.startsWith('dec ')) {
            let result = instruction.match(/dec (?<arg>(a|b|c|d))/);
            if (result) {
                this.registers[result.groups.arg]--;
                this.currentLine++;
                return true;
            }else {
                // invalid instruction, maybe caused by tgl instruction
                this.currentLine++;
                return true;
            }
        } else if(instruction.startsWith('jnz ')){
            let result = instruction.match(/jnz (?<arg>(-?\d+|a|b|c|d)) (?<offset>(-?\d+|a|b|c|d))/);
            if(result)
            {
                let arg = parseInt(result.groups.arg);
                let value  = (!isNaN(arg)) ? arg : this.registers[result.groups.arg];
                let offset = parseInt(result.groups.offset);
                if(isNaN(offset))
                    offset = this.registers[result.groups.offset];
                this.currentLine += ((value) ? offset : 1);
                return true;
            }else {
                // invalid instruction, maybe caused by tgl instruction
                this.currentLine++;
                return true;
            }
        } else if(instruction.startsWith('tgl ')){
            let result = instruction.match(/tgl (?<arg>(-?\d+|a|b|c|d))/);
            if(result)
            {
                let arg = parseInt(result.groups.arg);
                let offset  = (!isNaN(arg)) ? arg : this.registers[result.groups.arg];
                let targetLine = this.currentLine + offset;
                this.#toggleCommand(inputLines, targetLine);
                this.currentLine++;
                return true;
            }
        } else if(instruction.startsWith('out ')) {
            let result = instruction.match(/out (?<arg>(-?\d+|a|b|c|d))/);
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
        }

        throw "Unknown instruction: " + instruction;
    }
    #toggleCommand(inputLines, targetLine){
        if(targetLine < 0 || targetLine >= this.lineCount)
            return false;
        if(inputLines[targetLine].startsWith('inc ')){
            inputLines[targetLine] = inputLines[targetLine].replace('inc ', 'dec ');
        } else if(inputLines[targetLine].startsWith('dec ')
            || inputLines[targetLine].startsWith('tgl ')){
            inputLines[targetLine] = 'inc ' + inputLines[targetLine].substring(4);
        } else if(inputLines[targetLine].startsWith('cpy ')){
            inputLines[targetLine] = inputLines[targetLine].replace('cpy ', 'jnz ');
        } else if(inputLines[targetLine].startsWith('jnz ')){
            inputLines[targetLine] = inputLines[targetLine].replace('jnz ', 'cpy ');
        }
    }
}

export {AssemBunny};