// ChronalDevice.java
// AoC 2018
// Author: Chi-Kit Pao
//
// Used by Day16

package net.chikitpao.aoc2018;

import java.util.ArrayList;
import java.util.HashMap;

public class ChronalDevice {
    interface Operation {
        int execute(int[] vec3, int[] regs);
    }
    public class Instruction{
        public ChronalDevice device;
        public String name;
        public int opCode = -1;
        public Operation op;
        public Instruction(ChronalDevice device, String name, Operation op) {
            this.device = device;
            this.name = name;
            this.op = op;
        }
        public void execute(int[] vec3){
            execute(vec3, this.device.registers);
        }
        public void execute(int[] vec3, int[] regs){
            op.execute(vec3, regs);
        }
    }
    public int[] registers = {0, 0, 0, 0};
    public ArrayList<Instruction> instructions = new ArrayList<>();
    public HashMap<String, Instruction> instructionsMap = new HashMap<>();

    public ChronalDevice(){
        instructions.add(new Instruction(this, "addr",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] + regs[opArgs[1]]));
        instructions.add(new Instruction(this, "addi",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] + opArgs[1]));
        instructions.add(new Instruction(this, "mulr",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] * regs[opArgs[1]]));
        instructions.add(new Instruction(this, "muli",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] * opArgs[1]));
        instructions.add(new Instruction(this, "banr",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] & regs[opArgs[1]]));
        instructions.add(new Instruction(this, "bani",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] & opArgs[1]));
        instructions.add(new Instruction(this, "borr",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] | regs[opArgs[1]]));
        instructions.add(new Instruction(this, "bori",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]] | opArgs[1]));
        instructions.add(new Instruction(this, "setr",
                (opArgs, regs) -> regs[opArgs[2]] = regs[opArgs[0]]));
        instructions.add(new Instruction(this, "seti",
                (opArgs, regs) -> regs[opArgs[2]] = opArgs[0]));
        instructions.add(new Instruction(this, "gtir",
                (opArgs, regs) -> regs[opArgs[2]] = (opArgs[0] > regs[opArgs[1]]) ? 1 : 0));
        instructions.add(new Instruction(this, "gtri",
                (opArgs, regs) -> regs[opArgs[2]] = (regs[opArgs[0]] > opArgs[1]) ? 1 : 0));
        instructions.add(new Instruction(this, "gtrr",
                (opArgs, regs) -> regs[opArgs[2]] = (regs[opArgs[0]] > regs[opArgs[1]]) ? 1 : 0));
        instructions.add(new Instruction(this, "eqir",
                (opArgs, regs) -> regs[opArgs[2]] = (opArgs[0] == regs[opArgs[1]]) ? 1 : 0));
        instructions.add(new Instruction(this, "eqri",
                (opArgs, regs) -> regs[opArgs[2]] = (regs[opArgs[0]] == opArgs[1]) ? 1 : 0));
        instructions.add(new Instruction(this, "eqrr",
                (opArgs, regs) -> regs[opArgs[2]] = (regs[opArgs[0]] == regs[opArgs[1]]) ? 1 : 0));
        instructions.forEach(e -> instructionsMap.put(e.name, e));
    }
    public Instruction getInstruction(String instr){
        return instructionsMap.get(instr);
    }
    public ArrayList<Instruction> getInstructions(){
        return instructions;
    }
}
