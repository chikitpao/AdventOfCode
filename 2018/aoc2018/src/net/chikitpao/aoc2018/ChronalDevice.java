// ChronalDevice.java
// AoC 2018
// Author: Chi-Kit Pao
//
// Used by Day16

package net.chikitpao.aoc2018;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class ChronalDevice {
    interface Operation {
        int execute(int[] vec3, int[] regs);
    }
    public class Instruction{
        public ChronalDevice device;
        public String name;
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
    public class CodeLine{
        public String instr;
        public int[] arg3;
        public CodeLine(String instr, int arg3[]){
            this.instr = instr;
            this.arg3 = arg3;
        }
    }
    // Day 16 needs 4 registers. Day 19 needs 6 registers.
    public int[] registers = {0, 0, 0, 0, 0, 0};
    public int ipRegister = -1;    // Instruction Pointer register index (Day 19)
    private ArrayList<CodeLine> codeLines = new ArrayList<>();    // Code lines (Day 19)
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
    public void parseInput(ArrayList<String> lines) {
        Iterator<String> iterator = lines.iterator();
//       // Example lines
        // #ip 2
        // seti 123 0 1
        // ..
        Pattern pattern1 = Pattern.compile("#ip (\\d)", 0);
        Matcher matcher = pattern1.matcher(iterator.next());
        if(matcher.matches())
            ipRegister = Integer.parseInt(matcher.group(1));

        Pattern pattern2 = Pattern.compile("(\\w+) (\\d+) (\\d+) (\\d+)", 0);
        while(iterator.hasNext()){
            matcher = pattern2.matcher(iterator.next());
            if (matcher.matches()) {
                int[] args = new int[]{Integer.parseInt(matcher.group(2)), Integer.parseInt(matcher.group(3)),
                        Integer.parseInt(matcher.group(4))};
                codeLines.add(new CodeLine(matcher.group(1), args));
            }
        }
    }

    public void run() {
        while(true){
            int currentLine = registers[ipRegister];
            if(currentLine < 0 || currentLine >= codeLines.size())
                return;
            CodeLine codeLine = codeLines.get(currentLine);
            instructionsMap.get(codeLine.instr).execute(codeLine.arg3);
            registers[ipRegister]++;
        }
    }
}
