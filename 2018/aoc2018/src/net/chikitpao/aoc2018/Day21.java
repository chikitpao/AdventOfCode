// Day21.java
// AoC 2018 Day 21: Chronal Conversion
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.atomic.AtomicBoolean;

public class Day21 {
    public static ArrayList<String> parseInput(String fileName){
        ArrayList<String> lines = new ArrayList<>();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                lines.add(line);
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return lines;
    }

    public static void main(String[] args) {
        ArrayList<String> lines = parseInput("Day21_input.txt");
        System.out.println("Day 21:");
        System.out.println("Question 1: What is the lowest non-negative integer value for register 0 that causes "
                + "the program to halt after executing the fewest instructions?");
        ChronalDevice cd = new ChronalDevice();
        cd.parseInput(lines);
        // Debug the program at line 28 "eqrr 1 0 4" and find out what value is in register 1:
        cd.registers[0] = 11285115;
        cd.run();
        System.out.println("Answer: 11285115");
        System.out.println("Question 2: What is the lowest non-negative integer value for register 0 that causes "
                + "the program to halt after executing the most instructions?");
        cd = new ChronalDevice();
        cd.parseInput(lines);
        AtomicBoolean isFirstValue = new AtomicBoolean(true);
        HashSet<Integer> history =  new HashSet<>();
        int[] lastValue = {0};
        cd.run((lineNumber, registers)-> {
            if(lineNumber != 28)
                return true;
            if(isFirstValue.get()) {
                isFirstValue.set(false);
            } else {
                if(history.contains(registers[1]))
                    return false; // found cycle
            }
            lastValue[0] = registers[1];
            history.add(registers[1]);
            return true;
        });
        System.out.println("Answer: " + lastValue[0]);
    }
}
