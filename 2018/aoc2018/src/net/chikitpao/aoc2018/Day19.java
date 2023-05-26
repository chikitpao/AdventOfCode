// Day1.java
// AoC 2018 Day 19: Go With The Flow
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

public class Day19 {
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

    private static int getAnswer2() {
        // r[0] accumulates factors of r[2] (10.551.376), inclusive 1 and 10.551.376
        // See Day19_input_description.txt for description.
        int r0 = 0;
        int r2 = 10551376;
        for(int r1 = 1; r1 <= r2; ++r1){
            if((r2 % r1) == 0)
                r0 += r1;
        }
        return r0;
    }
    public static void main(String[] args) {
        ArrayList<String> lines = parseInput("Day19_input.txt");
        System.out.println("Day 19:");
        System.out.println("Question 1: What value is left in register 0 when the background process halts?");
        ChronalDevice cd = new ChronalDevice();
        cd.parseInput(lines);
        cd.run();
        System.out.println("Answer: " + cd.registers[0]);
        System.out.println("Question 2: What value is left in register 0 when this new background process halts?");
        cd = new ChronalDevice();
        cd.parseInput(lines);
        cd.registers[0] = 1;
        // These two lines would take too long!
        // cd.run();
        // System.out.println("Answer: " + cd.registers[0]);
        System.out.println("Answer: " + getAnswer2());
    }
}
