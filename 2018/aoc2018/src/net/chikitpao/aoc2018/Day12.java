// Day12.java
// AoC 2018 Day 12: Subterranean Sustainability
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class Day12 {
    class Cave{
        private ArrayList<Boolean> potsRight = new ArrayList<>(); // 0, 1, 2, ...
        private ArrayList<Boolean> potsLeft = new ArrayList<>(); // -1, -2, ...
        private ArrayList<Boolean> initPotsRight;

        HashMap<String, String> rules = new HashMap<>();
        public void parseRule(String input, String output) {
            rules.put(input, output);
            // Output (max. growth rate to the right: 1):
            // ...## #
            if((input.startsWith("...") || input.startsWith("....") || input.startsWith("....."))
                    && output.charAt(0) == '#')
                System.out.println(input + ' ' + output);
            // Output (max. growth rate to the left: 1):
            // .#... #
            // ##... #
            if((input.endsWith("...") || input.endsWith("....")) && output.charAt(0) == '#')
                System.out.println(input + ' ' + output);
        }

        public void parseInitialState(String string) {
            potsRight.ensureCapacity(string.length());
            for(int i = 0; i < string.length(); ++i)
                potsRight.add(string.charAt(i) == '#');
            initPotsRight = new ArrayList<>(potsRight);
        }

        public String getPattern(int pos){
            StringBuilder sb = new StringBuilder();
            for(int i = pos - 2; i <= pos + 2; ++i){
                if(i < 0) {
                    if(-i - 1 >= potsLeft.size())
                        sb.append('.');
                    else
                        sb.append(potsLeft.get(-i - 1) ? '#' : '.');
                } else {
                    if(i >= potsRight.size())
                        sb.append('.');
                    else
                        sb.append(potsRight.get(i) ? '#' : '.');
                }
            }
            return sb.toString();
        }
        public long getWeightedSum() {
            long sum = 0;
            for(int i = 0; i < potsLeft.size(); ++i){
                if(potsLeft.get(i))
                    sum += -i - 1;
            }
            for(int i = 0; i < potsRight.size(); ++i){
                if(potsRight.get(i))
                    sum += i;
            }
            return sum;
        }

        public void processStep() {
            ArrayList<Boolean> newPotsLeft = new ArrayList<>();
            for(int i = 0; i < potsLeft.size(); ++i)
                newPotsLeft.add(rules.get(getPattern(-i - 1)).equals("#"));
            if(rules.get(getPattern(-potsLeft.size() - 1)).equals("#"))
                newPotsLeft.add(true);

            ArrayList<Boolean> newPotsRight = new ArrayList<>();
            for(int i = 0; i < potsRight.size(); ++i)
                newPotsRight.add(rules.get(getPattern(i)).equals("#"));
            if(rules.get(getPattern(potsRight.size())).equals("#"))
                newPotsRight.add(true);

            potsLeft = newPotsLeft;
            potsRight = newPotsRight;
        }

        public void resetPots() {
            potsRight = new ArrayList<>(initPotsRight);
            potsLeft = new ArrayList<>();
        }
    }
    public static long getAnswer1(Cave cave) {
        for(int i = 0; i < 20; ++i)
            cave.processStep();
        return cave.getWeightedSum();
    }
    public static long getAnswer2(Cave cave) {
        cave.resetPots();
        long previousSum = cave.getWeightedSum();
        long currentSum = previousSum;
        // Output:
        // ## Initial: sum 1997
        System.out.format("## Initial: sum %d%n", currentSum);
        for(long i = 0; i < 1000; ++i){
            cave.processStep();
            currentSum = cave.getWeightedSum();
            // Output:
            // ## i <i>: sum <sum> delta <delta>
            // Beginning with i = 162, delta is always 23:
            // ## i 162l: sum 4107 delta 23
            System.out.format("## i %dl: sum %d delta %d%n", i, currentSum, currentSum - previousSum);
            previousSum = currentSum;
        }
        // Formula for i > 162 (with i = n - 1):
        // result = 4107 + (i - 162) * 23
        long n = 50000000000l;
        long i = n - 1;
        return 4107l + (i - 162l) * 23l;
    }

    public static Cave parseInput(Day12 obj, String fileName){
        Cave cave = obj.new Cave();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            // Example line:
            // initial state: ##..#.#.#..##..#..##..##..#.#....#.....##.#########...#.#..#..#....#.###.###....#..........###.#.#..
            line = reader.readLine();
            final String begin = "initial state: ";
            int pos = line.indexOf(begin);
            if(pos == -1)
                throw new RuntimeException("Invalid input!");
            cave.parseInitialState(line.substring(pos));

            reader.readLine(); // empty line

            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                // Example line:
                // ..##. => .
                Pattern pattern = Pattern.compile("([#.]+) => ([#.])", 0);
                Matcher matcher = pattern.matcher(line);
                if(!matcher.matches())
                    throw new RuntimeException("Cannot parse line");
                cave.parseRule(matcher.group(1), matcher.group(2));
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return cave;
    }

    public static void main(String[] args) {
        Day12 obj = new Day12();
        System.out.println("Day 12:");
        Cave cave = parseInput(obj, "Day12_input.txt");
        System.out.println("Question 1: After 20 generations, what is the sum of the numbers of all pots which "
                 + "contain a plant?");
        System.out.println("Answer: " + getAnswer1(cave));
        System.out.println("Question 2: After fifty billion (50000000000) generations, what is the sum "
                + "of the numbers of all pots which contain a plant?");
        System.out.println("Answer: " + getAnswer2(cave));
    }
}
