// Day1.java
// AoC 2018 Day 1: Chronal Calibration
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Optional;

public class Day1 {
    public static int GetAnswer1(ArrayList<Integer> changes){
        return changes.stream().reduce(0, Integer::sum);
    }
    public static int GetAnswer2(ArrayList<Integer> changes){
        Optional<Integer> answer2 = Optional.empty();
        int frequency = 0;
        HashSet<Integer> knownFrequencies = new HashSet<>();
        knownFrequencies.add(0);
        while(true) {
            for (Integer change : changes) {
                frequency += change;
                if (answer2.isEmpty() && knownFrequencies.contains(frequency)) {
                    answer2 = Optional.of(frequency);
                    return answer2.get();
                }
                knownFrequencies.add(frequency);
            }
        }
    }
    public static void main(String[] args) {
        ArrayList<Integer> changes = new ArrayList<>();

        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", "Day1_input.txt");
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                changes.add(Integer.parseInt(line));
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }

        System.out.println("Day 1:");
        System.out.println("Question 1: Starting with a frequency of zero, what is the resulting frequency after all "
                + "of the changes in frequency have been applied?");
        System.out.println("Answer: " + GetAnswer1(changes));
        System.out.println("Question 2: What is the first frequency your device reaches twice?");
        System.out.println("Answer: " + GetAnswer2(changes));
    }
}
