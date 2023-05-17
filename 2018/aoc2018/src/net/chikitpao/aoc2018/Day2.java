// Day2.java
// AoC 2018 Day 2: Inventory Management System
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
import java.util.Arrays;

public class Day2 {
    public static int GetAnswer1(ArrayList<String> boxIDs) {
        int twoLettersCount = 0;
        int threeLettersCount = 0;
        for(String s: boxIDs) {
            char[] chars = s.toCharArray();
            Arrays.sort(chars);
            char currentChar = '!'; // just an invalid char
            int currentLetterCount = 0;
            boolean hasTwoLetters = false;
            boolean hasThreeLetters = false;
            for(int i = 0; i < chars.length; ++i) {
                if(chars[i] == currentChar) {
                    currentLetterCount++;
                    continue;
                }
                if (currentLetterCount == 2)
                    hasTwoLetters = true;
                else if (currentLetterCount == 3)
                    hasThreeLetters = true;

                currentChar = chars[i];
                currentLetterCount = 1;
                if(hasTwoLetters && hasThreeLetters)
                    break;
            }
            if(hasTwoLetters)
                twoLettersCount++;
            if(hasThreeLetters)
                threeLettersCount++;
        }
        return twoLettersCount * threeLettersCount;
    }
    public static String GetAnswer2(ArrayList<String> boxIDs) {
        for(int i = 0; i< boxIDs.size(); ++i){
            String lhs = boxIDs.get(i);
            for(int j = i + 1; j < boxIDs.size(); ++j){
                String rhs = boxIDs.get(j);
                boolean foundPair = true;
                boolean foundDifference = false;
                ArrayList<Character> result = new ArrayList<>();
                for(int k = 0; k < lhs.length(); ++k) {
                    // Use codePointAt since char is unsigned
                    int diff = Math.abs(lhs.codePointAt(k) - rhs.codePointAt(k));
                    if(diff != 0) {
                        if(foundDifference) {
                            foundPair = false;
                            break;
                        }
                        foundDifference = true;
                    } else {
                        result.add(lhs.charAt(k));
                    }
                }
                if(foundPair) {
                    StringBuilder builder = new StringBuilder(result.size());
                    result.forEach(builder::append);
                    return builder.toString();
                }
            }
        }

        return "";
    }
    public static void main(String[] args) {
        ArrayList<String> boxIDs = new ArrayList<>();

        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", "Day2_input.txt");
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                boxIDs.add(line);
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }

        System.out.println("Day 2:");
        System.out.println("Question 1: What is the checksum for your list of box IDs?");
        System.out.println("Answer: " + GetAnswer1(boxIDs));
        System.out.println("Question 2: What letters are common between the two correct box IDs?");
        System.out.println("Answer: " + GetAnswer2(boxIDs));
    }
}
