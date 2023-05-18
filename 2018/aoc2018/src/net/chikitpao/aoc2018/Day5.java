// Day5.java
// AoC 2018 Day 5: Alchemical Reduction
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
import java.util.stream.Collectors;

public class Day5 {

    public static boolean canReact(Character a, Character b){
        if(a >= 'a' && a <= 'z')
            return (a - 'a') == (b - 'A');
        else if(a >= 'A' && a <= 'Z')
            return (a - 'A') == (b - 'a');
        return false;
    }
    public static void react(ArrayList<Character> characters){
        boolean finished = true;
        do
        {
            finished = true;
            for(int i = 0; i + 1 < characters.size(); ){
                if(canReact(characters.get(i), characters.get(i + 1))){
                    characters.remove(i);
                    characters.remove(i);
                    // i remains the same
                    finished = false;
                    continue;
                }
                i++;
            }
        } while(!finished);
    }
    public static void remove(ArrayList<Character> characters, Character a){
        Character b;
        if(a >= 'a' && a <= 'z')
            b = Character.toChars(a - 'a' + 'A')[0];
        else if(a >= 'A' && a <= 'Z')
            b = Character.toChars(a - 'A' - 'a')[0];
        else {
            b = a;
        }
        characters.removeIf(c -> (c == a || c == b));
    }
    public static int GetAnswer1(ArrayList<Character> charactersCopy) {
        ArrayList<Character> characters = new ArrayList<>(charactersCopy);
        react(characters);
        return characters.size();
    }
    public static int GetAnswer2(ArrayList<Character> charactersCopy) {
        int min = -1;
        for(char c = 'a'; c <= 'z'; ++c)
        {
            ArrayList<Character> characters = new ArrayList<>(charactersCopy);
            remove(characters, c);
            react(characters);
            if(min == -1)
                min = characters.size();
            else if(min > characters.size())
                min = characters.size();
        }
        return min;
    }
    public static ArrayList<Character> parseInput(String fileName){
        ArrayList<Character> characters = null;
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line = reader.readLine();
            characters = new ArrayList<>(line.chars().mapToObj(v -> ((char) v)).collect(Collectors.toList()));
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        for(int i = 0; i < characters.size(); ++i){
            Character c = characters.get(i);
            if(!Character.isAlphabetic(c))
                System.err.format("Non-alphbetic character %c found at index %d!%n", c, i);
        }
        return characters;
    }
    public static void main(String[] args) {
        Day4 obj = new Day4();
        ArrayList<Character> characters = parseInput("Day5_input.txt");

        System.out.println("Day 5:");
        System.out.println("Question 1: How many units remain after fully reacting the polymer you scanned?");
        System.out.println("Answer: " + GetAnswer1(characters));
        System.out.println("Question 2: What is the length of the shortest polymer you can produce by removing all "
                + "units of exactly one type and fully reacting the result?");
        System.out.println("Answer: " + GetAnswer2(characters));

    }
}
