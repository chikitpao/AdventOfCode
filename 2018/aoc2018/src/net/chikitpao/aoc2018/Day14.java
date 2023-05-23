// Day14.java
// AoC 2018 Day 14: Chocolate Charts
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import net.chikitpao.util.MyLinkedList;

import java.util.ArrayList;
import java.util.LinkedList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class Day14 {

    public static String getAnswer1(int puzzleInput){
        ArrayList<Integer> recipes = new ArrayList<>(puzzleInput + 12);
        int index1 = 0;
        int index2 = 1;
        recipes.add(3);
        recipes.add(7);
        while(recipes.size() < puzzleInput + 10){
            int sum = recipes.get(index1) + recipes.get(index2);
            addRecipes(recipes, sum);
            index1 = updatePosition(recipes, index1);
            index2 = updatePosition(recipes, index2);
        }
        StringBuilder sb = new StringBuilder();
        for(int i = puzzleInput; i < puzzleInput + 10; ++i)
            sb.append(recipes.get(i));
        return sb.toString();
    }

    private static int updatePosition(ArrayList<Integer> recipes, int index) {
        return (index + recipes.get(index) + 1) % recipes.size();
    }

    private static void addRecipes(ArrayList<Integer> recipes, int sum) {
        if(sum >= 10)
            recipes.add(sum / 10);
        recipes.add(sum % 10);
    }

    public static int getAnswer2(int puzzleInput){
        ArrayList<Integer> puzzleInputDigits = new ArrayList<>();
        String puzzleInputString = Integer.toString(puzzleInput);
        for(char c: puzzleInputString.toCharArray())
            puzzleInputDigits.add(c - '0');

        ArrayList<Integer> recipes = new ArrayList<>();
        int currentSearchIndex = 0;
        int index1 = 0;
        int index2 = 1;
        recipes.add(3);
        recipes.add(7);
        while(true){
            int sum = recipes.get(index1) + recipes.get(index2);
            addRecipes(recipes, sum);
            index1 = updatePosition(recipes, index1);
            index2 = updatePosition(recipes, index2);

            if(recipes.size() >= currentSearchIndex + puzzleInputDigits.size()){
                int tempIndex = currentSearchIndex;
                boolean foundPartialMatch = false;
                for(; tempIndex < recipes.size(); ++tempIndex){
                    if(recipes.get(tempIndex) == puzzleInputDigits.get(0)) {
                        int matchCount = 0;
                        for(; matchCount < puzzleInputDigits.size(); ++matchCount){
                            if(tempIndex + matchCount >= recipes.size()){
                                // partial match to the end
                                foundPartialMatch = true;
                                break;
                            }
                            if(recipes.get(tempIndex + matchCount) != puzzleInputDigits.get(matchCount))
                                break;
                        }
                        if(foundPartialMatch){
                            currentSearchIndex = tempIndex;
                            break;
                        }
                        // full match
                        if(matchCount == puzzleInputDigits.size())
                            return tempIndex;
                    }
                }
                if(!foundPartialMatch)
                    currentSearchIndex = recipes.size(); // no match
            }
        }
    }

    public static void main(String[] args) {
        final int PUZZLE_INPUT = 505961;

        System.out.println("Day 14:");
        System.out.println("Question 1: What are the scores of the ten recipes immediately after the number of "
                + "recipes in your puzzle input?");
        System.out.println("Answer: " + getAnswer1(PUZZLE_INPUT));
        System.out.println("Question 2: How many recipes appear on the scoreboard to the left of the score sequence "
                + "in your puzzle input?");
        System.out.println("Answer: " + getAnswer2(PUZZLE_INPUT));
    }
}
