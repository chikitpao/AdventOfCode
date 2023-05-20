// Day9.java
// AoC 2018 Day 9: Marble Mania
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import java.util.ArrayList;
import java.util.Collections;

public class Day9 {
    static final int PLAYERS_COUNT = 424;

    public static int GetAnswer(int lastMarble) {
        ArrayList<Integer> scores = new ArrayList<>(Collections.nCopies(PLAYERS_COUNT, 0));
        ArrayList<Integer> marbles = new ArrayList<>();
        int currentPosition = 0;
        int currentPlayer = 0;
        for(int i = 0; i <= lastMarble; ++i){
            if(i == 0){
                marbles.add(0);
            } else if(i == 1){
                marbles.add(1);
                currentPosition = 1;
            } else {
                if(i % 23 == 0){
                    int score = i;
                    currentPosition -= 7;
                    if(currentPosition < 0)
                        currentPosition += marbles.size();
                    score += marbles.remove(currentPosition);
                    // System.out.format("## currentPlayer %d currentPosition %d score %d (i %d, j %d)%n",
                    //         currentPlayer, currentPosition, score, i, score - i);
                    scores.set(currentPlayer, scores.get(currentPlayer) + score);
                } else {
                    currentPosition += 2;
                    if(currentPosition > marbles.size())
                        currentPosition = 1;
                    marbles.add(currentPosition, i);
                }
            }
            currentPlayer = (currentPlayer + 1) % PLAYERS_COUNT;
        }
        return Collections.max(scores);
    }

    public static void main(String[] args) {
        System.out.println("Day 9:");
        System.out.println("Question 1: What is the winning Elf's score?");
        System.out.println("Answer: " + GetAnswer(71482));
        //System.out.println("Question 2: What would the new winning Elf's score be if the number of the last marble "
        //        + "were 100 times larger?");
        //System.out.println("Answer: " + GetAnswer(7148200));
    }
}
