// Day9.java
// AoC 2018 Day 9: Marble Mania
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import java.util.ArrayList;
import java.util.Collections;

import net.chikitpao.util.MyLinkedList;

public class Day9 {

    public static long GetAnswer(int playersCount, int lastMarble) {
        ArrayList<Long> scores = new ArrayList<>(Collections.nCopies(playersCount, 0l));
        MyLinkedList<Integer> marbles = new MyLinkedList<>();
        // ListIterator is a struggle and doesn't fit the current task (Task requires moving iterator forward and
        // backward, iterator in front of value, etc..)
        // So I've implemented a new linked list instead.
        MyLinkedList<Integer>.Iterator<Integer> iterator = marbles.begin();
        int currentPlayer = 0;
        for(int i = 0; i <= lastMarble; ++i){
            if(i == 0){
                iterator.add(0);
            } else if(i == 1){
                iterator.next();
                iterator.add(1);
            } else {
                if(i % 23 == 0){
                    long score = i;
                    iterator.move(-7, true);
                    score += iterator.get();
                    iterator.remove();
                    if(scores.get(currentPlayer) + score < scores.get(currentPlayer))
                        throw new RuntimeException();
                    scores.set(currentPlayer, scores.get(currentPlayer) + score);
                } else {
                    iterator.move(2, true);
                    iterator.add(i);
                }
            }
            currentPlayer = (currentPlayer + 1) % playersCount;
        }
        return Collections.max(scores);
    }

    public static void main(String[] args) {
        System.out.println("Day 9:");
        System.out.println("Question 1: What is the winning Elf's score?");
        System.out.println("Answer: " + GetAnswer(424, 71482));
        System.out.println("Question 2: What would the new winning Elf's score be if the number of the last marble "
                + "were 100 times larger?");;
        System.out.println("Answer: " + GetAnswer(424, 7148200));
    }
}
