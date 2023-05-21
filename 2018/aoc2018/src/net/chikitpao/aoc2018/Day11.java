// Day11.java
// AoC 2018 Day 11: Chronal Charge
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import net.chikitpao.util.Pair;
import net.chikitpao.util.Pos;

import java.util.ArrayList;
import java.util.Collections;

public class Day11 {

    static int GRID_SIZE = 300;
    public static int calculatePowerLevel(int x, int y, int puzzleInput){
        // Find the fuel cell's rack ID, which is its X coordinate plus 10.
        int rackID = x + 10;
        // Begin with a power level of the rack ID times the Y coordinate.
        // Increase the power level by the value of the grid serial number (your puzzle input).
        int powerLevel = rackID * y + puzzleInput;
        // Set the power level to itself multiplied by the rack ID.
        powerLevel *= rackID;
        // Keep only the hundreds digit of the power level (so 12345 becomes 3; numbers with no hundreds digit become 0).
        int digit = (powerLevel / 100) % 10;
        // Subtract 5 from the power level.
        return digit - 5;
    }

    public static ArrayList<ArrayList<Integer>> calculateGrid(int puzzleInput){
        ArrayList<ArrayList<Integer>> rows = new ArrayList<>(GRID_SIZE);
        for(int i = 0; i < GRID_SIZE; ++i) {
            ArrayList<Integer> normalRow = new ArrayList<>(Collections.nCopies(GRID_SIZE, 0));
            int y = i + 1;
            for(int j = 0; j < GRID_SIZE; ++j) {
                int x = j + 1;
                normalRow.set(j, Day11.calculatePowerLevel(x, y, puzzleInput));
            }
            rows.add(normalRow);
        }
        return rows;
    }
    public static Pair<Pos, Integer> calculateMaximumSquare(ArrayList<ArrayList<Integer>> grid, int squareSize) {
        ArrayList<ArrayList<Integer>> rows = new ArrayList<>(GRID_SIZE);
        for(int i = 0; i < GRID_SIZE; ++i) {
            ArrayList<Integer> normalRow = grid.get(i);
            ArrayList<Integer> rowWithSums = new ArrayList<>(GRID_SIZE - squareSize + 1);
            for(int j = 0; j < GRID_SIZE - squareSize + 1; ++j) {
                int sum = 0;
                for(int k = 0; k < squareSize; ++k)
                    sum += normalRow.get(j + k);
                rowWithSums.add(sum);
            }
            rows.add(rowWithSums);
        }
        int maxI = -1;
        int maxJ = -1;
        int max = 0;
        for(int i = 0; i < GRID_SIZE - squareSize + 1; ++i) {
            for(int j = 0; j < GRID_SIZE - squareSize + 1; ++j) {
                int sum = 0;
                for(int k = 0; k < squareSize; ++k)
                    sum += rows.get(i + k).get(j);
                if((maxI == -1) || (sum > max)) {
                    max = sum;
                    maxI = i;
                    maxJ = j;
                }
            }
        }
        return new Pair(new Pos(maxI + 1, maxJ + 1), max);
    }

    private static  Pair <Pos, Integer> getAnswer2(ArrayList<ArrayList<Integer>> grid) {
        Pair <Pos, Integer> maxSquare = null;
        int maxSquareSize = 0;
        for(int i = 1; i <= 300; ++i) {
            Pair<Pos, Integer> square = calculateMaximumSquare(grid, i);
            if((maxSquare == null) || (square.second > maxSquare.second)) {
                maxSquareSize = i;
                maxSquare = square;
            }
        }
        return new Pair<>(maxSquare.first, maxSquareSize);
    }

    public static void main(String[] args) {
        final int PUZZLE_INPUT = 8561;
        ArrayList<ArrayList<Integer>>  grid = calculateGrid(PUZZLE_INPUT);

        System.out.println("Day 11:");
        System.out.println("Question 1: What is the X,Y coordinate of the top-left fuel cell of the 3x3 square with the largest total power?");
        Pair<Pos, Integer> answer1 = calculateMaximumSquare(grid, 3);
        System.out.println("Answer: " + answer1.first.x + "," + answer1.first.y);
        System.out.println("Question 2: What is the X,Y,size identifier of the square with the largest total power?");
        Pair<Pos, Integer> answer2 = getAnswer2(grid);
        System.out.println("Answer: " + answer2.first.x + "," + answer2.first.y + "," + answer2.second);
    }


}
