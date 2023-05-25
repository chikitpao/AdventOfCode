// Day15Tests.java
// AoC 2018 Day 15: Beverage Bandits
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import java.util.ArrayList;
import java.util.Arrays;

public class Day15Tests {

    static String str0 = "#######\n#.G...#\n#...EG#\n#.#.#G#\n#..G#E#\n#.....#\n#######\n";
    static String str1 = "#######\n#G..#E#\n#E#E.E#\n#G.##.#\n#...#E#\n#...E.#\n#######\n";
    static String str2 = "#######\n#E..EG#\n#.#G.E#\n#E.##E#\n#G..#.#\n#..E#.#\n#######\n";
    static String str3 = "#######\n#E.G#.#\n#.#G..#\n#G.#.G#\n#G..#.#\n#...E.#\n#######\n";
    static String str4 = "#######\n#.E...#\n#.#..G#\n#.###.#\n#E#G#G#\n#...#G#\n#######\n";
    static  String str5 = "#########\n#G......#\n#.E.#...#\n#..##..G#\n#...##..#\n#...#...#\n#.G...G.#\n#.....G.#\n#########\n";


    public static String puzzles[] = {str0, str1, str2, str3, str4, str5};
    public static int answers[] = {27730, 36334, 39514, 27755, 28944, 18740};

    public static ArrayList<String> getPuzzle(int index){
        return new ArrayList<>(Arrays.stream(puzzles[index].split("\n")).toList());
    }
}
