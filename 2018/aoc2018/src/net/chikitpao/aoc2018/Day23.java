// Day23.java
// AoC 2018 Day 23: Experimental Emergency Teleportation
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import net.chikitpao.util.Pair;
import net.chikitpao.util.Pos;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;

public class Day23{
    class Nanobot
    {
        int[] pos = new int[3];
        int radius;
        int[] maxReachable = new int[3];
        int[] minReachable = new int[3];

        Nanobot(int x, int y, int z, int radius){
            pos[0] = x;
            pos[1] = y;
            pos[2] = z;
            this.radius = radius;
            maxReachable[0] = x + radius;
            minReachable[0] = x - radius;
            maxReachable[1] = y + radius;
            minReachable[1] = y - radius;
            maxReachable[2] = z + radius;
            minReachable[2] = z - radius;
        }
        static int getDistance3D(Nanobot lhs, Nanobot rhs){
            return Math.abs(lhs.pos[0] - rhs.pos[0]) + Math.abs(lhs.pos[1] - rhs.pos[1])
                    + Math.abs(lhs.pos[2] - rhs.pos[2]);
        }
        static int getDistance3D(Nanobot bot, int[] pos){
            return Math.abs(bot.pos[0] - pos[0]) + Math.abs(bot.pos[1] - pos[1]) + Math.abs(bot.pos[2] - pos[2]);
        }
        static int getDistance3D(int[] lhs, int[] rhs){
            return Math.abs(lhs[0] - rhs[0]) + Math.abs(lhs[1] - rhs[1]) + Math.abs(lhs[2] - rhs[2]);
        }
    }
    static ArrayList<Nanobot> nanobots = new ArrayList<>();

    public static void parseInput(Day23 obj, String fileName){
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if(line.isEmpty())
                    continue;
                // Example line:
                // pos=<-3079732,26664397,4300940>, r=64337129
                Pattern pattern = Pattern.compile("pos=<(-?\\d+),(-?\\d+),(-?\\d+)>, r=(\\d+)", 0);
                Matcher matcher = pattern.matcher(line);
                if(matcher.matches()){
                    nanobots.add(obj.new Nanobot(Integer.parseInt(matcher.group(1)), Integer.parseInt(matcher.group(2)),
                            Integer.parseInt(matcher.group(3)), Integer.parseInt(matcher.group(4))));
                }
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
    }

    private static long getAnswer1() {
        Nanobot maxNanobot = Collections.max(nanobots, Comparator.comparing(e -> e.radius));
        return nanobots.stream().filter(e -> Nanobot.getDistance3D(maxNanobot, e) <= maxNanobot.radius).count();
    }

    private static Pair<Integer, Integer> findExtrema(int index) {
        // 1. Find minimum and maximum z coordinates.
        Nanobot maxBot = Collections.max(nanobots, Comparator.comparing(e -> e.maxReachable[index]));
        Nanobot minBot = Collections.min(nanobots, Comparator.comparing(e -> e.minReachable[index]));
        return new Pair<>(minBot.minReachable[index], maxBot.maxReachable[index]);
    }
    private static HashSet<Integer> findRelevantPlanesSet(int index) {
        HashSet<Integer> relevantPlanesSet = new HashSet<>();
        for(Nanobot bot: nanobots){
            relevantPlanesSet.add(bot.minReachable[index] - 1);
            relevantPlanesSet.add(bot.minReachable[index]);
            relevantPlanesSet.add(bot.maxReachable[index]);
            relevantPlanesSet.add(bot.maxReachable[index] + 1);
        }
        return relevantPlanesSet;
    }

    private static long getAnswer2() {
        ArrayList<Pair<Integer, Integer>> extrema = new ArrayList<>();
        ArrayList<HashSet<Integer>> relevantPlanesSets = new ArrayList<>();
        for(int i = 0; i < 3; ++i){
            // 1. Find minimum and maximum z coordinates.
            Pair<Integer, Integer> axisExtrema = findExtrema(i);
            extrema.add(axisExtrema);
            // 2. Collect "relevant" coordinates (where coordinate becomes reachable / unreachable by a nanobot).
            HashSet<Integer> relevantPlanesSet = findRelevantPlanesSet(i);
            relevantPlanesSets.add(relevantPlanesSet);
            // Output:
            // ## index 0 min -211915514 max 264380721
            // ## index 0 relevantPlanesSet.size() 4000
            // ## index 1 min -284459723 max 208522400
            // ## index 1 relevantPlanesSet.size() 4000
            // ## index 2 min -164183940 max 194089849
            // ## index 2 relevantPlanesSet.size() 4000
            System.out.format("## index %d min %d max %d%n", i, axisExtrema.first, axisExtrema.second);
            System.out.format("## index %d relevantPlanesSet.size() %d%n", i, relevantPlanesSet.size());
        }

        // 3. Unfortunately, I got stuck here.
        // The solution below is implemented by Reddit user fhinkel. The idea is to divide the search area size by two
        // successively and keep the grid with the most count.
        // https://www.reddit.com/r/adventofcode/comments/a8s17l/2018_day_23_solutions/
        // https://github.com/fhinkel/AdventOfCode2018/blob/master/day23.js
        int[] min = new int[]{extrema.get(0).first, extrema.get(1).first, extrema.get(2).first};
        int[] max = new int[]{extrema.get(0).second, extrema.get(1).second, extrema.get(2).second};
        int gridSize = max[0] - min[0];
        int[] bestGridPos = null;
        int[] originPos = {0, 0, 0};
        while(gridSize > 0){
            int maxCount = 0;
            for(int i = min[0]; i < max[0]; i += gridSize){
                for(int j = min[1]; j < max[1]; j += gridSize){
                    for(int k = min[2]; k < max[2]; k += gridSize){
                        int count = 0;
                        int[] gridPos = {i, j, k};
                        for(Nanobot bot: nanobots){
                            if(Nanobot.getDistance3D(bot, gridPos) - bot.radius < gridSize)
                                count++;
                        }
                        if (maxCount < count) {
                            maxCount = count;
                            bestGridPos = Arrays.copyOf(gridPos, gridPos.length);
                        } else if (maxCount == count) {
                            if ((bestGridPos == null)
                                    || (Nanobot.getDistance3D(gridPos, originPos)
                                    < Nanobot.getDistance3D(bestGridPos, originPos))) {
                                bestGridPos = Arrays.copyOf(gridPos, gridPos.length);
                            }
                        }
                    }
                }
            }
            for (int i = 0; i < 3; i++) {
                min[i] = bestGridPos[i] - gridSize;
                max[i] = bestGridPos[i] + gridSize;
            }
            gridSize /= 2;
        }
        return Nanobot.getDistance3D(bestGridPos, originPos);
    }

    public static void main(String[] args) throws Exception {
        Day23 obj = new Day23();
        parseInput(obj, "Day23_input.txt");
        System.out.println("Day 23:");
        System.out.println("Question 1: Find the nanobot with the largest signal radius. How many nanobots are in "
                + "range of its signals?");
        System.out.println("Answer: " + getAnswer1());
        System.out.println("Question 2: Find the coordinates that are in range of the largest number of nanobots. "
                + "What is the shortest manhattan distance between any of those points and 0,0,0?");
        System.out.println("Answer: " + getAnswer2());
    }
}
