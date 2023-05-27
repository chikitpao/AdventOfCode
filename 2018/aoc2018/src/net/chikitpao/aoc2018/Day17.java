// Day17.java
// AoC 2018 Day 17: Reservoir Research
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
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;


public class Day17 {
    class Reservoir{
        private HashSet<Pos> clayPositions = new HashSet<>();
        private Pos source = new Pos(0, 500);
        private Pos minPos;
        private Pos maxPos;
        private int inputMinY = 0;
        private int inputMaxY = 0;
        private int width = 0;
        private int height = 0;
        private Pos offset;

        enum Tile{

            Sand('.'),
            Clay('#'),
            WaterFlowing('|'),
            WaterStopped('~'),
            Source('+');

            public char tileChar;

            Tile(char tileChar){
                this.tileChar = tileChar;
            }

        }

        private ArrayList<char[]> map;
        private char[][] waterMap;
        HashSet<Pos> history = new HashSet<>();
        public int waterCount = 0;

        private void addClay(Pos pos) {
            clayPositions.add(pos);

            if (minPos == null){
                minPos = new Pos(pos);
            } else {
                minPos.x = Math.min(minPos.x, pos.x);
                minPos.y = Math.min(minPos.y, pos.y);
            }
            if(maxPos == null){
                maxPos = new Pos(pos);
            } else {
                maxPos.x = Math.max(maxPos.x, pos.x);
                maxPos.y = Math.max(maxPos.y, pos.y);
            }
        }

        private void createMap(){
            inputMinY = minPos.y;
            minPos.y = Math.min(source.y, minPos.y);
            minPos.x -= 2;
            minPos.y -= 2;
            inputMaxY = maxPos.y;
            maxPos.x += 2;
            maxPos.y += 1;
            offset = new Pos(minPos.y, minPos.x);
            width = maxPos.x - minPos.x + 1;
            height = maxPos.y - minPos.y + 1;

            map = new ArrayList<>();
            Pos testPos = new Pos(0, 0);
            for(int i = minPos.y; i <= maxPos.y; ++i){
                testPos.y = i;
                char[] row = new char[width];
                for(int j = 0; j < width; ++j){
                    testPos.x = j + offset.x;
                    row[j] = clayPositions.contains(testPos) ? Tile.Clay.tileChar :
                            testPos.equals(source)? Tile.Source.tileChar :
                            Tile.Sand.tileChar;
                }
                map.add(row);
            }
        }

        public int getRemainignWaterCount() {
            int count = 0;
            for(int i = 0; i < height; ++i) {
                for(int j = 0; j < waterMap[i].length; ++j){
                    if(waterMap[i][j] == Tile.WaterStopped.tileChar)
                        count++;
                }
            }
            return count;
        }

        private void resetWaterPosition(Pair<Pos, Integer> waterPos, Pos relativeSource){
            waterPos.first.y = relativeSource.y;
            waterPos.first.x = relativeSource.x;
            waterPos.second = 0;
        }
        private void run(){
            waterMap = new char[map.size()][];
            for(int i = 0; i < height; ++i) {
                char[] mapRow = map.get(i);
                waterMap[i] = Arrays.copyOf(map.get(i),mapRow.length);
            }
            Pos relativeSource = new Pos(source.y - offset.y, source.x - offset.x);

            // Relative(!) position and horizontal movement (-1 left, 0 straight, 1 right)
            Pair<Pos, Integer> waterPos = new Pair<>(new Pos(relativeSource.y, relativeSource.x), 0);
            waterCount = 0;
            // height - 1 -> 1 larger than max y
            while(waterPos.first.y + 1 < height){
                if(waterPos.first.y + 1 >= height)
                    throw new RuntimeException("Water y position too large!");

                // source is blocked (only if something else is wrong)
                if(waterMap[waterPos.first.y][waterPos.first.x] == Tile.WaterFlowing.tileChar
                    || waterMap[waterPos.first.y][waterPos.first.x] == Tile.WaterStopped.tileChar)
                    break;

                if(waterMap[waterPos.first.y + 1][waterPos.first.x] != Tile.Clay.tileChar
                        && waterMap[waterPos.first.y + 1][waterPos.first.x] != Tile.WaterStopped.tileChar){

                    if(waterPos.first.y == (inputMaxY - offset.y)){
                        // Stops at max Y position
                        waterMap[waterPos.first.y][waterPos.first.x] = Tile.WaterFlowing.tileChar;
                        if(!history.add(new Pos(waterPos.first)))
                            throw new RuntimeException("Duplicated count!");
                        waterCount++;
                        resetWaterPosition(waterPos, relativeSource);
                        continue;
                    }
                    if(waterMap[waterPos.first.y + 1][waterPos.first.x] == Tile.WaterFlowing.tileChar){
                        // Source is not a valid position for water.
                        if(waterPos.first.equals(relativeSource))
                            break;
                        // Also position above min Y is not valid.
                        if(waterPos.first.y < (inputMinY - offset.y))
                            break;

                        // Stop as flowing water
                        waterMap[waterPos.first.y][waterPos.first.x] = Tile.WaterFlowing.tileChar;
                        if(!history.add(new Pos(waterPos.first)))
                            throw new RuntimeException("Duplicated count!");
                        waterCount++;
                        resetWaterPosition(waterPos, relativeSource);
                        continue;
                    }

                    waterPos.first.y++;
                    waterPos.second = 0;
                    continue;
                }

                if(waterPos.first.x + 1 >= width) {
                    throw new RuntimeException("Water x position too large!");
                } else if(waterPos.second != -1 && waterMap[waterPos.first.y][waterPos.first.x + 1] != Tile.Clay.tileChar &&
                    waterMap[waterPos.first.y][waterPos.first.x + 1] != Tile.WaterStopped.tileChar &&
                    waterMap[waterPos.first.y][waterPos.first.x + 1] != Tile.WaterFlowing.tileChar) {
                    waterPos.first.x += 1;
                    waterPos.second = 1;
                    continue;
                }

                if(waterPos.first.x - 1 < 0) {
                    throw new RuntimeException("Water x position too small!");
                } else if(waterPos.second != 1 && waterMap[waterPos.first.y][waterPos.first.x - 1] != Tile.Clay.tileChar &&
                        waterMap[waterPos.first.y][waterPos.first.x - 1] != Tile.WaterStopped.tileChar &&
                        waterMap[waterPos.first.y][waterPos.first.x - 1] != Tile.WaterFlowing.tileChar){
                    waterPos.first.x -= 1;
                    waterPos.second = -1;
                    continue;
                }

                // Source is not a valid position for water.
                if(waterPos.equals(relativeSource))
                    break;
                // Also position above min Y is not valid.
               if(waterPos.first.y < (inputMinY - offset.y))
                    break;

                boolean isFlowing = (waterMap[waterPos.first.y][waterPos.first.x - 1] == Tile.WaterFlowing.tileChar) ||
                        (waterMap[waterPos.first.y][waterPos.first.x + 1] == Tile.WaterFlowing.tileChar);
                // Surrounded by water but left and right are contradicting regarding flowing or stopped.
                // Then we need to changed water marked as stopped to flowing
                if(isFlowing){
                    if(waterMap[waterPos.first.y][waterPos.first.x - 1] == Tile.WaterStopped.tileChar){
                        for(int x = waterPos.first.x - 1; waterMap[waterPos.first.y][x] == Tile.WaterStopped.tileChar; x--)
                            waterMap[waterPos.first.y][x] = Tile.WaterFlowing.tileChar;
                    }
                    if(waterMap[waterPos.first.y][waterPos.first.x + 1] == Tile.WaterStopped.tileChar){
                        for(int x = waterPos.first.x + 1; waterMap[waterPos.first.y][x] == Tile.WaterStopped.tileChar; x++)
                            waterMap[waterPos.first.y][x] = Tile.WaterFlowing.tileChar;
                    }
                }

                waterMap[waterPos.first.y][waterPos.first.x] = isFlowing ? Tile.WaterFlowing.tileChar:
                        Tile.WaterStopped.tileChar;
                if(!history.add(new Pos(waterPos.first)))
                    throw new RuntimeException("Duplicated count!");
                waterCount++;
                resetWaterPosition(waterPos, relativeSource);
            }
        }

        public void printMap() {
            for(int i = 0; i < height; ++i)
                System.out.format("%d: %s%n", i + offset.y, new String(waterMap[i]));
        }

        private static String[] testData = {
                "x=495, y=2..7",
                "y=7, x=495..501",
                "x=501, y=3..7",
                "x=498, y=2..4",
                "x=506, y=1..2",
                "x=498, y=10..13",
                "x=504, y=10..13",
                "y=13, x=498..504"
        };

        public void parseInputLines(ArrayList<String> lines) {
            for(String line: lines){
                Pattern pattern = Pattern.compile("(x|y)=(\\d+), (x|y)=(\\d+)..(\\d+)", 0);
                Matcher matcher = pattern.matcher(line);
                if(matcher.matches()){
                    boolean xRow = (matcher.group(1).compareTo("x") == 0);
                    int v1 = Integer.parseInt(matcher.group(2));
                    int v2 = Integer.parseInt(matcher.group(4));
                    int v3 = Integer.parseInt(matcher.group(5));
                    for(int i = v2; i <= v3; i++)
                        addClay(xRow ? new Pos(i, v1) : new Pos(v1, i));
                }
            }
            createMap();
        }
    }

    public static ArrayList<String> getInputLines(String fileName){
        ArrayList<String> lines = new ArrayList<>();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                lines.add(line);
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return lines;
    }

    public static void main(String[] args) {
        Day17 obj = new Day17();

//        Reservoir testReservoir = obj.new Reservoir();
//        testReservoir.parseInputLines(new ArrayList<>(Arrays.asList(Reservoir.testData)));
//        testReservoir.run();
//        testReservoir.printMap();
//        System.out.println(testReservoir.waterCount);

        ArrayList<String> lines = getInputLines("Day17_input.txt");
        Reservoir reservoir = obj.new Reservoir();
        reservoir.parseInputLines(lines);
        reservoir.run();
        // reservoir.printMap();

        System.out.println("Day 17:");
        System.out.println("Question 1: How many tiles can the water reach within the range of y values in your scan?");
        System.out.println("Answer: " + reservoir.waterCount);
        System.out.println("Question 2: How many water tiles are left after the water spring stops producing water "
                + "and all remaining water not at rest has drained?");
        System.out.println("Answer: " + reservoir.getRemainignWaterCount());
    }
}
