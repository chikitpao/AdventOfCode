// Day18.java
// AoC 2018 Day 18: Settlers of The North Pole
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import net.chikitpao.util.Pair;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;


public class Day18 {
    class LumberArea{
        private int width = 0;
        private int height = 0;

        public class Count extends Pair<Integer, Integer> {
            public Count(Integer treeCount, Integer lumberyardCount){
                super(treeCount, lumberyardCount);
            }
            public Integer getTreeCount() { return first; }
            public Integer getLumberyardCount() { return second; }

            public void incrementTreeCount() { first++; }

            public void incrementLumberyardCount() { second++; }
        }

        public class CycleFinder {
            private HashMap<Count, Integer> history = new HashMap<>();
            private ArrayList<Integer> duplicateMinutes = new ArrayList<>();

            int requiredCycles;
            public CycleFinder(int requiredCycles){
                this.requiredCycles = requiredCycles;
            }
            public boolean storeResult(Count count, Integer minute){
                Integer previousKey = history.put(count, minute);
                if(previousKey != null){
                    duplicateMinutes.add(minute);
                    if(duplicateMinutes.size() > requiredCycles)
                    {
                        int i = 0;
                        for(; i < requiredCycles - 1; i++){
                            if(duplicateMinutes.get(duplicateMinutes.size() - 1 - i) !=
                                    duplicateMinutes.get(duplicateMinutes.size() - 2 - i) + 1)
                                break;
                        }
                        if(i == requiredCycles - 1)
                        {
                            System.out.format("## End of %d successive duplications: (TreeCount %d and "
                                    + "lumberyardCount %d) after minute %d and minute %d%n",
                                    requiredCycles, count.getTreeCount(), count.getLumberyardCount(), previousKey,
                                    minute);
                            return true;
                        }
                    }

                }
                return false;
            }
        }

        enum Tile{
            Open('.'),
            Tree('|'),
            Lumberyard('#');

            public final char tileChar;

            Tile(char tileChar){
                this.tileChar = tileChar;
            }
        }

        private ArrayList<char[]> map = new ArrayList<>();
        private ArrayList<char[]> oldMap = new ArrayList<>();

        // Returns number of wooded acres and number of lumberyards.
        private Count run(int minutes, CycleFinder cycleFinder){
            for(int minute = 1; minute <=  minutes; ++minute){
                copyMap();
                for(int i = 0; i < height; ++i){
                    for(int j = 0; j < width; ++j){
                        Count neighborhood = getNeighborhood(oldMap, i, j);
                        char current = oldMap.get(i)[j];
                        if(current == Tile.Open.tileChar){
                            if(neighborhood.getTreeCount() >= 3)
                                map.get(i)[j] = Tile.Tree.tileChar;
                        } else if(current == Tile.Tree.tileChar){
                            if(neighborhood.getLumberyardCount() >= 3)
                                map.get(i)[j] = Tile.Lumberyard.tileChar;
                        } else if(current == Tile.Lumberyard.tileChar){
                            if(neighborhood.getTreeCount() >= 1 && neighborhood.getLumberyardCount() >= 1)
                                map.get(i)[j] = Tile.Lumberyard.tileChar;
                            else
                                map.get(i)[j] = Tile.Open.tileChar;
                        } else {
                            throw new RuntimeException("Invalid tile!");
                        }
                    }
                }
                if(cycleFinder != null){
                    Count count = countResources();
                    if(cycleFinder.storeResult(count, minute))
                        return count;
                }
            }
            return countResources();
        }

        private Count countResources(){
            Count count = new Count(0, 0);
            for(int i = 0; i < height; ++i){
                for(int j = 0; j < width; ++j){
                    char current = map.get(i)[j];
                    if(current == Tile.Tree.tileChar)
                        count.incrementTreeCount();
                    else if(current == Tile.Lumberyard.tileChar)
                        count.incrementLumberyardCount();
                }
            }
            return count;
        }

        private Count getNeighborhood(ArrayList<char[]> map_, int row, int column) {
            Count count = new Count(0, 0);
            for(int i = -1; i < 2; ++i){
                for(int j = -1; j < 2; ++j) {
                    if(i == 0 && j == 0)
                        continue;
                    if(isValidPosition(row + i, column + j)){
                        char tile = map_.get(row + i)[column + j];
                        if(tile == Tile.Tree.tileChar)
                            count.incrementTreeCount();
                        else if(tile == Tile.Lumberyard.tileChar)
                            count.incrementLumberyardCount();
                    }
                }
            }
            return count;
        }
        private boolean isValidPosition(int row, int column){
            if(row < 0 || row >= height)
                return false;
            if(column < 0 || column >= width)
                return false;
            return true;
        }

        public void parseInputLines(ArrayList<String> lines) {
            height = lines.size();
            for(String line: lines){
                map.add(Arrays.copyOf(line.toCharArray(), line.length()));
                oldMap.add(new char[line.length()]);
            }
            width = map.get(0).length;
        }

        private void copyMap(){
            for(int i = 0; i < map.size(); ++i){
                System.arraycopy(map.get(i), 0, oldMap.get(i), 0, width);
            }
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
        Day18 obj = new Day18();

        ArrayList<String> lines = getInputLines("Day18_input.txt");
        System.out.println("Day 18:");
        System.out.println("Question 1: What will the total resource value of the lumber collection area be after 10 "
                + "minutes?");
        LumberArea lumberArea = obj.new LumberArea();
        lumberArea.parseInputLines(lines);
        LumberArea.Count answer1 = lumberArea.run(10, null);
        System.out.println("Answer: " + (answer1.getTreeCount() * answer1.getLumberyardCount()));

        System.out.println("Question 2: What will the total resource value of the lumber collection area be after "
                + "1000000000 minutes?");
        LumberArea lumberArea2 = obj.new LumberArea();
        lumberArea2.parseInputLines(lines);
        LumberArea.CycleFinder cycleFinder = lumberArea2.new CycleFinder(500);
        lumberArea.run(1000000000, cycleFinder);
        // Output of CycleFinder:
        // requiredCycles = 20
        // End of 20 successive duplications: (TreeCount 607 and lumberyardCount 354) after minute 426 and minute 454
        // requiredCycles = 500
        // End of 500 successive duplications: (TreeCount 596 and lumberyardCount 334) after minute 906 and minute 934
        // => Real cycle starts with minute 434, cycle length 28 (= 454 - 426 or = 934 - 906)
        // desiredCycle: minutes which will produce the same result as 1000000000 minutes
        // Use 434 instead of 433 to be sure.
        int desiredCycle = (1000000000 - 434) % 28 + 434;
        LumberArea lumberArea3 = obj.new LumberArea();
        lumberArea3.parseInputLines(lines);
        LumberArea.Count answer2 = lumberArea3.run(desiredCycle, null);
        System.out.println("Answer: " + (answer2.getTreeCount() * answer2.getLumberyardCount()));
    }
}
