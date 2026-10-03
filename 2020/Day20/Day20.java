// AoC 2020 Day 19: Monster Message
// Author: Chi-Kit Pao
//
// Outputs:
// Question 1: What do you get if you multiply together the IDs of the four corner tiles?
// Answer: 15670959891893
//

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class Main {
    protected static int getPattern(String line) {
        int pattern = 0;
        for (int i = 0; i < line.length(); ++i) {
            if (line.charAt(i) == '#')
                pattern += (1 << i);
        }
        return pattern;
    }
    protected static int normalize(int pattern, int width) {
        int alternative = 0;
        for (int i = 0; i < width; ++i) {
            if((pattern & (1 << i)) != 0) {
                alternative += (1 << (width - i - 1));
            }
        }
        return Math.min(pattern, alternative);
    }
    protected static void addMapping(HashMap<Integer, ArrayList<Long>> patternMap, int pattern, long tileId) {
        if (patternMap.containsKey(pattern)) {
            patternMap.get(pattern).add(tileId);
        } else {
            ArrayList<Long> ids = new ArrayList<>();
            ids.add(tileId);
            patternMap.put(pattern, ids);
        }
    }
    protected static HashMap<Integer, ArrayList<Long>> parseInput() {
        // pattern -> Tile IDs
        HashMap<Integer, ArrayList<Long>> patternMap = new HashMap<>();

        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("", "input.txt");
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            long currentTileId = -1;
            int currentRow = 0;
            int topPattern = 0;
            int leftPattern = 0;
            int rightPattern = 0;
            while ((line = reader.readLine()) != null) {
                if(line.isEmpty()) {
                    currentTileId = -1;
                    currentRow = 0;
                } else {
                    if (currentTileId == -1) {
                        String s = line.split(" ")[1];
                        currentTileId = Long.parseLong(s.substring(0, s.length() - 1));
                        leftPattern = 0;
                        rightPattern = 0;
                    } else {
                        int width = line.length();
                        leftPattern += (line.charAt(0) == '#') ? (1 << currentRow) : 0;
                        rightPattern += (line.charAt(width - 1) == '#') ? (1 << currentRow) : 0;
                        if (currentRow == 0) {
                            topPattern = normalize(getPattern(line), width);
                        } else if (currentRow + 1 == width) {
                            int bottomPattern = normalize(getPattern(line), width);
                            leftPattern = normalize(leftPattern, width);
                            rightPattern = normalize(rightPattern, width);
                            addMapping(patternMap, topPattern, currentTileId);
                            addMapping(patternMap, bottomPattern, currentTileId);
                            addMapping(patternMap, leftPattern, currentTileId);
                            addMapping(patternMap, rightPattern, currentTileId);
                        }
                        currentRow++;
                    }
                }
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return patternMap;
    }

    public static void main(String[] args) {
        var patternMap = parseInput();

        HashSet<Long> edgeIds = new HashSet<>();
        HashSet<Long> cornerIds = new HashSet<>();
        for (Map.Entry<Integer, ArrayList<Long>> e: patternMap.entrySet()) {
            if (e.getValue().size() == 1) {
                // ID of corner tile. Shall be associated to two patterns.
                long id = e.getValue().get(0);
                if (edgeIds.contains(id))
                    cornerIds.add(id);
                else
                    edgeIds.add(id);
            }
        }

        System.out.println(edgeIds.size());
        long answer1 = 1;
        for (Long id: cornerIds) {
            answer1 *= id;
        }

        System.out.println("Question 1: What do you get if you multiply together the IDs of the four corner tiles?");
        System.out.printf("Answer: %d\n", answer1);
    }
}