// Day6.java
// AoC 2018 Day 6: Chronal Coordinates
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import net.chikitpao.util.Pos;

public class Day6 {
    class Pos2 extends Pos
    {
        int nearestPosIndex = -1;
        public Pos2(int y, int x){
            super(y, x);
        }

        public void getNearestPosition(ArrayList<Pos> positions) {
            ArrayList<Integer> distances = new ArrayList<>(positions.size());
            for(Pos position: positions){
                int distance = Math.abs(x - position.x) + Math.abs(y - position.y);
                distances.add(distance);
            }
            int minDistance = Collections.min(distances);
            ArrayList<Integer> minIndices = new ArrayList<>();
            for(int i = 0; i < distances.size(); ++i){
                if(minDistance == distances.get(i))
                    minIndices.add(i);
            }
            if(minIndices.size() == 1)
                nearestPosIndex = minIndices.get(0);
            // otherwise it remains -1.
        }
    }
    // minimum / maximum coordinates of positions from input
    private int minX = 0;
    private int maxX = 0;
    private int minY = 0;
    private int maxY = 0;

    public static int GetAnswer1(Day6 obj, ArrayList<Pos> positions) {
        ArrayList<Integer> nearestPosCount = new ArrayList<>();
        for(int i = 0; i < positions.size(); ++i)
            nearestPosCount.add(0);

        HashSet<Integer> boundaryIndices = new HashSet<>();
        // Check additional positions at the boundaries
        int edgeY = obj.maxY - obj.minY;
        int edgeX = obj.maxX - obj.minX;
        for(int i = obj.minY - edgeY; i <= obj.maxY + edgeY; ++i){
            for(int j = obj.minX - edgeX; j <= obj.maxX + edgeX; ++j){
                Pos2 pos2 = obj.new Pos2(i, j);
                pos2.getNearestPosition(positions);
                if(pos2.nearestPosIndex != -1)
                {
                    if(i == obj.minY - edgeY || i == obj.maxY + edgeY || j == obj.minX - edgeX || j == obj.maxX + edgeX)
                        boundaryIndices.add(pos2.nearestPosIndex);
                    nearestPosCount.set(pos2.nearestPosIndex, nearestPosCount.get(pos2.nearestPosIndex) + 1);
                }
            }
        }

        boundaryIndices.forEach(e -> { nearestPosCount.set(e, 0); });
        return Collections.max(nearestPosCount);
    }
    public static int GetAnswer2(Day6 obj, ArrayList<Pos> positions) {
        int count = 0;
        for(int i = obj.minY - 10000; i <= obj.maxY + 10000; ++i){
            for(int j = obj.minX - 10000; j <= obj.maxX + 10000; ++j){
                int sum = 0;
                boolean isValid = true;
                for(Pos position: positions) {
                    int distance = Math.abs(j - position.x) + Math.abs(i - position.y);
                    sum += distance;
                    if(sum >= 10000){
                        isValid = false;
                        break;
                    }
                }
                if(isValid)
                    count++;
            }
        }
        return count;
    }
    public static ArrayList<Pos> parseInput(String fileName){
        ArrayList<Pos> positions = new ArrayList<>();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                int pos = line.indexOf(", ");
                positions.add(new Pos(Integer.parseInt(line.substring(pos + 2)),
                        Integer.parseInt(line.substring(0, pos))));
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return positions;
    }
    public static void main(String[] args) {
        ArrayList<Pos> positions = parseInput("Day6_input.txt");
        Day6 obj = new Day6();

        obj.minX = obj.maxX = positions.get(0).x;
        obj.minY = obj.maxY = positions.get(0).y;
        for(Pos position: positions){
            obj.minX = Math.min(obj.minX, position.x);
            obj.maxX = Math.max(obj.maxX, position.x);
            obj.minY = Math.min(obj.minY, position.y);
            obj.maxY = Math.max(obj.maxY, position.y);
        }

        System.out.println("Day 6:");
        System.out.println("Question 1: What is the size of the largest area that isn't infinite?");
        System.out.println("Answer: " + GetAnswer1(obj, positions));
        System.out.println("Question 2: What is the size of the region containing all locations which have a total "
                + "distance to all given coordinates of less than 10000?");
        System.out.println("Answer: " + GetAnswer2(obj, positions));
    }
}
