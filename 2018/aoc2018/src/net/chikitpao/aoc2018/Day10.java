// Day10.java
// AoC 2018 Day 10: The Stars Align
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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.chikitpao.util.Pair;

public class Day10 {
    class LightPoint{
        // REMARK: Don't use class net.chikitpao.util.Pos since we need long here (Pos: int)
        public long x;
        public long y;
        public long vx;
        public long vy;
        public LightPoint(long x, long y, long vx, long vy){
            this.x = x;
            this.y = y;
            this.vx = vx;
            this.vy = vy;
        }
        public LightPoint(LightPoint other){
            this.x = other.x;
            this.y = other.y;
            this.vx = other.vx;
            this.vy = other.vy;
        }
        public void processStep(){
            x += vx;
            y += vy;
        }
        public static ArrayList<Long> calculateBoundingBox(Collection<LightPoint> lightPoints){
            LightPoint firstItem = lightPoints.iterator().next();
            Pair<Long, Long> upperLeft = new Pair<>(firstItem.x, firstItem.y);
            Pair<Long, Long> bottomRight = new Pair<>(firstItem.x, firstItem.y);
            for(LightPoint lightPoint: lightPoints){
                upperLeft.first = Math.min(upperLeft.first, lightPoint.x);
                upperLeft.second = Math.min(upperLeft.second, lightPoint.y);
                bottomRight.first = Math.max(bottomRight.first, lightPoint.x);
                bottomRight.second = Math.max(bottomRight.second, lightPoint.y);
            }
            ArrayList<Long> result = new ArrayList<>();
            result.add(upperLeft.first);
            result.add(upperLeft.second);
            result.add(bottomRight.first);
            result.add(bottomRight.second);
            return result;
        }
    }
    public static void GetAnswer1(Day10 obj, ArrayList<LightPoint> lightPoints) {
        long minBoundingBoxSize = -1;
        long minBoundingBoxFrame = -1;
        long minX = -1;
        long minY = -1;
        long maxX = -1;
        long maxY = -1;

        ArrayList<LightPoint> lightPointsCopy1 = new ArrayList<>();
        lightPoints.forEach(e -> lightPointsCopy1.add(obj.new LightPoint(e)));
        for(int i = 0; i < 100000; ++i){
            for(LightPoint lightPoint: lightPointsCopy1)
                lightPoint.processStep();
            ArrayList<Long> boundingBox = LightPoint.calculateBoundingBox(lightPointsCopy1);
            long dx = Math.abs(boundingBox.get(2) - boundingBox.get(0) + 1);
            long dy = Math.abs(boundingBox.get(3) - boundingBox.get(1) + 1);
            long boundingBoxSize = dx * dy;
            if((minBoundingBoxSize == -1) || (boundingBoxSize < minBoundingBoxSize)){
                minBoundingBoxSize = boundingBoxSize;
                minBoundingBoxFrame = i;
                minX = boundingBox.get(0);
                minY = boundingBox.get(1);
                maxX = boundingBox.get(2);
                maxY = boundingBox.get(3);
            }
        }
        // Output for my puzzle input:
        //## minBoundingBoxFrame 10812 minboundingBoxSize 620 width 62 height 10
        System.out.format("## minBoundingBoxFrame %d minboundingBoxSize %d width %d height %d%n",
                minBoundingBoxFrame, minBoundingBoxSize, maxX - minX + 1, maxY - minY + 1);

        ArrayList<LightPoint> lightPointsCopy2 = new ArrayList<>();
        lightPoints.forEach(e -> lightPointsCopy2.add(obj.new LightPoint(e)));
        HashSet<Pair<Long, Long>> positions = new HashSet<>();
        for(int i = 0; i <= 10812; ++i){
            for(LightPoint lightPoint: lightPointsCopy2)
            {
                lightPoint.processStep();
                if(i == 10812)
                    positions.add(new Pair<>(lightPoint.x, lightPoint.y));
            }
        }
        for(long i = 0; i <= maxY - minY; ++i){
            System.out.print(">>");
            for(long j = 0; j <= maxX - minX; ++j){
                boolean contains = positions.contains(new Pair<>(minX + j, minY + i));
                Character c = contains ? '#' : ' ';
                System.out.print(c);
            }
            System.out.print("\n");
        }
    }

    public static ArrayList<LightPoint> parseInput(Day10 obj, String fileName){
        ArrayList<LightPoint> lightPoints = new ArrayList<>();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                // Example line:
                // position=<-53868, -10684> velocity=< 5,  1>
                Pattern pattern = Pattern.compile("position=<\\s*([-]?\\d+),\\s*([-]?\\d+)> "
                        + "velocity=<\\s*([-]?\\d+),\\s*([-]?\\d+)>", 0);
                Matcher matcher = pattern.matcher(line);
                if(!matcher.matches())
                    throw new RuntimeException("Cannot parse line");
                lightPoints.add(obj.new LightPoint(Integer.parseInt(matcher.group(1)),
                                Integer.parseInt(matcher.group(2)), Integer.parseInt(matcher.group(3)),
                                Integer.parseInt(matcher.group(4))));
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return lightPoints;
    }

    public static void main(String[] args) {
        Day10 obj = new Day10();
        ArrayList<LightPoint> lightPoints = parseInput(obj, "Day10_input.txt");

        System.out.println("Day 10:");
        System.out.println("Question 1: What message will eventually appear in the sky?");
        System.out.println("Answer: ");
        GetAnswer1(obj, lightPoints);
        // Answer 1: ERCXLAJL
        System.out.println("Question 2: Exactly how many seconds would they have needed to wait for that message to "
                + "appear?");
        // Answer 2: 10813
    }
}
