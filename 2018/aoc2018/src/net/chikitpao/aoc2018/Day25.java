// Day25.java
// AoC 2018 Day 25: Four-Dimensional Adventure
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

public class Day25{
    class FixedPoint{
        public Constellation constellation;
        public int[] coordinates;
        FixedPoint(int[] coordinates){
            this.coordinates = coordinates;
        }
        public boolean isCloseEnough(FixedPoint otherFixedPoint) {
            return Math.abs(coordinates[0] - otherFixedPoint.coordinates[0])
                    + Math.abs(coordinates[1] - otherFixedPoint.coordinates[1])
                    + Math.abs(coordinates[2] - otherFixedPoint.coordinates[2])
                    + Math.abs(coordinates[3] - otherFixedPoint.coordinates[3])
                    <= 3;
        }
    }
    class Constellation{
        public ArrayList<FixedPoint> fixedPoints = new ArrayList<>();
        public Constellation(FixedPoint fixedPoint) {
            addFixedPoint(fixedPoint);
        }
        public void addFixedPoint(FixedPoint fixedPoint) {
            fixedPoints.add(fixedPoint);
            fixedPoint.constellation = this;
        }
        public void mergeConstellation(Constellation otherConstellation) {
            for(FixedPoint fixedPoint: otherConstellation.fixedPoints)
                addFixedPoint(fixedPoint);
            otherConstellation.fixedPoints.clear();
            constellations.remove(otherConstellation);
        }
    }
    static ArrayList<FixedPoint> fixedPoints = new ArrayList<>(1378);
    static HashSet<Constellation> constellations = new HashSet<>();
    private static void calculateConstellations(Day25 obj) {
        for(int i = 0; i < fixedPoints.size(); ++i){
            FixedPoint fixedPoint = fixedPoints.get(i);
            Constellation constellation;
            if(fixedPoint.constellation == null) {
                constellation = obj.new Constellation(fixedPoint);
                constellations.add(constellation);
            } else {
                constellation = fixedPoint.constellation;
            }
            for(int j = i + 1; j < fixedPoints.size(); ++j){
                FixedPoint otherFixedPoint = fixedPoints.get(j);
                if (otherFixedPoint.constellation == constellation)
                    continue;
                if(fixedPoint.isCloseEnough(otherFixedPoint)) {
                    if (otherFixedPoint.constellation == null) {
                        constellation.addFixedPoint(otherFixedPoint);
                    } else {
                        Constellation otherConstellation = otherFixedPoint.constellation;
                        constellation.mergeConstellation(otherConstellation);
                        constellations.remove(otherConstellation);
                    }
                }
            }
        }
    }
    public static void parseInput(Day25 obj, String fileName){
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if(line.isEmpty())
                    continue;
                String[] coordinateStrings = line.split(",");
                int[] coordinates = new int[4];
                for(int i = 0; i < coordinateStrings.length; ++i)
                    coordinates[i] = Integer.parseInt(coordinateStrings[i]);
                fixedPoints.add(obj.new FixedPoint(coordinates));
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
    }
    public static void main(String[] args) throws Exception {
        Day25 obj = new Day25();
        parseInput(obj, "Day25_input.txt");
        calculateConstellations(obj);

        System.out.println("Day 25:");
        System.out.println("Question: How many constellations are formed by the fixed points in spacetime?");
        System.out.println("Answer: " + constellations.size());
    }
}
