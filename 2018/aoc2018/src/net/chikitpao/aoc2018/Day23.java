// Day23.java
// AoC 2018 Day 23: Experimental Emergency Teleportation
// Author: Chi-Kit Pao
//
// REMARK: For part 2, I used Java to write a MPS file for input of COIN-OR Branch and Cut solver (CBC), and return
// values from the solution file.
//

package net.chikitpao.aoc2018;

import net.chikitpao.util.Pair;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
        // 1. Find minimum and maximum coordinates.
        Nanobot maxBot = Collections.max(nanobots, Comparator.comparing(e -> e.maxReachable[index]));
        Nanobot minBot = Collections.min(nanobots, Comparator.comparing(e -> e.minReachable[index]));
        return new Pair<>(minBot.minReachable[index], maxBot.maxReachable[index]);
    }

    class MpsColumn{
        static final String ENDLINE = "\r\n";
        String name;
        boolean isInteger;
        ArrayList<Pair<String, Long>> coefficients = new ArrayList<>();
        MpsColumn(String name) {
            this(name, false);
        }
        MpsColumn(String name, boolean isInteger){
            this.name = name;
            this.isInteger = isInteger;
        }
        public void addConstraint(String rowName, long value) {
            coefficients.add(new Pair<>(rowName, value));
        }
        void writeColumns(BufferedWriter writer) throws IOException {
            if(isInteger)
                writer.write(String.format("    MARK      'MARKER'                 'INTORG'%s", MpsColumn.ENDLINE));
            for(Pair<String, Long> row: coefficients){
                writer.write(String.format("    %-8s  %-8s  %s%s",
                        name, row.first, MpsColumn.longToString(row.second), MpsColumn.ENDLINE));
            }
            if(isInteger)
                writer.write(String.format("    MARK      'MARKER'                 'INTEND'%s", MpsColumn.ENDLINE));
        }
        public static void writeRhs(BufferedWriter writer, Pair<String, Long> rhsValue) throws IOException {
            writer.write(String.format("    %-8s  %-8s  %s%s",
                    "RHS", rhsValue.first, MpsColumn.longToString(rhsValue.second), MpsColumn.ENDLINE));
        }
        public static String longToString(long value) {
            if(value == 0l)
                return " 0.000000000000e+00";

            char sign = (value > 0) ? ' ' : '-';
            long absValue = Math.abs(value);

            // Only works when mantissa is long enough, otherwise result is not reliable as integer / long.
            int exp = 0;
            long value1 = 0;
            long value2 = 0;
            long place = 1l;
            long mulitpleAfterPoint = 1000000000000l;
            for(; exp <= 12; exp++){
                if(place * 10l > absValue){
                    value1 = absValue / place;
                    value2 = (exp == 0) ? 0 : ((absValue % place) * mulitpleAfterPoint);
                    break;
                }
                mulitpleAfterPoint /= 10l;
                place *= 10l;
            }
            if(exp > 12)
                throw new NumberFormatException(String.format("Value too large for conversion: %d", value));

            String str = String.format("%c%d.%012de+%02d", sign, value1, value2, exp);
            return str;
        }
    }

    // Create a MPS file for input of COIN-OR Branch and Cut solver (CBC), and return values from the solution file.
    private static long getAnswer2(Day23 obj) {
        // Find minimum and maximum coordinates.
        long maxM = Math.abs((long) nanobots.get(0).maxReachable[0]);
        for(int i = 0; i < 3; ++i){
            Pair<Integer, Integer> axisExtrema = findExtrema(i);
            // Output:
            // ## index 0 min -211915514 max 264380721
            // ## index 1 min -284459723 max 208522400
            // ## index 2 min -164183940 max 194089849
            System.out.format("## index %d min %d max %d%n", i, axisExtrema.first, axisExtrema.second);
            maxM = Math.max(maxM, Math.abs((long) axisExtrema.first));
            maxM = Math.max(maxM, Math.abs((long) axisExtrema.second));
        }
        maxM = 6l * (maxM + 1l);
        // It seems that some maxM value will cause CBC to output wrong coordinates in spite of correct objective
        // value. Try next higher power of two.
        long tempMaxM = 1l;
        while(tempMaxM < maxM)
            tempMaxM *= 2;
        maxM = tempMaxM;

        // Create column variables and fill coefficients
        final int BOT_COUNT = 1000;
        int constraintCount = 0;
        ArrayList<Pair<String, Long>> rhsValues = new ArrayList<>();
        ArrayList<MpsColumn> covColumns = new ArrayList<>(BOT_COUNT);
        for(int i = 0; i < BOT_COUNT; i++)
            covColumns.add(obj.new MpsColumn(String.format("COV%04d", i), true));
        MpsColumn totalCovColumn = obj.new MpsColumn("TOTALCOV", true);
        MpsColumn xColumn = obj.new MpsColumn("X");
        MpsColumn yColumn = obj.new MpsColumn("Y");
        MpsColumn zColumn = obj.new MpsColumn("Z");
        ArrayList<MpsColumn> allColumns = new ArrayList<>(BOT_COUNT + 4);
        allColumns.addAll(covColumns);
        allColumns.add(totalCovColumn);
        allColumns.add(xColumn);
        allColumns.add(yColumn);
        allColumns.add(zColumn);
        // TOTALCOV - sum of (COV0000, ..., COV0999) = 0
        constraintCount++;
        totalCovColumn.addConstraint(String.format("_C%d", constraintCount), 1);
        totalCovColumn.addConstraint("OBJ", 1);
        for(MpsColumn covColumn :covColumns)
            covColumn.addConstraint(String.format("_C%d", constraintCount), -1);
        rhsValues.add(new Pair<>(String.format("_C%d", constraintCount), 0l));
        // (signs[0] * X + signs[1] * Y + signs[2] * Z + maxM * COV<index>) <=
        // (bot.radius + (bot.pos[0] * signs[0]) + (bot.pos[1] * signs[1]) + (bot.pos[2] * signs[2]) + maxM);
        int index = 0;
        for(Nanobot bot: nanobots) {
            for(int subIndex = 0; subIndex < 8; subIndex++) {
                constraintCount++;
                int[] signs = new int[]{((subIndex & 1) == 0) ? 1 : -1,
                        ((subIndex & 2) == 0) ? 1 : -1,
                        ((subIndex & 4) == 0) ? 1 : -1};
                covColumns.get(index).addConstraint(String.format("_C%d", constraintCount), maxM);
                xColumn.addConstraint(String.format("_C%d", constraintCount), signs[0]);
                yColumn.addConstraint(String.format("_C%d", constraintCount), signs[1]);
                zColumn.addConstraint(String.format("_C%d", constraintCount), signs[2]);
                long rhs = ((long) bot.radius) + (((long) bot.pos[0]) * signs[0]) + (((long) bot.pos[1]) * signs[1])
                        + (((long) bot.pos[2]) * signs[2]) + maxM;
                rhsValues.add(new Pair<>(String.format("_C%d", constraintCount), rhs));
            }
            index++;
        }

        // Write content of MPS file
        Path path = FileSystems.getDefault().getPath("input", "Day23_Part2.mps");
        Charset charset = Charset.forName("US-ASCII");
        try (BufferedWriter writer = Files.newBufferedWriter(path, charset)) {
            writer.write("*SENSE:Maximize" + MpsColumn.ENDLINE);
            writer.write("NAME          FindMaxCoverage" + MpsColumn.ENDLINE);
            // Section ROWS
            writer.write("ROWS" + MpsColumn.ENDLINE);
            writer.write(" N  OBJ" + MpsColumn.ENDLINE);
            writer.write(" E  _C1" + MpsColumn.ENDLINE);
            for(int i = 2; i <= 8001; i++)
                writer.write(String.format(" L  _C%d%s", i, MpsColumn.ENDLINE));
            // Section COLUMNS
            writer.write("COLUMNS" + MpsColumn.ENDLINE);
            for(MpsColumn column: allColumns)
                column.writeColumns(writer);
            // Section RHS
            writer.write("RHS" + MpsColumn.ENDLINE);
            for(Pair<String, Long> rhsValue: rhsValues)
                MpsColumn.writeRhs(writer, rhsValue);
            // Section BOUNDS
            writer.write("BOUNDS" + MpsColumn.ENDLINE);
            for(int i = 0; i < BOT_COUNT; i++)
                writer.write(String.format(" BV BND       COV%04d %s", i, MpsColumn.ENDLINE));
            writer.write(String.format(" LO BND       TOTALCOV   0.000000000000e+00%s", MpsColumn.ENDLINE));
            writer.write(" FR BND       X       " + MpsColumn.ENDLINE);
            writer.write(" FR BND       Y       " + MpsColumn.ENDLINE);
            writer.write(" FR BND       Z       " + MpsColumn.ENDLINE);
            // Section ENDATA
            writer.write("ENDATA" + MpsColumn.ENDLINE);
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }

        // Command line (CBC is installed with PuLP package on my machine):
        // C:\Users\Chi-K\AppData\Roaming\Python\Python39\site-packages\pulp\solverdir\cbc\win\64\cbc.exe
        // C:\Users\Chi-K\Documents\code\gitclone\own\AdventOfCode\2018\aoc2018\input\Day23_Part2.mps max timeMode
        // elapsed branch printingOptions all solution
        // C:\Users\Chi-K\Documents\code\gitclone\own\AdventOfCode\2018\aoc2018\input\Day23_Part2.sol
        // (default strategy 1)

        // End of output file Day23_Part2.sol:
        //   1000 TOTALCOV             976                       1
        //   1001 X               47812313                      -0
        //   1002 Y               20634366                      -0
        //   1003 Z               11715984                      -0

        return Nanobot.getDistance3D(new int[]{47812313, 20634366, 11715984}, new int[]{0, 0, 0});
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
        System.out.println("Answer: " + getAnswer2(obj));
    }

}
