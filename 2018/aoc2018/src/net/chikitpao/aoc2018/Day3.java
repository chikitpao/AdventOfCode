// Day3.java
// AoC 2018 Day 3: No Matter How You Slice It
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import net.chikitpao.util.Pos;

public class Day3 {
    class Claim{
        int id;
        int x;
        int y;
        int width;
        int height;
        Claim(String idStr, String xStr, String yStr, String widthStr, String heightStr)
        {
            id = Integer.parseInt(idStr);
            x = Integer.parseInt(xStr);
            y = Integer.parseInt(yStr);
            width = Integer.parseInt(widthStr);
            height = Integer.parseInt(heightStr);
        }
    }
    public static int GetAnswer1(ArrayList<Day3.Claim> claims) {
        HashSet<Pos> claimedPositions = new HashSet<>();
        HashSet<Pos> duplicatedPositions = new HashSet<>();

        // Method 1: Use object with own equals and hashCode implementation.
        long start = System.currentTimeMillis();
        for(Day3.Claim claim: claims) {
            for (int i = claim.y; i < claim.y + claim.height; ++i) {
                for (int j = claim.x; j < claim.x + claim.width; ++j) {
                    Pos pos = new Pos(i, j);
                    if (!claimedPositions.add(pos))
                        duplicatedPositions.add(pos);
                }
            }
        }
        long finish = System.currentTimeMillis();
        long timeElapsed = finish - start;
        System.out.println("## timeElapsed (Pos): " + timeElapsed);

        // Method 2: Use string represention
        HashSet<String> claimedPositionsStr = new HashSet<>();
        HashSet<String> duplicatedPositionsStr = new HashSet<>();
        start = System.currentTimeMillis();
        for(Day3.Claim claim: claims){
            for(int i = claim.y; i < claim.y + claim.height; ++i){
                for(int j = claim.x; j < claim.x + claim.width; ++j) {
                    String str = "" + i + "," + j;
                    if(!claimedPositionsStr.add(str))
                        duplicatedPositionsStr.add(str);
                }
            }
        }
        finish = System.currentTimeMillis();
        timeElapsed = finish - start;
        System.out.println("## timeElapsed (String): " +  timeElapsed);

        System.out.println("## Answer with position strings: " + duplicatedPositionsStr.size());
        return duplicatedPositions.size();
    }
    public static int GetAnswer2(ArrayList<Day3.Claim> claims) {
        HashMap<Pos, Integer> claimedPositions = new HashMap<>();
        HashSet<Integer> intactClaims = new HashSet<>();

        // Method 1: Use object with own equals and hashCode implementation.
        for(Day3.Claim claim: claims) {
            boolean intact = true;
            for (int i = claim.y; i < claim.y + claim.height; ++i) {
                for (int j = claim.x; j < claim.x + claim.width; ++j) {
                    Pos pos = new Pos(i, j);
                    Integer previousClaim = claimedPositions.put(pos, claim.id);
                    if (previousClaim != null) {
                        intact = false;
                        intactClaims.remove(previousClaim);
                    }
                }
            }
            if(intact)
                intactClaims.add(claim.id);
        }
        if(intactClaims.size() != 1)
            throw new RuntimeException("Intact claims count is not 1!");
        return (Integer) intactClaims.toArray()[0];
    }
    public static void main(String[] args) {
        Day3 obj = new Day3();
        ArrayList<Day3.Claim> claims = new ArrayList<>();

        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", "Day3_input.txt");
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                // Example line:
                //#1 @ 509,796: 18x15
                Pattern pattern = Pattern.compile("#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)", 0);
                Matcher matcher = pattern.matcher(line);
                if(matcher.matches()){
                    claims.add(obj.new Claim(matcher.group(1), matcher.group(2), matcher.group(3), matcher.group(4),
                            matcher.group(5)));
                }
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }

        System.out.println("Day 3:");
        System.out.println("Question 1: How many square inches of fabric are within two or more claims?");
        System.out.println("Answer: " + GetAnswer1(claims));
        System.out.println("Question 2: What is the ID of the only claim that doesn't overlap?");
        System.out.println("Answer: " + GetAnswer2(claims));
    }
}
