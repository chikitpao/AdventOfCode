// Day4.java
// AoC 2018 Day 4: Repose Record
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
import java.util.stream.Collectors;
import java.util.stream.IntStream;

public class Day4 {
    class Shift{
        ArrayList<Integer> minutes = new ArrayList<>(); // minute of asleep and wake ups;

        public void fallAsleep(int minute) {
            if((minutes.size() % 2) != 0)
                throw new RuntimeException("Improper asleep!");
            minutes.add(minute);
        }

        public void wakeUp(int minute) {
            if((minutes.size() % 2) == 0)
                throw new RuntimeException("Improper wake up!");
            minutes.add(minute);
        }

        public int getMinutesAsleep(){
            int minutesAsleep = 0;
            for(int i = 0; i < minutes.size(); i+=2) {
                int end = (i + 1 < minutes.size()) ? minutes.get(i+1) : 60;
                minutesAsleep += end - minutes.get(i);
            }
            return minutesAsleep;
        }

        public ArrayList<Integer> getSleepingMinutes() {
            ArrayList<Integer> list= new ArrayList<>();
            for(int i = 0; i < minutes.size(); i+=2) {
                int end = (i + 1 < minutes.size()) ? minutes.get(i+1) : 60;
                for(int j = minutes.get(i); j < end; ++j)
                    list.add(j);
            }
            return list;
        }
    }

    class Guard {
        public int id;
        public ArrayList<Shift> shifts = new ArrayList<>();

        public Guard(int id) {
            this.id = id;
            shifts.add(new Shift());
        }

        public Shift addShift() {
            shifts.add(new Shift());
            return getLastShift();
        }
        public Shift getLastShift(){
            return shifts.get(shifts.size() - 1);
        }

        public int getMinutesAsleep(){
            int minutesAsleep = 0;
            for(Shift shift: shifts)
                minutesAsleep += shift.getMinutesAsleep();
            return minutesAsleep;
        }
        // minute with most sleep, frequency
        public int[] getSleepingMinute(){
            ArrayList<Integer> sleepingCounts = new ArrayList<>();
            for(int i = 0; i < 60; ++i)
                sleepingCounts.add(0);
            int max = 0;
            for(Shift shift: shifts) {
                for(int index: shift.getSleepingMinutes())
                {
                    int newValue = sleepingCounts.get(index) + 1;
                    if(newValue > max)
                        max = newValue;
                    sleepingCounts.set(index, newValue);
                }
            }
            return new int[]{sleepingCounts.indexOf(max), max};
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null)
                return false;
            if (!(obj instanceof Guard))
                return false;
            return id == ((Guard) obj).id;
        }

        @Override
        public int hashCode() {
            return id;
        }
    }
    public static int GetAnswer1( HashMap<Integer, Guard> guards) {
        // Strategy 1: Find the guard that has the most minutes asleep. What minute does that guard spend asleep the
        // most?
        Guard laziestGaurd = Collections.max(guards.values(), Comparator.comparing(guard -> guard.getMinutesAsleep()));
        return laziestGaurd.id * laziestGaurd.getSleepingMinute()[0];
    }
    public static int GetAnswer2( HashMap<Integer, Guard> guards) {
        // Strategy 2: Of all guards, which guard is most frequently asleep on the same minute?
        int maxFrequency = 0;
        int sleepingMinute = 0;
        int guardID = 0;
        for(Guard guard: guards.values()){
            int value[] = guard.getSleepingMinute();
            if(value[1] > maxFrequency){
                maxFrequency = value[1];
                sleepingMinute = value[0];
                guardID = guard.id;
            }
        }
        return guardID * sleepingMinute;
    }
    public static ArrayList<String> parseInput(String fileName){
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
        //Collections.sort(lines, (lhs, rhs) -> { return lhs.substring(0, 18).compareTo(rhs.substring(0,18));});
        Collections.sort(lines, Comparator.comparing(line -> line.substring(0, 18)));
        return lines;
    }
    public static void main(String[] args) {
        Day4 obj = new Day4();
        ArrayList<String> noteLines = parseInput("Day4_input.txt");
        HashMap<Integer, Guard> guards = new HashMap<>();

        Shift currentShift = null;
        for(String line: noteLines){
            // Example lines:
            //[1518-11-13 00:04] Guard #2411 begins shift
            //[1518-09-18 00:43] wakes up
            //[1518-05-31 00:54] falls asleep
            if (line.indexOf("Guard") != -1) {
                Pattern patternShift = Pattern.compile("\\[(\\d+)\\-(\\d+)\\-(\\d+) (\\d+):(\\d+)\\] Guard #(\\d+) begins shift", 0);
                Matcher matcher = patternShift.matcher(line);
                int guardId;
                if(!matcher.matches())
                    throw new RuntimeException("Cannot parse guard");
                guardId = Integer.parseInt(matcher.group(6));
                Guard currentGuard = guards.get(guardId);
                if(currentGuard == null){
                    currentGuard = obj.new Guard(guardId);
                    guards.put(guardId, currentGuard);
                    currentShift = currentGuard.getLastShift();
                } else{
                    currentShift = currentGuard.addShift();
                }
            } else if (line.indexOf("wakes") != -1) {
                Pattern patternWake = Pattern.compile("\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\] wakes up", 0);
                Matcher matcher = patternWake.matcher(line);
                if(matcher.matches())
                    currentShift.wakeUp(Integer.parseInt(matcher.group(5)));
            } else {
                if (line.indexOf("falls") == -1)
                    throw new RuntimeException("Unknown text!");
                Pattern patternAsleep = Pattern.compile("\\[(\\d+)-(\\d+)-(\\d+) (\\d+):(\\d+)\\] falls asleep", 0);
                Matcher matcher = patternAsleep.matcher(line);
                if(matcher.matches())
                    currentShift.fallAsleep(Integer.parseInt(matcher.group(5)));
            }
        }

        System.out.println("Day 4:");
        System.out.println("Question 1: What is the ID of the guard you chose multiplied by the minute you chose?");
        System.out.println("Answer: " + GetAnswer1(guards));
        System.out.println("Question 2: What is the ID of the guard you chose multiplied by the minute you chose?");
        System.out.println("Answer: " + GetAnswer2(guards));
    }
}
