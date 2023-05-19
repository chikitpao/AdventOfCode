// Day7.java
// AoC 2018 Day 7: The Sum of Its Parts
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

public class Day7 {
    class Step{
        char name;
        int duration;
        int priority; // -1 empty, 0 highest priority, 1 a little bit lower, etc.
        ArrayList<Character> predecessors = new ArrayList<>();
        ArrayList<Character> successors = new ArrayList<>();

        Step(char name){
            this.name = name;
            this.duration = 60 + this.name - 'A' + 1;
            this.priority = -1;
        }
        void addPredecessor(char c){
            if(true)
                predecessors.add(c);
        }
        void addSuccessor(char c){
            if(true)
                successors.add(c);
        }
        public void sortConnections() {
            Collections.sort(predecessors);
            Collections.sort(successors);
        }
        public static ArrayList<ArrayList<Character>> CalculatePriorityTable(HashMap<Character, Step> steps){
            ArrayList<ArrayList<Character>> priorityTable = new ArrayList<>(steps.size());
            ArrayList<Character> pending = new ArrayList<>(steps.values().stream().map(step -> step.name)
                    .collect(Collectors.toList()));
            while(!pending.isEmpty())
            {
                for(int i = 0; i < pending.size(); )
                {
                    Step step = steps.get(pending.get(i));
                    if(step.predecessors.size() == 0)
                    {
                        priorityTable.add(new ArrayList<>());
                        priorityTable.get(0).add(pending.get(i));
                        step.priority = 0;
                        pending.remove(i);
                        // i remains the same
                        continue;
                    }
                    if(step.predecessors.stream().anyMatch(v -> pending.contains(v))) {
                        i++;
                        continue;
                    }
                    int priority = step.predecessors.stream().mapToInt(stepName -> steps.get(stepName).priority)
                            .max().getAsInt() + 1;
                    if(priority == priorityTable.size())
                        priorityTable.add(new ArrayList<>());
                    priorityTable.get(priority).add(pending.get(i));
                    step.priority = priority;
                    pending.remove(i);
                    // i remains the same
                }
            }
            return priorityTable;
        }
    }
    class ProcessingStep{
     public int startTime;
     public Step step;
     public ProcessingStep(int startTime, Step step){
         this.startTime = startTime;
         this.step = step;
     }
    }


    public static String GetAnswer1(HashMap<Character, Step> steps) {
        System.out.format("## Steps with no predecessors: %s%n",
                steps.values().stream().filter(step -> step.predecessors.size()==0).count());
        Step currentStep = steps.values().stream().filter(step -> step.predecessors.size()==0).findFirst().get();
        ArrayList<Step> done = new ArrayList<>();
        ArrayList<Step> nextSteps = new ArrayList<>();
        while(done.size() != steps.size()){
            done.add(currentStep);
            for(Character c: currentStep.successors){
                Step step = steps.get(c);
                if(done.contains(step) || nextSteps.contains(step))
                    continue;
                List<Step> predecessorSteps = step.predecessors.stream().map(p -> steps.get(p))
                        .collect(Collectors.toList());
                if(done.containsAll(predecessorSteps))
                    nextSteps.add(step);
            }
            nextSteps.sort(Comparator.comparing(step -> step.name));
            currentStep = nextSteps.isEmpty() ? null : nextSteps.remove(0);
        }
        return done.stream().map(step -> ((Character) step.name).toString()).collect(Collectors.joining());
    }

    public static int GetAnswer2(Day7 obj, HashMap<Character, Step> steps) {
        ArrayList<ArrayList<Character>> priorityTable = Step.CalculatePriorityTable(steps);
        // REMARK: For my puzzle input, I've got the following priorities (priority -> number of steps):
        // 0 -> 1 (S), 1 -> 1 (C), 2 -> 2 (L, P), 3 -> 6 (A, M, Q, V, W, Y), 4 -> 2 (N, U), 5 -> 1 (H), 6 -> 1 (O),
        // 7 -> 2 (T, D), 8 -> 2 (G, R), 9 -> 1 (K), 10 -> 1 (B), 11 -> 1 (J), 12 -> 1 (E), 13 -> 1 (F), 14 -> 1 (X),
        // 15 -> 1 (Z), 16 -> 1 (I)
        // Priority 3: (A, M, Y) <- P; (V, W) <- L; Q <- (L & P)
        // Priority 4: N <- W; U <- (M & Q & V)
        // Priority 5: H <- N & (M & V)
        // Priority 8: G <- T; R <- D
        // Priority 9: K <- G
        // REMARK (from problem description): If multiple steps are available, workers should still begin them in
        // alphabetical order.
        int currentTimeStamp = 0;
        ArrayList<Integer> relevantTimeStamps = new ArrayList<>();
        ArrayList<Step> done = new ArrayList<>();
        ArrayList<ProcessingStep> processing = new ArrayList<>();
        ArrayList<Character> remaining = new ArrayList<>(steps.keySet());
        Collections.sort(remaining);
        while(done.size() != steps.size()){
            for(int i = 0; i < processing.size(); ) {
                ProcessingStep ps = processing.get(i);
                if(ps.startTime + ps.step.duration <= currentTimeStamp)
                {
                    System.out.format("## Step %c finishes at time stamp %d%n", ps.step.name, currentTimeStamp);
                    done.add(ps.step);
                    processing.remove(i);
                    // i remains the same
                    continue;
                }
                i++;
            }

            Iterator<Character> iterator = remaining.iterator();
            while(processing.size() < 5 && iterator.hasNext()){
                Character c = iterator.next();
                Step step = steps.get(c);
                boolean isAvailable = false;
                if(step.predecessors.size() == 0)
                    isAvailable = true;
                else{
                    List<Step> predecessorSteps = step.predecessors.stream().map(p -> steps.get(p))
                            .collect(Collectors.toList());
                    if(done.containsAll(predecessorSteps))
                        isAvailable = true;
                }
                if(isAvailable)
                {
                    System.out.format("## Process %c at time stamp %d%n", step.name, currentTimeStamp);
                    processing.add(obj.new ProcessingStep(currentTimeStamp, step));
                    int timeStamp = currentTimeStamp + step.duration;
                    if(!relevantTimeStamps.contains(timeStamp))
                        relevantTimeStamps.add(timeStamp);
                    iterator.remove();
                }
            }
            if(!relevantTimeStamps.isEmpty()){
                Collections.sort(relevantTimeStamps);
                currentTimeStamp = relevantTimeStamps.remove(0);
            } else {
                return currentTimeStamp;
            }
        }
        return 0; // Unreachable
    }

    public static HashMap<Character, Step> parseInput(Day7 obj, String fileName){
        HashMap<Character, Step> steps = new HashMap<>();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                // Example line:
                // Step S must be finished before step C can begin.
                Pattern pattern = Pattern.compile("Step ([A-Z]) must be finished before step ([A-Z]) can begin.", 0);
                Matcher matcher = pattern.matcher(line);
                if(!matcher.matches())
                    throw new RuntimeException("Cannot parse line");
                Character char1 = matcher.group(1).charAt(0);
                Step step1 = steps.get(char1);
                if(step1 == null){
                    step1 = obj.new Step(char1);
                    steps.put(char1, step1);
                }
                Character char2 = matcher.group(2).charAt(0);
                Step step2 = steps.get(char2);
                if(step2 == null){
                    step2 = obj.new Step(char2);
                    steps.put(char2, step2);
                }
                step1.addSuccessor(char2);
                step2.addPredecessor(char1);
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        for(Step step: steps.values())
            step.sortConnections();
        return steps;
    }

    public static void main(String[] args) {
        Day7 obj = new Day7();
        HashMap<Character, Step> steps = parseInput(obj, "Day7_input.txt");

        System.out.println("Day 7:");
        System.out.println("Question 1: In what order should the steps in your instructions be completed?");
        System.out.println("Answer: " + GetAnswer1(steps));
        System.out.println("Question 2: With 5 workers and the 60+ second step durations described above, how long "
                + "will it take to complete all of the steps?");
        System.out.println("Answer: " + GetAnswer2(obj, steps));
    }
}
