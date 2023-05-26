// Day16.java
// AoC 2018 Day 16: Chronal Classification
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
import java.util.*;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import static java.util.stream.Collectors.toCollection;

public class Day16 {
    public class SampleTest{
        ArrayList<ArrayList<ArrayList<String>>> testData = Stream.generate(ArrayList<ArrayList<String>>::new)
                .limit(16).collect(toCollection(ArrayList::new));
        ArrayList<ChronalDevice.Instruction> opCodeMapping = new ArrayList<>(Collections.nCopies(16, null));

        public void addCandidates(int i, ArrayList<String> candidates) {
            ArrayList<ArrayList<String>> values = testData.get(i);
            values.add(candidates);
        }

        private void filterCandidates() {
            // Only keep candidates which appear every test
            for (int j = 0; j < testData.size(); ++j){
                ArrayList<ArrayList<String>> data = testData.get(j);
                for(int i = 1; i < data.size(); ++i)
                    data.get(0).retainAll(data.get(i));
                data.subList(1, data.size()).clear();
            }
        }

        public void calculateOpCodeMap(ChronalDevice cd) {
            filterCandidates();
            HashMap<Integer, String> map;
            do{
                map = new HashMap<>();
                // Only 1 possibilty left for opCode?
                for(int i = 0; i < testData.size(); ++i){
                    ArrayList<String> data = testData.get(i).get(0);
                    if(data.size() == 1) {
                        map.put(i, data.get(0));
                        data.clear();
                    }
                }
                // Remove operations which have got an opCode.
                Collection<String> values = map.values();
                for(int i = 0; i < testData.size(); ++i){
                    ArrayList<String> data = testData.get(i).get(0);
                    if(!data.isEmpty())
                        data.removeAll(values);
                }

                for(Map.Entry<Integer, String> entry: map.entrySet()){
                    opCodeMapping.set(entry.getKey(), cd.getInstruction(entry.getValue()));
                }
            } while(!map.isEmpty());
        }
    }
    public class Sample{
        int[] registersBefore = new int[4];
        int[] instruction = new int[4];
        int[] registersAfter = new int[4];
        public static Sample parse(Day16 obj, String line1, String line2, String line3) {
            Sample sample = obj.new Sample();
            // Example lines:
            // Before: [0, 0, 2, 2]
            // 9 2 3 0
            // After:  [4, 0, 2, 2]
            Pattern pattern1 = Pattern.compile("(Before:|After: ) \\[(\\d+), (\\d+), (\\d+), (\\d+)\\]", 0);
            Matcher matcher = pattern1.matcher(line1);
            if(matcher.matches()){
                for(int i = 0; i < 4; ++i)
                    sample.registersBefore[i] = Integer.parseInt(matcher.group(i+2));
            }
            matcher = pattern1.matcher(line3);
            if(matcher.matches()){
                for(int i = 0; i < 4; ++i)
                    sample.registersAfter[i] = Integer.parseInt(matcher.group(i+2));
            }
            Pattern pattern2 = Pattern.compile("(\\d+) (\\d+) (\\d+) (\\d+)", 0);
            matcher = pattern2.matcher(line2);
            if(matcher.matches()){
                for(int i = 0; i < 4; ++i)
                    sample.instruction[i] = Integer.parseInt(matcher.group(i+1));
            }
            return sample;
        }
    }

    public static Pair<ArrayList<Sample>, ArrayList<int[]>> parseInput(Day16 obj, String fileName){
        ArrayList<Sample> samples = new ArrayList<>();
        ArrayList<int[]> instrs = new ArrayList<>();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            boolean section1 = true;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                if(section1 && line.startsWith("Before:")){
                    String line2 = reader.readLine();
                    String line3 = reader.readLine();
                    reader.readLine(); // empty line
                    samples.add(Sample.parse(obj, line, line2, line3));
                    continue;
                }
                section1 = false;
                Pattern pattern = Pattern.compile("(\\d+) (\\d+) (\\d+) (\\d+)", 0);
                Matcher matcher = pattern.matcher(line);
                if(matcher.matches()){
                    int[] instruction = new int[4];
                    for(int i = 0; i < 4; ++i)
                        instruction[i] = Integer.parseInt(matcher.group(i+1));
                    instrs.add(instruction);
                }
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return new Pair<>(samples, instrs);
    }

    public static Pair<Integer, SampleTest> handleSamples(Day16 obj, ChronalDevice cd, ArrayList<Sample> samples){
        Pair<Integer, SampleTest> results = new Pair<>(0, obj.new SampleTest());
        for(Sample sample: samples){
            int currentSampleCount = 0;
            ArrayList<String> candidates = new ArrayList<>();
            for(ChronalDevice.Instruction instr: cd.getInstructions()){
                int[] copy = sample.registersBefore.clone();
                int[] args = Arrays.copyOfRange(sample.instruction, 1, 4);
                instr.execute(args, copy);
                if(Arrays.equals(sample.registersAfter, copy)) {
                    currentSampleCount++;
                    candidates.add(instr.name);
                }
            }
            results.second.addCandidates(sample.instruction[0], candidates);
            if(currentSampleCount >= 3)
                results.first++;
        }
        results.second.calculateOpCodeMap(cd);
        return results;
    }

    public static void main(String[] args) {
        Day16 obj = new Day16();
        Pair<ArrayList<Sample>, ArrayList<int[]>> input = parseInput(obj, "Day16_input.txt");
        ChronalDevice cd = new ChronalDevice();
        Pair<Integer, SampleTest> results = handleSamples(obj, cd, input.first);
        for(int[] instr: input.second)
            results.second.opCodeMapping.get(instr[0]).execute(Arrays.copyOfRange(instr, 1, 4));

        System.out.println("Day 16:");
        System.out.println("Question 1: Ignoring the opcode numbers, how many samples in your puzzle input behave "
                 + "like three or more opcodes?");
        System.out.println("Answer: " + results.first);
        System.out.println("Question 2: What value is contained in register 0 after executing the test program?");
        System.out.println("Answer: " + cd.registers[0]);
    }
}
