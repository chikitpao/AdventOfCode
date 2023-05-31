// Day20.java
// AoC 2018 Day 20: A Regular Map
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import net.chikitpao.util.Pair;
import net.chikitpao.util.Pos;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Day20{
    class BaseMap{
        HashSet<Pos> doors = new HashSet<>();
        HashSet<Pos> rooms = new HashSet<>();

        public BaseMap(String input){
            if(input.charAt(0) != '^' || input.charAt(input.length() - 1) != '$')
                throw new RuntimeException("Invalid input");
            HashSet<Pos> currentPositions = new HashSet<>();
            Pos startPos = new Pos(0, 0);
            rooms.add(startPos);
            currentPositions.add(startPos);
            int caret = 0;
            while(caret < input.length()){
                caret = parseNextToken(currentPositions, input, caret);
            }
        }

        private int parseNextToken(HashSet<Pos> currentPositions, String input, int caret) {
            switch(input.charAt(caret)){
                case 'N':
                    for(Pos currentPosition: currentPositions){
                        addDoor(currentPosition.y - 1, currentPosition.x);
                        currentPosition.y -= 2;
                        addRoom(new Pos(currentPosition));
                    }
                    return caret + 1;
                case 'E':
                    for(Pos currentPosition: currentPositions){
                        addDoor(currentPosition.y, currentPosition.x + 1);
                        currentPosition.x += 2;
                        addRoom(new Pos(currentPosition));
                    }
                    return caret + 1;
                case 'S':
                    for(Pos currentPosition: currentPositions){
                        addDoor(currentPosition.y + 1, currentPosition.x);
                        currentPosition.y += 2;
                        addRoom(new Pos(currentPosition));
                    }
                    return caret + 1;
                case 'W':
                    for(Pos currentPosition: currentPositions){
                        addDoor(currentPosition.y, currentPosition.x - 1);
                        currentPosition.x -= 2;
                        addRoom(new Pos(currentPosition));
                    }
                    return caret + 1;
                case '(':
                    return parseBranches(currentPositions, input, caret);
                case '^':
                case '$':
                    return caret + 1;
                default:
                    return input.length();
            }
        }

        private int parseBranches(HashSet<Pos> currentPositions, String input, int caret) {
            int bracketStart = caret;
            int bracketEnd = -1;
            int bracketLevel = 0;
            ArrayList<Integer> subStringPositions = new ArrayList<>();
            for(int i = caret; i < input.length(); ++i){
                char current = input.charAt(i);
                if(current == '(') {
                    bracketLevel++;
                    if(bracketLevel == 1)
                        bracketStart = i;
                } else if (current == ')'){
                    if(bracketLevel == 1){
                        bracketEnd = i;
                        break;
                    }
                    bracketLevel--;
                } else if (current == '|'){
                    if(bracketLevel == 1) {
                        subStringPositions.add(i);
                        subStringPositions.add(i);
                    }
                }
            }
            subStringPositions.add(0, bracketStart);
            subStringPositions.add(bracketEnd);
            ArrayList<Pos> tempPositions = new ArrayList<>();
            for(int i = 0; i < subStringPositions.size(); i+=2){
                int startIncl = subStringPositions.get(i) + 1;
                int endExcl = subStringPositions.get(i + 1);
                for(Pos currentPosition: currentPositions){
                    Pos tempPosition = new Pos(currentPosition);
                    if(startIncl == endExcl){
                        tempPositions.add(tempPosition);
                        continue;
                    }
                    int j = startIncl;
                    HashSet<Pos> tempPositions2 = new HashSet<>();
                    tempPositions2.add(tempPosition);
                    while(j < endExcl)
                        j = parseNextToken(tempPositions2, input, j);
                    tempPositions.addAll(tempPositions2);
                }
            }
            currentPositions.clear();
            currentPositions.addAll(tempPositions);
            return bracketEnd + 1;
        }

        private void addDoor(int y, int x) {
            doors.add(new Pos(y, x));
        }
        private void addRoom(Pos pos) {
            rooms.add(new Pos(pos));
        }

        public int getMaximumPath() {
            int steps = 0;
            HashSet<Pos> newPositions = new HashSet<>();
            HashSet<Pos> processing = new HashSet<>();
            HashSet<Pos> visited = new HashSet<>();
            processing.add(new Pos(0, 0));
            boolean foundNewPos;
            do {
                foundNewPos = false;
                for(Pos pos: processing) {
                    ArrayList<Pos> neighbors = getNeighbors(pos);
                    for(Pos neighbor: neighbors){
                        if(!visited.contains(neighbor) && !processing.contains(neighbor))
                            newPositions.add(neighbor);
                    }
                }
                if (!newPositions.isEmpty()) {
                    foundNewPos = true;
                    steps++;
                    visited.addAll(processing);
                    processing.clear();
                    processing.addAll(newPositions);
                    newPositions.clear();
                }
            } while(foundNewPos);
            return steps;
        }

        public int getRoomCount(int minimumDoorCount) {
            int steps = 0;
            HashSet<Pos> newPositions = new HashSet<>();
            HashSet<Pos> processing = new HashSet<>();
            HashSet<Pos> visited = new HashSet<>();
            processing.add(new Pos(0, 0));
            boolean foundNewPos;
            do {
                foundNewPos = false;
                for(Pos pos: processing) {
                    ArrayList<Pos> neighbors = getNeighbors(pos);
                    for(Pos neighbor: neighbors){
                        if(!visited.contains(neighbor) && !processing.contains(neighbor))
                            newPositions.add(neighbor);
                    }
                }
                if (!newPositions.isEmpty()) {
                    foundNewPos = true;
                    steps++;
                    visited.addAll(processing);
                    processing.clear();
                    processing.addAll(newPositions);
                    newPositions.clear();
                    if(steps + 1 == minimumDoorCount)
                        return rooms.size() - visited.size() - processing.size();
                }
            } while(foundNewPos);
            return 0; // if steps is less than minimumDoorCount
        }

        private ArrayList<Pos> getNeighbors(Pos pos) {
            ArrayList<Pos> result = new ArrayList<>();
            Pos door = new Pos(pos);
            door.y = pos.y + 1;
            if(doors.contains(door))
                result.add(new Pos(pos.y + 2, pos.x));
            door.y = pos.y - 1;
            if(doors.contains(door))
                result.add(new Pos(pos.y - 2, pos.x));
            door.y = pos.y;
            door.x = pos.x + 1;
            if(doors.contains(door))
                result.add(new Pos(pos.y, pos.x + 2));
            door.x = pos.x - 1;
            if(doors.contains(door))
                result.add(new Pos(pos.y, pos.x - 2));
            return result;
        }
    }
    public static String getInputLine(String fileName){
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            if((line = reader.readLine()) != null)
                return line;
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return "";
    }

    public static void main(String[] args) throws Exception {
        Day20 obj = new Day20();
        ArrayList<Pair<String, Integer>> testInputs = new ArrayList<>();
        testInputs.add(new Pair<>("^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$", 23));
        testInputs.add(new Pair<>("^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$", 31));
        for(Pair<String, Integer> testInput: testInputs){
            BaseMap testMap = obj.new BaseMap(testInput.first);
            if(testMap.getMaximumPath() != testInput.second)
                throw new RuntimeException("Incorrect answer for test input!");
        }

        String input = getInputLine("Day20_input.txt");
        System.out.println("Day 20:");
        System.out.println("Question 1: What is the largest number of doors you would be required to pass through to "
                + "reach a room?");
        BaseMap baseMap = obj.new BaseMap(input);
        System.out.println("Answer: " + baseMap.getMaximumPath());
        System.out.println("Question 2: How many rooms have a shortest path from your current location that pass "
                + "through at least 1000 doors?");
        System.out.println("Answer: " + baseMap.getRoomCount(1000));
    }
}
