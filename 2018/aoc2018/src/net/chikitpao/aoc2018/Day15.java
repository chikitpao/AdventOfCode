// Day15.java
// AoC 2018 Day 15: Beverage Bandits
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;


import net.chikitpao.util.Pos;

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static java.util.stream.Collectors.toCollection;


public class Day15 {
    public class Combat{
        public int mapWidth;
        public int mapHeight;


        public int run(int elvesAttackPower, boolean isHelping) {
            elves.forEach(e -> e.attackPower = elvesAttackPower);
            int elvesCount = elves.size();
            int rounds = 0;
            while(true){
                units.clear();
                units.addAll(elves);
                units.addAll(goblins);
                units.sort(Comparator.comparing(e -> getPosScore(e.pos)));
                for(int i = 0; i < units.size(); ++i){
                    Unit unit = units.get(i);
                    if(unit == null)
                        continue;
                    unit.move(this);
                    if(isHelping && (elves.size() < elvesCount))
                        return -1;
                    if(elves.isEmpty()){
                        int a = (units.stream().skip(i + 1).filter(e -> (e != null)).findAny().isPresent()) ? 0 : 1;
                        return (rounds + a) * goblins.stream().map(e -> e.hitPoints).reduce(0, Integer::sum);
                    } else if(goblins.isEmpty()){
                        int a = (units.stream().skip(i + 1).filter(e -> (e != null)).findAny().isPresent()) ? 0 : 1;
                        return (rounds + a) * elves.stream().map(e -> e.hitPoints).reduce(0, Integer::sum);
                    }
                }
                rounds++;
            }
        }

        private static boolean isAdjacent(Pos lhs, Pos rhs) {
            if(lhs.y == rhs.y)
                return Math.abs(lhs.x - rhs.x) == 1;
            else if(lhs.x == rhs.x)
                return Math.abs(lhs.y - rhs.y) == 1;
            return false;
        }
        private int getPosScore(Pos pos){
            return pos.y * mapWidth + pos.x;
        }

        public class Unit {
            char type;
            Pos pos;
            ArrayList<Unit> foes;
            public int attackPower = 3;
            public int hitPoints = 200;


            public Unit(char type, int x, int y, ArrayList<Unit> foes) {
                this.type = type;
                this.pos = new Pos(y, x);
                this.foes = foes;
            }

            public void move(Combat combat) {
                // Attack
                if(attack(combat))
                    return;

                // Move:
                HashSet<Pos> foesNeighborhood = new HashSet<>();
                for(Unit foe: foes)
                    foesNeighborhood.addAll(combat.getValidNeighborhood(foe.pos));
                // In range and nearest
                ArrayList<Pos> targetedPositions = combat.searchBfs(pos, foesNeighborhood);
                if(targetedPositions.isEmpty())
                    return;
                Pos targetedPosition = targetedPositions.get(0);
                // Distance:
                // Step:
                HashSet<Pos> firstSteps = combat.getValidNeighborhood(pos).stream()
                        .collect(Collectors.toCollection(HashSet::new));
                ArrayList<Pos> startPositions = combat.searchBfs(targetedPosition, firstSteps);
                if(startPositions.isEmpty())
                    return;
                map.get(pos.y).set(pos.x, '.');
                pos = startPositions.get(0);
                map.get(pos.y).set(pos.x, type);
                // Move and attack is allowed!
                attack(combat);
            }

            private boolean attack(Combat combat) {
                ArrayList<Unit> surrounding = foes.stream().filter(e -> Combat.isAdjacent(this.pos, e.pos))
                        .collect(toCollection(ArrayList::new));
                if(surrounding.isEmpty())
                    return false;
                if(surrounding.size() > 1)
                    surrounding.sort(Comparator.comparing(e
                            -> (e.pos.x + mapWidth * (e.pos.y + e.hitPoints * mapHeight))));
                Unit foe = surrounding.get(0);
                foe.hitPoints -= attackPower;
                if(foe.hitPoints <= 0) {
                    int index = combat.units.indexOf(foe);
                    combat.units.set(index, null);
                    combat.map.get(foe.pos.y).set(foe.pos.x, '.');
                    foes.remove(foe);
                }
                return true;
            }
        }
        private ArrayList<Pos> getValidNeighborhood(Pos pos) {
            ArrayList<Pos> temp = new ArrayList<>();
            temp.add(new Pos(pos.y - 1, pos.x));
            temp.add(new Pos(pos.y + 1, pos.x));
            temp.add(new Pos(pos.y, pos.x - 1));
            temp.add(new Pos(pos.y, pos.x + 1));
            Predicate<Pos> isValid = pos2 -> {
                if(pos2.x < 0 || pos2.x >= mapWidth)
                    return false;
                if(pos2.y < 0 || pos2.y >= mapHeight)
                    return false;
                if(map.get(pos2.y).get(pos2.x) != '.')
                    return false;
                return true;
            };
            return temp.stream().filter(isValid::test).collect(toCollection(ArrayList::new));
        }
        private ArrayList<Pos> searchBfs(Pos startPos, HashSet<Pos> endPositions) {
            ArrayList<Pos> targetedPositions;

            if(endPositions.contains(startPos)){
                targetedPositions = new ArrayList<>();
                targetedPositions.add(startPos);
                return targetedPositions;
            }

            boolean foundNewPos;
            HashSet<Pos> recentlyVisited = new HashSet<>();
            HashSet<Pos> visited = new HashSet<>();
            visited.add(startPos);
            do {
                for(Pos visitedPos: visited)
                    recentlyVisited.addAll(getValidNeighborhood(visitedPos).stream()
                            .filter(e -> !visited.contains(e)).toList());
                foundNewPos = !recentlyVisited.isEmpty();
                targetedPositions = recentlyVisited.stream()
                        .filter(e -> endPositions.contains(e)).collect(toCollection(ArrayList::new));
                // Chosen:
                if(!targetedPositions.isEmpty()){
                    if(targetedPositions.size() > 1)
                        targetedPositions.sort(Comparator.comparing(e -> getPosScore(e)));
                    break;
                }
                visited.addAll(recentlyVisited);
                recentlyVisited.clear();
            } while(foundNewPos);
            return targetedPositions;
        }

        public ArrayList<Unit> units = new ArrayList<>();
        public ArrayList<Unit> elves = new ArrayList<>();
        public ArrayList<Unit> goblins = new ArrayList<>();
        public ArrayList<ArrayList<Character>> map = new ArrayList<>();
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
        return lines;
    }

    private static Combat parseInputLines(Day15 obj, ArrayList<String> lines) {
        Combat combat = obj.new Combat();
        int row = 0;
        for(String line: lines){
            combat.mapWidth = line.length();
            for(int col = 0; col < line.length(); ++col){
                char c = line.charAt(col);
                if(c == 'E')
                    combat.elves.add(combat.new Unit(c, col, row, combat.goblins));
                else if(c == 'G')
                    combat.goblins.add(combat.new Unit(c, col, row, combat.elves));
            }
            combat.map.add(line.chars().mapToObj(i -> (char) i).collect(toCollection(ArrayList::new)));
            row++;
        }
        combat.mapHeight = row;
        return combat;
    }

    public static void main(String[] args) {
        Day15 obj = new Day15();
        System.out.println("Day 15:");

        for(int i = 0; i < Day15Tests.answers.length; ++i){
            ArrayList<String> lines = Day15Tests.getPuzzle(i);
            Combat combat = parseInputLines(obj, lines);
            int answer = combat.run(3, false);
            if(answer != Day15Tests.answers[i]){
                System.out.format("Expected %d retrieved %d%n", Day15Tests.answers[i], answer);
                throw new RuntimeException("Incorrect answer");
            }
        }

        ArrayList<String> lines = parseInput("Day15_input.txt");
        Combat combat = parseInputLines(obj, lines);
        int answer1 = combat.run(3, false);

        System.out.println("Question 1: What is the outcome of the combat described in your puzzle input?");
        System.out.println("Answer: " + answer1);

        System.out.println("Question 2: After increasing the Elves' attack power until it is just barely enough for "
                + "them to win without any Elves dying, what is the outcome of the combat described in your puzzle "
                + "input?");
        int answer2 = 0;
        int elvesAttackPower = 4;
        while(true) {
            Combat combat2 = parseInputLines(obj, lines);
            answer2 = combat2.run(elvesAttackPower, true);
            if(answer2 >= 0)
                break;
            elvesAttackPower++;
        }
        System.out.println("Answer: " + answer2);
    }
}
