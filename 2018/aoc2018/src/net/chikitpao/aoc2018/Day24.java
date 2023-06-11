// Day24.java
// AoC 2018 Day 24: Immune System Simulator 20XX
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

public class Day24{
    class Simulation {
        public static final int TYPE_IMMUNE_SYSTEM = 0;
        public static final int TYPE_INFECTION = 1;
        public static final int TYPE_STALEMATE = 2; // Used by getWinningArmyUnitCount when no group can attack.

        enum AttackType{
            RADIATION,
            SLASHING,
            FIRE,
            COLD,
            BLUDGEONING;

            public static AttackType fromString(String name)
            {
                if("radiation".equals(name))
                    return RADIATION;
                if("slashing".equals(name))
                    return SLASHING;
                if("fire".equals(name))
                    return FIRE;
                if("cold".equals(name))
                    return COLD;
                if("bludgeoning".equals(name))
                    return BLUDGEONING;
                throw new RuntimeException("Invalid AttackType name: " + name);
            }
        }
        class Group{

            public int type;
            public int groupNumber; // per type, one-based
            public int unitCount;
            public int hitPoints;
            public int attackDamage;
            public AttackType attackType;
            public int initiative;

            ArrayList<AttackType> weaknesses = new ArrayList<>();
            ArrayList<AttackType> immunities = new ArrayList<>();
            public Group(int type, int groupNumber, int unitCount, int hitPoints, String conditionString,
                         int attackDamage, String attackTypeName, int initiative){
                this.type = type;
                this.groupNumber = groupNumber;
                this.unitCount = unitCount;
                this.hitPoints = hitPoints;

                if(conditionString != null){
                    String[] conditions = conditionString.substring(conditionString.indexOf('(') + 1,
                            conditionString.indexOf(')')).split("; ");
                    for(String condition: conditions){
                        ArrayList<AttackType> conditionGroup;
                        String[] conditionTypes;
                        if(condition.startsWith("immune to ")){
                            conditionGroup = immunities;
                            conditionTypes = condition.substring("immune to ".length()).split(", ");
                        } else if (condition.startsWith("weak to ")){
                            conditionGroup = weaknesses;
                            conditionTypes = condition.substring("weak to ".length()).split(", ");
                        } else {
                            throw new RuntimeException("Invalid condition: " + condition);
                        }
                        for(String conditionType: conditionTypes)
                            conditionGroup.add(AttackType.fromString(conditionType));
                    }
                }

                this.attackDamage = attackDamage;
                this.attackType = AttackType.fromString(attackTypeName);
                this.initiative = initiative;
            }
            public Group(Group other){
                this.type = other.type;
                this.groupNumber = other.groupNumber;
                this.unitCount = other.unitCount;
                this.hitPoints = other.hitPoints;
                this.immunities = (ArrayList<AttackType>) other.immunities.clone();
                this.weaknesses = (ArrayList<AttackType>) other.weaknesses.clone();
                this.attackDamage = other.attackDamage;
                this.attackType = other.attackType;
                this.initiative = other.initiative;
            }
            public int getEffectivePower(){
                return unitCount * attackDamage;
            }

            public int getExpectedDamage(Group opponent) {
                if(opponent.immunities.contains(attackType))
                    return 0;
                int damage = unitCount * attackDamage;
                if(opponent.weaknesses.contains(attackType))
                    damage *= 2;
                return damage;
            }

            public boolean attack(Group opponent) {
                if(unitCount == 0)
                    return false;
                if(opponent == null || opponent.unitCount == 0)
                    return false;

                int damage = getExpectedDamage(opponent);
                // It shouldn't happen since this case has already been handled in the target selection phase.
                if(damage == 0)
                    throw new RuntimeException("Attack with 0 damage!");

                int unitLoss = Math.min(damage / opponent.hitPoints, opponent.unitCount);
                // Too weak to cause damage.
                if(unitLoss == 0)
                    return false;
                opponent.unitCount -= unitLoss;
                return true;
            }
        }

        ArrayList<Group> immuneSystem = new ArrayList<>();
        ArrayList<Group> immuneSystemCopy = new ArrayList<>();
        ArrayList<Group> infections = new ArrayList<>();
        ArrayList<Group> infectionsCopy = new ArrayList<>();

        // type of winning army, total unit count
        public Pair<Integer, Integer> getWinningArmyUnitCount() {
            Pair<Integer, Integer> result = new Pair<>(TYPE_IMMUNE_SYSTEM, 0);
            if(immuneSystem.isEmpty() == infections.isEmpty()){
                result.first = TYPE_STALEMATE;
                result.second = -1;
                return result;
            }
            ArrayList<Group> winningArmy;
            if(!immuneSystem.isEmpty()){
                winningArmy = immuneSystem;
                result.first = TYPE_IMMUNE_SYSTEM;
            } else {
                winningArmy = infections;
                result.first = TYPE_INFECTION;
            }
            result.second = winningArmy.stream().reduce(0, (subTotal, group) -> subTotal + group.unitCount,
                    Integer::sum);
            return result;
        }
        public void parseInputLines(ArrayList<String> inputLines) {
            // Input file contains blocks for "Immune System:" and "Infection:"
            // Example line:
            // 2153 units each with 14838 hit points (immune to fire, bludgeoning, radiation; weak to slashing)
            // with an attack that does 11 radiation damage at initiative 3
            Pattern pattern = Pattern.compile("(\\d+) units each with (\\d+) hit points (\\(.*\\) )?" +
                    "with an attack that does (\\d+) (\\w+) damage at initiative (\\d+)", 0);
            int[] sectionBegins = new int[]{inputLines.indexOf("Immune System:"), inputLines.indexOf("Infection:")};
            int count = 0;
            boolean handlingInfection = false;
            for(int lineIndex = 0; lineIndex < inputLines.size(); ++lineIndex){
                if(lineIndex == sectionBegins[0])
                    continue;
                if(lineIndex == sectionBegins[1]){
                    handlingInfection = true;
                    count = 0;
                    continue;
                }
                Matcher matcher = pattern.matcher(inputLines.get(lineIndex));
                if (matcher.matches()) {
                    count++;
                    ArrayList<Group> group = (!handlingInfection) ? immuneSystem : infections;
                    ArrayList<Group> groupCopy = (!handlingInfection) ? immuneSystemCopy : infectionsCopy;
                    int type = (!handlingInfection) ? TYPE_IMMUNE_SYSTEM : TYPE_INFECTION;
                    Group newGroup = new Group(type, count, Integer.parseInt(matcher.group(1)),
                            Integer.parseInt(matcher.group(2)), matcher.group(3), Integer.parseInt(matcher.group(4)),
                            matcher.group(5), Integer.parseInt(matcher.group(6)));
                    group.add(newGroup);
                    groupCopy.add(new Group(newGroup));
                } else {
                    System.err.println("No match: " + inputLines.get(lineIndex));
                }
            }
        }
        public void run() {
            run(0);
        }
        public void run(int boost) {
            if(boost > 0){
                for(Group group: immuneSystem){
                    group.attackDamage += boost;
                }
            }
            while(!immuneSystem.isEmpty() && !infections.isEmpty()) {
                boolean attackOccurred = false;
                ArrayList<Group> attackingGroups = new ArrayList<>(immuneSystem);
                attackingGroups.addAll(infections);
                ArrayList<Group> attackedGroups = new ArrayList<>();

                // 1. Target selection phase
                // Sort by target selection order for attackers
                attackingGroups.sort((lhs, rhs) -> {
                    if(lhs.getEffectivePower() != rhs.getEffectivePower())
                        return rhs.getEffectivePower() - lhs.getEffectivePower();
                    return rhs.initiative - lhs.initiative;
                });
                // Sort by target preference of attackers and choose the first one
                for(Group attacker: attackingGroups){
                    ArrayList<Group> potentialTargets = new ArrayList<>(immuneSystem);
                    potentialTargets.addAll(infections);
                    potentialTargets.remove(attacker);
                    potentialTargets.removeAll(attackedGroups);
                    potentialTargets.removeIf(e -> e.type == attacker.type);

                    if(potentialTargets.isEmpty()){
                        attackedGroups.add(null);
                        continue;
                    }

                    potentialTargets.sort((lhs, rhs) -> {
                        if(attacker.getExpectedDamage(lhs) != attacker.getExpectedDamage(rhs))
                            return attacker.getExpectedDamage(rhs) - attacker.getExpectedDamage(lhs);
                        if(lhs.getEffectivePower() != rhs.getEffectivePower())
                            return rhs.getEffectivePower() - lhs.getEffectivePower();
                        return rhs.initiative - lhs.initiative;
                    });
                    if(attacker.getExpectedDamage(potentialTargets.get(0)) == 0)
                        attackedGroups.add(null);
                    else
                        attackedGroups.add(potentialTargets.remove(0));
                }
                if(attackingGroups.size() != attackedGroups.size())
                    throw new RuntimeException("attackingGroups.size() != attackedGroups.size(): " +
                            attackingGroups.size() + " != " + attackedGroups.size());

                // 2. Attacking phase
                // Sort attacks by attackers initiative
                ArrayList<Pair<Group, Group>> attackSequence = new ArrayList<>();
                for(int i = 0; i < attackingGroups.size(); ++i)
                    attackSequence.add(new Pair<>(attackingGroups.get(i), attackedGroups.get(i)));
                attackSequence.sort((lhs, rhs) -> rhs.first.initiative - lhs.first.initiative);

                for(Pair<Group, Group> attack: attackSequence){
                    boolean attackJustOccurred = attack.first.attack(attack.second);
                    attackOccurred = attackOccurred || attackJustOccurred;
                }

                immuneSystem.removeIf(e -> e.unitCount == 0);
                infections.removeIf(e -> e.unitCount == 0);

                // Stalemate situation. No group can attack!
                if(!attackOccurred)
                    break;
            }
        }
        public void runWithBoost() {
            reset();

            // Find result per binary search
            int iterateStep = 1000;
            // Boundary value, victory
            Pair<Integer, Boolean> lowerBound = new Pair<>(0, false);
            Pair<Integer, Boolean> upperBound = new Pair<>(iterateStep, false);
            boolean foundUpperBound = false;
            int currentValue = upperBound.first;
            while(true){
                run(currentValue);
                Pair<Integer, Integer> result = getWinningArmyUnitCount();
                if(result.first == TYPE_IMMUNE_SYSTEM) {
                    if(!foundUpperBound)
                        foundUpperBound = true;
                    upperBound.first = currentValue;
                    upperBound.second = true;
                    if(upperBound.first - lowerBound.first == 1)
                        return;
                    currentValue = (upperBound.first + lowerBound.first) / 2;
                } else if (result.first == TYPE_INFECTION){
                    if(!foundUpperBound) {
                        lowerBound.first = upperBound.first;
                        upperBound.first += iterateStep;
                        currentValue = upperBound.first;
                    } else {
                        lowerBound.first = currentValue;
                        lowerBound.second = false;
                        if(upperBound.first - lowerBound.first == 1){
                            // upperBound.first is the desired value. Rerun with this value for correct internal states.
                            reset();
                            run(upperBound.first);
                            if(getWinningArmyUnitCount().first != TYPE_IMMUNE_SYSTEM)
                                throw new RuntimeException("Run with boost finished but failed to get Winning Army "
                                        + "Unit Count!");
                            return;
                        }
                        currentValue = (upperBound.first + lowerBound.first) / 2;
                    }
                } else {
                    if(result.first != TYPE_STALEMATE)
                        throw new RuntimeException("Unknown result: " + result.first);
                    if(upperBound.second && !lowerBound.second){
                        lowerBound.first = currentValue;
                        lowerBound.second = false;
                        if(upperBound.first - lowerBound.first == 1){
                            // upperBound.first is the desired value. Rerun with this value for correct internal states.
                            reset();
                            run(upperBound.first);
                            if(getWinningArmyUnitCount().first != TYPE_IMMUNE_SYSTEM)
                                throw new RuntimeException("Run with boost finished but failed to get Winning Army "
                                        + "Unit Count!");
                            return;
                        }
                        currentValue = (upperBound.first + lowerBound.first) / 2;
                    } else {
                        throw new RuntimeException("Cannot handle stalemate situation! Current boost value: "
                                + currentValue);
                    }
                }
                reset();
            }
        }

        private void reset() {
            immuneSystem.clear();
            for(Group group: immuneSystemCopy)
                immuneSystem.add(new Group(group));
            infections.clear();
            for(Group group: infectionsCopy)
                infections.add(new Group(group));
        }
    }

    public static ArrayList<String> parseInput(String fileName){
        ArrayList<String> lines = new ArrayList<>();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            while ((line = reader.readLine()) != null) {
                if(!line.isEmpty())
                    lines.add(line);
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return lines;
    }

    static String testInput = "Immune System:\n" +
            "17 units each with 5390 hit points (weak to radiation, bludgeoning) with" +
            " an attack that does 4507 fire damage at initiative 2\n" +
            "989 units each with 1274 hit points (immune to fire; weak to bludgeoning," +
            " slashing) with an attack that does 25 slashing damage at initiative 3\n" +
            "Infection:\n" +
            "801 units each with 4706 hit points (weak to radiation) with an attack" +
            " that does 116 bludgeoning damage at initiative 1\n" +
            "4485 units each with 2961 hit points (immune to radiation; weak to fire," +
            " cold) with an attack that does 12 slashing damage at initiative 4\n";

    public static void main(String[] args) throws Exception {
        Day24 obj = new Day24();

        System.out.println("Day 24:");

        Simulation testSimulation = obj.new Simulation();
        testSimulation.parseInputLines(new ArrayList<>(Arrays.stream(testInput.split("\n")).toList()));
        testSimulation.run();
        System.out.println("## Answer 1 of test input: " + testSimulation.getWinningArmyUnitCount().second);
        testSimulation.runWithBoost();
        System.out.println("## Answer 2 of test input: " + testSimulation.getWinningArmyUnitCount().second);

        ArrayList<String> inputLines = parseInput("Day24_input.txt");
        Simulation simulation = obj.new Simulation();
        simulation.parseInputLines(inputLines);
        simulation.run();

        System.out.println("Question 1: As it stands now, how many units would the winning army have?");
        System.out.println("Answer: " + simulation.getWinningArmyUnitCount().second);

        System.out.println("Question 2: How many units does the immune system have left after getting the smallest "
                + "boost it needs to win?");
        simulation.runWithBoost();
        System.out.println("Answer: " + simulation.getWinningArmyUnitCount().second);
    }
}
