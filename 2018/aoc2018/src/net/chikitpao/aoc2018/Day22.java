// Day22.java
// AoC 2018 Day 22: Mode Maze
// Author: Chi-Kit Pao
//

package net.chikitpao.aoc2018;

import net.chikitpao.util.Pos;

import java.util.*;

public class Day22 {
    class Tile{
        int geoIndex;
        int erosionLevel;
        int type;
        public void setGeoIndex(int index, int depth){
            geoIndex = index;
            erosionLevel = ((geoIndex + depth) % 20183);
            type = erosionLevel % 3;
        }
    }

    enum Equipment{
        NEITHER,
        TORCH,
        CLIMBING_GEAR;
    }
    class SearchNode{
        Pos pos;
        Equipment equipment;
        SearchNode north;
        SearchNode east;
        SearchNode south;
        SearchNode west;
        SearchNode equipmentChange;
        SearchNode predecessor;
        int currentWeight = 0;  // minutes
        SearchNode(Pos pos, Equipment equipment){
            this.pos = pos;
            this.equipment = equipment;
        }
        @Override
        public boolean equals(Object obj) {
            if(obj == null)
                return false;
            if(!(obj instanceof SearchNode))
                return false;
            return pos.y == ((SearchNode) obj).pos.y && pos.x == ((SearchNode) obj).pos.x
                    && equipment == ((SearchNode) obj).equipment;
        }
        @Override
        public int hashCode(){
            // Must the factor be prime?
            return pos.hashCode() * 1021 + equipment.ordinal();
        }
    }

    class DijkstraSearch {
        int width;
        int height;
        ArrayList<SearchNode[][]> searchNodes;
        SearchNode startNode;
        SearchNode endNode;
        HashSet<SearchNode> unvisited = new HashSet<>();
        LinkedList<SearchNode> processing = new LinkedList<>();
        HashSet<SearchNode> visited = new HashSet<>();
        DijkstraSearch(Day22 obj, ArrayList<Tile[]> grid, Pos targetPos){
            // Create 2 states (= nodes) per tile to reflect equipment usage.
            // Connect these nodes properly (weight is determined during search).
            // Use Dijkstra algorithm to find minimum time needed to reach the target.
            // Start: Position (0, 0), equipped with torch.
            // End: Target Position, equipped with torch.
            width = grid.get(0).length;
            height = grid.size();
            searchNodes = new ArrayList<>();
            for(int i = 0; i < grid.size(); ++i) {
                Tile[] row = grid.get(i);
                SearchNode[][] searchNodeRow = new SearchNode[width][3];
                searchNodes.add(searchNodeRow);
                for (int j = 0; j < width; ++j) {
                    switch(row[j].type){
                        case 0: // rocky
                        {
                            createNodes(obj, i, j, Equipment.TORCH, Equipment.CLIMBING_GEAR, searchNodeRow);
                            if(i == 0 && j == 0)
                                startNode = searchNodeRow[j][Equipment.TORCH.ordinal()];
                            else if (i == targetPos.y && j == targetPos.x)
                                endNode = searchNodeRow[j][Equipment.TORCH.ordinal()];
                        }
                        break;
                        case 1: // wet
                            createNodes(obj, i, j, Equipment.NEITHER, Equipment.CLIMBING_GEAR, searchNodeRow);
                        break;
                        case 2: // narrow
                            createNodes(obj, i, j, Equipment.NEITHER, Equipment.TORCH, searchNodeRow);
                        break;
                    }
                }
            }
            for(int i = 0; i < height; ++i) {
                SearchNode[][] searchNodeRowPrev = (i == 0) ? null : searchNodes.get(i - 1);
                SearchNode[][] searchNodeRow = searchNodes.get(i);
                SearchNode[][] searchNodeRowNext = (i == height - 1) ? null : searchNodes.get(i + 1);
                for (int j = 0; j < searchNodeRow.length; ++j) {
                    for(int equipment = 0; equipment < Equipment.values().length; ++equipment){
                        if(searchNodeRow[j][equipment] == null)
                            continue;
                        if(i > 0 && searchNodeRowPrev[j][equipment] != null)
                            searchNodeRow[j][equipment].north = searchNodeRowPrev[j][equipment];
                        if((i + 1 < height) && searchNodeRowNext[j][equipment] != null)
                            searchNodeRow[j][equipment].south = searchNodeRowNext[j][equipment];
                        if(j > 0 && searchNodeRow[j - 1][equipment] != null)
                            searchNodeRow[j][equipment].west = searchNodeRow[j - 1][equipment];
                        if((j + 1 < width) && searchNodeRow[j + 1][equipment] != null)
                            searchNodeRow[j][equipment].east = searchNodeRow[j + 1][equipment];
                    }
                }
            }
        }
        public int calculateTime(Day22 obj){
            SearchNode currentNode = startNode;
            unvisited.remove(currentNode);
            boolean finished;
            ArrayList<SearchNode> neighbors = new ArrayList<>(Collections.nCopies(5, null));
            while(currentNode != null && currentNode != endNode){
                finished = true;
                if(currentNode.east == null && (currentNode.pos.x + 1 >= width))
                    throw new RuntimeException("Reached right border!");
                if(currentNode.south == null && (currentNode.pos.y + 1 >= height))
                    throw new RuntimeException("Reached bottom border!");
                neighbors.set(0, currentNode.north);
                neighbors.set(1, currentNode.east);
                neighbors.set(2, currentNode.south);
                neighbors.set(3, currentNode.west);
                neighbors.set(4, currentNode.equipmentChange);
                for(SearchNode neighbor: neighbors){
                    if(neighbor == null)
                        continue;
                    int newWeight = (neighbor == currentNode.equipmentChange) ?
                            (currentNode.currentWeight + 7) : (currentNode.currentWeight + 1);
                    if(unvisited.contains(neighbor)){
                        unvisited.remove(neighbor);
                        neighbor.currentWeight = newWeight;
                        neighbor.predecessor = currentNode;
                        processing.add(neighbor);
                        finished = false;
                    } else if(processing.contains(neighbor)) {
                        if(newWeight < neighbor.currentWeight) {
                            neighbor.currentWeight = newWeight;
                            neighbor.predecessor = currentNode;
                            finished = false;
                        }
                    }
                }

                visited.add(currentNode);
                if(!processing.isEmpty()) {
                    processing.sort(Comparator.comparing(e -> e.currentWeight));
                    currentNode = processing.get(0);
                    processing.remove(0);
                    finished = false;
                } else {
                    currentNode = null;
                }
                if(finished)
                    break;
            }
            if(currentNode == endNode)
                return endNode.currentWeight;
            return -1;
        }
        private void createNodes(Day22 obj, int row, int column, Equipment equipment1, Equipment equipment2,
                                 SearchNode[][] searchNodeRow) {
            SearchNode node1 = obj.new SearchNode(new Pos(row, column), equipment1);
            SearchNode node2 = obj.new SearchNode(new Pos(row, column), equipment2);
            node1.equipmentChange = node2;
            node2.equipmentChange = node1;
            searchNodeRow[column][node1.equipment.ordinal()] = node1;
            searchNodeRow[column][node2.equipment.ordinal()] = node2;
            unvisited.add(node1);
            unvisited.add(node2);
        }
    }

    public static ArrayList<Tile[]> calculateGrid(Day22 obj, int depth, Pos targetPos){
        // Rough estimate for dimension needed for part two:
        // 2 times might just cover the Manhattan distance to target.
        // So use 4 times to have some buffer.
        // Application shall still throw exception when trespassing right / bottom border.
        final int DIMENSION = Math.max(targetPos.y, targetPos.x) * 4;
        final int rowCount = DIMENSION;
        final int columnCount = DIMENSION;
        ArrayList<Tile[]> rows = new ArrayList<>(rowCount);
        for(int i = 0; i < rowCount; ++i) {
            Tile[] row = new Tile[columnCount];
            rows.add(row);
            // Set geologic index
            for (int j = 0; j < columnCount; ++j) {
                row[j] = obj.new Tile();
                if (i == targetPos.y && j == targetPos.x) {
                    row[j].setGeoIndex(0, depth);
                } else if (i == 0) {
                    row[j].setGeoIndex(j * 16807, depth); // (0, 0) will still have value 0.
                } else if (j == 0) {
                    row[j].setGeoIndex(i * 48271, depth);
                } else {
                    row[j].setGeoIndex(row[j - 1].erosionLevel * rows.get(i - 1)[j].erosionLevel, depth);
                }
            }
        }
        return rows;
    }

    public static int calculateRiskLevel(ArrayList<Tile[]> grid, Pos targetPos){
        int sum = 0;
        for(int i = 0; i <= targetPos.y; ++i) {
            Tile row[] = grid.get(i);
            // geologic index
            for(int j = 0; j <= targetPos.x; ++j){
                sum += row[j].type;
            }
        }
        return sum;
    }

    public static void main(String[] args) {
        Day22 obj = new Day22();
        Pos testTargetPos = new Pos(10, 10);
        ArrayList<Tile[]> testGrid = calculateGrid(obj, 510, testTargetPos);
        if(calculateRiskLevel(testGrid, testTargetPos) != 114)
            throw new RuntimeException("Wrong value for test input!");
        DijkstraSearch testSearch = obj.new DijkstraSearch(obj, testGrid, testTargetPos);
        if(testSearch.calculateTime(obj) != 45)
            throw new RuntimeException("Wrong value for test input!");

        Pos targetPos = new Pos(718, 11);
        ArrayList<Tile[]> grid = calculateGrid(obj, 11739, targetPos);

        System.out.println("Day 22:");
        System.out.println("Question 1: What is the total risk level for the smallest rectangle that includes 0,0 "
                + "and the target's coordinates?");
        System.out.println("Answer: " + calculateRiskLevel(grid, targetPos));
        System.out.println("Question 2: What is the fewest number of minutes you can take to reach the target?");
        long start = System.currentTimeMillis();
        DijkstraSearch search = obj.new DijkstraSearch(obj, grid, targetPos);
        System.out.println("Answer: " + search.calculateTime(obj));
        long finish = System.currentTimeMillis();
        long timeElapsed = finish - start;
        // Output: ## timeElapsed: 169745
        System.out.println("## timeElapsed: " + timeElapsed);
    }
}
