// AoC 2020 Day 20: Monster Message
// Author: Chi-Kit Pao
//
// Outputs:
// # Tile count: 144
// # Side length: 12
// # Edge count: 48
// Question 1: What do you get if you multiply together the IDs of the four corner tiles?
// Answer: 15670959891893
// # upperLeftId: 2659
// Question 2: How many # are not part of a sea monster?
// Answer: 1964
//

import java.io.BufferedReader;
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.file.FileSystems;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

public class Day20 {
    // Directions
    protected static final int EAST = 0;
    protected static final int SOUTH = 1;
    protected static final int WEST = 2;
    protected static final int NORTH = 3;

    // Turns (clockwise)
    enum Turn {
        TURN_90,
        TURN_180,
        TURN_270,
    }

    // Mirroring.
    // Mirroring at diagonal keeps the corners at the diagonal unchanged!
    enum Mirror {
        MAIN_DIAGONAL,
        ANTI_DIAGONAL
    }


    class Tile {
        public Tile (long id) {
            this.id = id;
        }
        public ArrayList<ArrayList<Character>> pixels = new ArrayList<>();
        // The smaller of pattern forwards and pattern backwards
        public ArrayList<Integer> normalizedPatterns = new ArrayList<>(4);
        public long id;

        public Tile createTurnedTileUpper(HashMap<Integer, ArrayList<Long>> patternMap, int leftPattern) {
            int edgeIndex = -1;
            int leftIndex = -1;
            for(int i = 0; i < 4; ++i) {
                var pattern = normalizedPatterns.get(i);
                var neighbors = patternMap.get(pattern);
                if(neighbors.size() == 1)
                    edgeIndex = i;
                if(pattern == leftPattern)
                    leftIndex = i;
            }
            switch (edgeIndex) {
                case 0:
                    if(leftIndex == 1) {
                        return createdMirroredTile(Mirror.MAIN_DIAGONAL).createTurnedTile(Turn.TURN_180);
                    } else {
                        return createTurnedTile(Turn.TURN_270);
                    }
                case 1:
                    if(leftIndex == 2) {
                        return createdMirroredTile(Mirror.ANTI_DIAGONAL).createTurnedTile(Turn.TURN_90);
                    } else {
                        return createTurnedTile(Turn.TURN_180);
                    }
                case 2:
                    if(leftIndex == 3) {
                        return createdMirroredTile(Mirror.MAIN_DIAGONAL);
                    } else {
                        return createTurnedTile(Turn.TURN_90);
                    }
                case 3:
                    if(leftIndex == 0) {
                        return createdMirroredTile(Mirror.ANTI_DIAGONAL).createTurnedTile(Turn.TURN_270);
                    } else {
                        return this;
                    }
                default:
                    throw new RuntimeException("Wrong tile geometry!");
            }
        }

        public Tile createTurnedTileUpperLeft(HashMap<Integer, ArrayList<Long>> patternMap) {
            ArrayList<Integer> edgeIndices = new ArrayList<>();
            for(int i = 0; i < 4; ++i) {
                var pattern = normalizedPatterns.get(i);
                var neighbors = patternMap.get(pattern);
                if(neighbors.size() == 1)
                    edgeIndices.add(i);
            }
            int i1 = edgeIndices.get(0);
            switch (i1) {
                case 0:
                    if(edgeIndices.get(1) == 3) {
                        return createTurnedTile(Turn.TURN_270);
                    } else {
                        return createTurnedTile(Turn.TURN_180);
                    }
                case 1:
                    return createTurnedTile(Turn.TURN_90);
                case 2:
                    return this;
                case 3:
                default:
                    throw new RuntimeException("Wrong tile geometry!");
            }
        }

        public Tile createTurnedTileLeft(HashMap<Integer, ArrayList<Long>> patternMap, int topPattern) {
            ArrayList<Integer> edgeIndices = new ArrayList<>(2);
            int topIndex = -1;
            for(int i = 0; i < 4; ++i) {
                var pattern = normalizedPatterns.get(i);
                var neighbors = patternMap.get(pattern);
                if(neighbors.size() == 1)
                    edgeIndices.add(i);
                if(pattern == topPattern)
                    topIndex = i;
            }
            switch (topIndex) {
                case 0:
                    if(edgeIndices.contains(1)) {
                        return createdMirroredTile(Mirror.MAIN_DIAGONAL).createTurnedTile(Turn.TURN_180);
                    } else {
                        return createTurnedTile(Turn.TURN_270);
                    }
                case 1:
                    if(edgeIndices.contains(2)) {
                        return createdMirroredTile(Mirror.ANTI_DIAGONAL).createTurnedTile(Turn.TURN_90);
                    } else {
                        return createTurnedTile(Turn.TURN_180);
                    }
                case 2:
                    if(edgeIndices.contains(3)) {
                        return createdMirroredTile(Mirror.MAIN_DIAGONAL);
                    } else {
                        return createTurnedTile(Turn.TURN_90);
                    }
                case 3:
                    if(edgeIndices.contains(0)) {
                        return createdMirroredTile(Mirror.ANTI_DIAGONAL).createTurnedTile(Turn.TURN_270);
                    } else {
                        return this;
                    }
                default:
                    throw new RuntimeException("Wrong tile geometry!");
            }
        }

        public Tile createTurnedTileOther(int leftPattern, int topPattern) {
            int leftIndex = -1;
            int topIndex = -1;
            for(int i = 0; i < 4; ++i) {
                var pattern = normalizedPatterns.get(i);
                if(pattern == leftPattern)
                    leftIndex = i;
                if(pattern == topPattern)
                    topIndex = i;
            }
            switch (leftIndex) {
                case 0:
                    if(topIndex == 1) {
                        return createTurnedTile(Turn.TURN_180);
                    } else {
                        return createdMirroredTile(Mirror.ANTI_DIAGONAL).createTurnedTile(Turn.TURN_270);
                    }
                case 1:
                    if(topIndex == 2) {
                        return createTurnedTile(Turn.TURN_90);
                    } else {
                        return createdMirroredTile(Mirror.MAIN_DIAGONAL).createTurnedTile(Turn.TURN_180);
                    }
                case 2:
                    if(topIndex == 3) {
                        return this;
                    } else {
                        return createdMirroredTile(Mirror.ANTI_DIAGONAL).createTurnedTile(Turn.TURN_90);
                    }
                case 3:
                    if(topIndex == 0) {
                        return createTurnedTile(Turn.TURN_270);
                    } else {
                        return createdMirroredTile(Mirror.MAIN_DIAGONAL);
                    }
                default:
                    throw new RuntimeException("Wrong tile geometry!");
            }
        }

        public Tile createTurnedTile(Turn turn) {
            Tile newTile = new Tile(this.id);
            int imageWidth = pixels.size();
            if (turn == Turn.TURN_90) {
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(NORTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(EAST));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(SOUTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(WEST));
                // TODO: pixels
                for (int i = 0; i < imageWidth; ++i){
                    var rowPixels = new ArrayList<Character>(pixels.size());
                    newTile.pixels.add(rowPixels);
                    for(int j = 0; j < imageWidth; ++j){
                        rowPixels.add(pixels.get(imageWidth - 1 - j).get(i));
                    }
                }
            } else if (turn == Turn.TURN_180) {
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(WEST));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(NORTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(EAST));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(SOUTH));
                for (int i = 0; i < imageWidth; ++i){
                    var rowPixels = new ArrayList<Character>(pixels.size());
                    newTile.pixels.add(rowPixels);
                    for(int j = 0; j < imageWidth; ++j){
                        rowPixels.add(pixels.get(imageWidth - 1 - i).get(imageWidth - 1 - j));
                    }
                }
            } else { // turn == Turn.TURN_270
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(SOUTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(WEST));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(NORTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(EAST));
                for (int i = 0; i < imageWidth; ++i){
                    var rowPixels = new ArrayList<Character>(pixels.size());
                    newTile.pixels.add(rowPixels);
                    for(int j = 0; j < imageWidth; ++j){
                        rowPixels.add(pixels.get(j).get(imageWidth - 1 - i));
                    }
                }
            }
            return newTile;
        }

        private Tile createdMirroredTile(Mirror mirror) {
            Tile newTile = new Tile(this.id);
            int imageWidth = pixels.size();
            if (mirror == Mirror.MAIN_DIAGONAL) {
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(SOUTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(EAST));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(NORTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(WEST));
                for (int i = 0; i < imageWidth; ++i){
                    var rowPixels = new ArrayList<Character>(pixels.size());
                    newTile.pixels.add(rowPixels);
                    for(int j = 0; j < imageWidth; ++j){
                        rowPixels.add(pixels.get(j).get(i));
                    }
                }
            } else {
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(NORTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(WEST));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(SOUTH));
                newTile.normalizedPatterns.add(this.normalizedPatterns.get(EAST));
                for (int i = 0; i < imageWidth; ++i){
                    var rowPixels = new ArrayList<Character>(pixels.size());
                    newTile.pixels.add(rowPixels);
                    for(int j = 0; j < imageWidth; ++j){
                        rowPixels.add(pixels.get(imageWidth - 1 - j).get( imageWidth - 1 - i));
                    }
                }
            }
            return newTile;
        }
    }

    protected static int getPattern(String line) {
        int pattern = 0;
        for (int i = 0; i < line.length(); ++i) {
            if (line.charAt(i) == '#')
                pattern += (1 << i);
        }
        return pattern;
    }
    protected static int alternate(int pattern, int width) {
        int alternative = 0;
        for (int i = 0; i < width; ++i) {
            if((pattern & (1 << i)) != 0) {
                alternative += (1 << (width - i - 1));
            }
        }
        return alternative;
    }
    protected static void addMapping(HashMap<Integer, ArrayList<Long>> patternMap, int pattern, long tileId) {
        if (patternMap.containsKey(pattern)) {
            patternMap.get(pattern).add(tileId);
        } else {
            ArrayList<Long> ids = new ArrayList<>(2);
            ids.add(tileId);
            patternMap.put(pattern, ids);
        }
    }
    protected HashMap<Long, Tile> parseInput() {
        // ID -> Tile
        HashMap<Long, Tile> tiles = new HashMap<>();

        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("", "input.txt");
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            long currentTileId = -1;
            int currentRow = 0;
            int topPattern = 0;
            int leftPattern = 0;
            int rightPattern = 0;
            Tile tile = null;
            while ((line = reader.readLine()) != null) {
                if(line.isEmpty()) {
                    currentTileId = -1;
                    currentRow = 0;
                } else {
                    if (currentTileId == -1) {
                        String s = line.split(" ")[1];
                        currentTileId = Long.parseLong(s.substring(0, s.length() - 1));
                        leftPattern = 0;
                        rightPattern = 0;
                        tile = new Tile(currentTileId);
                    } else {
                        int width = line.length();

                        if(currentRow > 0 && currentRow + 1 < width) {
                            ArrayList<Character> pixelLine = new ArrayList<>(width - 2);
                            char[] pixelArray = line.substring(1, line.length() - 1).toCharArray();
                            for(char c: pixelArray) {
                                pixelLine.add(c);
                            }
                            tile.pixels.add(pixelLine);
                        }

                        leftPattern += (line.charAt(0) == '#') ? (1 << (width - currentRow - 1)) : 0;
                        rightPattern += (line.charAt(width - 1) == '#') ? (1 << currentRow) : 0;
                        if (currentRow == 0) {
                            topPattern = getPattern(line);
                        } else if (currentRow + 1 == width) {
                            int bottomPattern = alternate(getPattern(line), width);
                            tile.normalizedPatterns.add(Math.min(rightPattern, alternate(rightPattern, width)));
                            tile.normalizedPatterns.add(Math.min(bottomPattern, alternate(bottomPattern, width)));
                            tile.normalizedPatterns.add(Math.min(leftPattern, alternate(leftPattern, width)));
                            tile.normalizedPatterns.add(Math.min(topPattern, alternate(topPattern, width)));
                            tiles.put(tile.id, tile);
                        }
                        currentRow++;
                    }
                }
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return tiles;
    }

    record Coord(int x, int y) {}
    class SeaMonster {
        SeaMonster(int rowCount, int columnCount) {
            this.rowCount = rowCount;
            this.columnCount = columnCount;
        }
        public SeaMonster createTurnedMonster(Turn turn) {
            SeaMonster newSeaMonster = new SeaMonster(rowCount, columnCount);
            newSeaMonster.coords = new ArrayList<>();
            if (turn == Turn.TURN_90) {
                newSeaMonster.rowCount = columnCount;
                newSeaMonster.columnCount = rowCount;
                for(Coord c: coords) {
                    newSeaMonster.coords.add(new Coord(rowCount - 1 - c.y, c.x));
                }
            } else if (turn == Turn.TURN_180) {
                for(Coord c: coords) {
                    newSeaMonster.coords.add(new Coord(columnCount - 1 - c.x, rowCount - 1 - c.y));
                }
            } else { // (turn == Turn.TURN_270)
                newSeaMonster.rowCount = columnCount;
                newSeaMonster.columnCount = rowCount;
                for(Coord c: coords) {
                    newSeaMonster.coords.add(new Coord(c.y, columnCount - 1 - c.x));
                }
            }
            return newSeaMonster;
        }
        public SeaMonster createMirroredMonster() {
            SeaMonster newSeaMonster = new SeaMonster(rowCount, columnCount);
            newSeaMonster.coords = new ArrayList<>();
            for(Coord c: coords) {
                newSeaMonster.coords.add(new Coord(c.x, rowCount - 1 - c.y));
            }
            return newSeaMonster;
        }

        public int rowCount;
        public int columnCount;
        public ArrayList<Coord> coords;

    }

    private int part2(HashMap<Long, Tile> tiles, HashMap<Integer, ArrayList<Long>> patternMap, int sideLength,
                             int imageWidth, HashSet<Long> cornerIds) {
        int canvasWidth = sideLength * imageWidth;
        ArrayList<ArrayList<Character>> canvas = new ArrayList<>(canvasWidth);
        for(int i = 0; i < canvasWidth; ++i) {
            var al = new ArrayList<Character>(canvasWidth);
            for(int j = 0; j < canvasWidth; ++j) {
                al.add(' ');
            }
            canvas.add(al);
        }
        ArrayList<ArrayList<Tile>> canvasTiles = new ArrayList<>(sideLength);

        // Choose one corner tile to start as upper-left corner.
        // Tiles in the first row (except the upper-left corner) must have the same pattern as its left neighbor, and no other tiles shall have its
        // pattern at the top.
        // Tiles in the first column (except the upper-left corner) must have the same pattern as its neighbor above, and no other tiles shall have its
        // pattern on the left.
        // Other tiles can be determined by the pattern of its neighbor on the left and of its neighbor above
        long upperLeftId = cornerIds.iterator().next();
        System.out.printf("# upperLeftId: %d\n", upperLeftId);
        {
            canvasTiles.add(new ArrayList<>(sideLength));

            Tile upperLeftTile = tiles.get(upperLeftId);
            Tile newTile = upperLeftTile.createTurnedTileUpperLeft(patternMap);
            addToCanvas(canvas, canvasTiles, newTile, 0, 0, imageWidth);
        }
        // Special handling for row 0
        for (int column = 1; column < sideLength; ++column) {
            Tile leftNeighborTile = canvasTiles.get(0).get(column - 1);
            int leftPattern = leftNeighborTile.normalizedPatterns.get(EAST);
            Tile upperTile = tiles.get(getOwnTileId(patternMap, leftPattern, leftNeighborTile.id));
            Tile newTile = upperTile.createTurnedTileUpper(patternMap, leftPattern);
            addToCanvas(canvas, canvasTiles, newTile, 0, column, imageWidth);
        }
        for (int row = 1; row < sideLength; ++row) {
            canvasTiles.add(new ArrayList<>(sideLength));
            {
                // column = 0
                Tile upperNeighborTile = canvasTiles.get(row - 1).get(0);
                int topPattern = upperNeighborTile.normalizedPatterns.get(SOUTH);
                Tile leftTile = tiles.get(getOwnTileId(patternMap, topPattern, upperNeighborTile.id));
                Tile newTile = leftTile.createTurnedTileLeft(patternMap, topPattern);
                addToCanvas(canvas, canvasTiles, newTile, row, 0, imageWidth);
            }
            for (int column = 1; column < sideLength; ++column) {
                Tile upperNeighborTile = canvasTiles.get(row - 1).get(column);
                int topPattern = upperNeighborTile.normalizedPatterns.get(SOUTH);
                Tile leftNeighborTile = canvasTiles.get(row).get(column - 1);
                int leftPattern = leftNeighborTile.normalizedPatterns.get(EAST);
                Tile tile = tiles.get(getOwnTileId(patternMap, leftPattern, leftNeighborTile.id));
                Tile newTile = tile.createTurnedTileOther(leftPattern, topPattern);
                addToCanvas(canvas, canvasTiles, newTile, row, column, imageWidth);
            }
        }

        // Generate every possible transformation of the sea monster pattern and find occurrences in the canvas.
        ArrayList<SeaMonster> seaMonsters = new ArrayList<>(8);
        SeaMonster seaMonster = new SeaMonster(3, 20);
        seaMonster.coords = new ArrayList<>(Arrays.asList(
                new Coord(0,1), new Coord(1,2), new Coord(4,2),
                new Coord(5,1), new Coord(6,1), new Coord(7,2),
                new Coord(10,2), new Coord(11,1), new Coord(12,1),
                new Coord(13,2), new Coord(16,2), new Coord(17,1),
                new Coord(18,0), new Coord(18,1), new Coord(19,1)
        ));
        seaMonsters.add(seaMonster);
        seaMonsters.add(seaMonster.createTurnedMonster(Turn.TURN_90));
        seaMonsters.add(seaMonster.createTurnedMonster(Turn.TURN_180));
        seaMonsters.add(seaMonster.createTurnedMonster(Turn.TURN_270));
        seaMonsters.add(seaMonsters.get(0).createMirroredMonster());
        seaMonsters.add(seaMonsters.get(1).createMirroredMonster());
        seaMonsters.add(seaMonsters.get(2).createMirroredMonster());
        seaMonsters.add(seaMonsters.get(3).createMirroredMonster());

        ArrayList<Integer> monsterCounts = new ArrayList<>();
        for(SeaMonster m: seaMonsters) {
            int monsterCount = countMonsters(canvas, m);
            if(monsterCount > 0)
                monsterCounts.add(monsterCount);
        }
        if(monsterCounts.size() != 1) {
            throw new RuntimeException("Failed to assume that only one monster orientation will have matches");
        }

        int poundCount = 0;
        for(var canvasLine: canvas) {
            for(var c: canvasLine) {
                if (c.equals('#'))
                    poundCount++;
            }
        }
        return poundCount - (monsterCounts.get(0) * seaMonster.coords.size());
    }

    private int countMonsters(ArrayList<ArrayList<Character>> canvas, SeaMonster m) {
        int count = 0;
        int canvasWidth = canvas.size();
        for(int row = 0; row + m.rowCount <= canvasWidth; ++row) {
            for(int column = 0; column + m.columnCount <= canvasWidth; ++column) {
                boolean isValid = true;
                for(var c: m.coords) {
                    if (!canvas.get(row + c.y).get(column + c.x).equals('#')){
                        isValid = false;
                        break;
                    }
                }
                if(isValid)
                    count++;
            }
        }
        return count;
    }

    private static Long getOwnTileId(HashMap<Integer, ArrayList<Long>> patternMap, int pattern, long otherId) {
        var tiles = patternMap.get(pattern);
        if (tiles.size() != 2) {
            throw new RuntimeException("Cannot not find own tile ID!");
        }

        if (tiles.get(0) == otherId)
            return tiles.get(1);
        return tiles.get(0);
    }

    private static void addToCanvas(ArrayList<ArrayList<Character>> canvas, ArrayList<ArrayList<Tile>> canvasTiles, Tile newTile, int row, int column, int imageWidth) {
        int rowOffset = row * imageWidth;
        int columnOffset = column * imageWidth;
        for(int i = 0; i < imageWidth; ++i) {
            int currentRow = rowOffset + i;
            for(int j = 0; j < imageWidth; ++j) {
                int currentColumn = columnOffset + j;
                canvas.get(currentRow).set(currentColumn, newTile.pixels.get(i).get(j));
            }
        }

        canvasTiles.get(row).add(newTile);
    }

    public static void main(String[] args) {
        Day20 obj = new Day20();
        var tiles = obj.parseInput();

        HashMap<Integer, ArrayList<Long>> patternMap = new HashMap<>();
        for(var tile: tiles.values()) {
            for(int i = 0; i < 4; ++i) {
                addMapping(patternMap, tile.normalizedPatterns.get(i), tile.id);
            }
        }
        // IDs of the edge pieces, including corner pieces.
        HashSet<Long> edgeIds = new HashSet<>();
        // IDs of the corner pieces.
        HashSet<Long> cornerIds = new HashSet<>();
        for (Map.Entry<Integer, ArrayList<Long>> e: patternMap.entrySet()) {
            if (e.getValue().size() == 1) {
                // ID of corner tile. Shall be associated to two patterns.
                long id = e.getValue().get(0);
                if (edgeIds.contains(id))
                    cornerIds.add(id);
                else
                    edgeIds.add(id);
            } else if (e.getValue().size() > 2) {
                throw new RuntimeException("No pattern shall be associated to more than two tiles!");
            }
        }
        System.out.printf("# Tile count: %d\n", tiles.size());
        int sideLength = (int) Math.round(Math.sqrt(tiles.size()));
        System.out.printf("# Side length: %d\n", sideLength);
        int edgeCount = edgeIds.size() + cornerIds.size();
        System.out.printf("# Edge count: %d\n", edgeCount);
        if(edgeCount != 4 * sideLength)
            throw new RuntimeException("Edge count != 4 * side length! Tiles are not a square!");

        long answer1 = 1;
        for (Long id: cornerIds) {
            answer1 *= id;
        }

        System.out.println("Question 1: What do you get if you multiply together the IDs of the four corner tiles?");
        System.out.printf("Answer: %d\n", answer1);

        int imageWidth = tiles.get(cornerIds.iterator().next()).pixels.get(0).size();
        int answer2 = obj.part2(tiles, patternMap, sideLength, imageWidth, cornerIds);

        System.out.println("Question 2: How many # are not part of a sea monster?");
        System.out.printf("Answer: %d\n", answer2);
    }
}