// Day13.java
// AoC 2018 Day 13: Mine Cart Madness
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


public class Day13 {
    enum Direction{
        UP(new Pos(-1, 0)),
        RIGHT(new Pos(0, 1)),
        DOWN(new Pos(1, 0)),
        LEFT(new Pos(0, -1));

        public final Pos update;
        Direction(Pos update){
            this.update = update;
        }
    }
    enum Turn{
        LEFT,
        STRAIGHT,
        RIGHT;
    }
    class Cart
    {
        public Pos pos;
        public Direction direction;
        public Turn nextTurn;
        public Cart(int x, int y, Direction direction)
        {
            this.pos = new Pos(y, x);
            this.direction = direction;
            this.nextTurn = Turn.LEFT;
        }
        public void move(){
            pos.x += direction.update.x;
            pos.y += direction.update.y;

            // turn already for next position
            char tile = map.get(pos.y).charAt(pos.x);
            switch(tile)
            {
                case '+':
                    if(nextTurn == Turn.LEFT){
                        direction = (direction == Direction.UP) ? Direction.LEFT
                                : (direction == Direction.RIGHT) ? Direction.UP
                                : (direction == Direction.DOWN) ? Direction.RIGHT
                                : Direction.DOWN;
                        nextTurn = Turn.STRAIGHT;
                    } else if (nextTurn == Turn.STRAIGHT){
                        nextTurn = Turn.RIGHT;
                    } else {
                        direction = (direction == Direction.UP) ? Direction.RIGHT
                                : (direction == Direction.RIGHT) ? Direction.DOWN
                                : (direction == Direction.DOWN) ? Direction.LEFT
                                : Direction.UP;
                        nextTurn = Turn.LEFT;
                    }
                    break;
                case '/':
                    direction = (direction == Direction.UP) ? Direction.RIGHT
                            : (direction == Direction.RIGHT) ? Direction.UP
                            : (direction == Direction.DOWN) ? Direction.LEFT
                            : Direction.DOWN;
                    break;
                case '\\':
                    direction = (direction == Direction.UP) ? Direction.LEFT
                            : (direction == Direction.RIGHT) ? Direction.DOWN
                            : (direction == Direction.DOWN) ? Direction.RIGHT
                            : Direction.UP;
                    break;
                case ' ':
                    throw new RuntimeException("Illegal position "  + pos.y + "," + pos.x);
            }
        }
    }
    private static ArrayList<String> map = new ArrayList<>();
    private static ArrayList<Cart> carts = new ArrayList<>();
    public static Pair<Pos, Pos> getAnswer() {
        Pair<Pos, Pos> answers = new Pair<>(null, null);
        int count = carts.size();
        while(true){
            carts.removeIf(e -> (e == null));
            carts.sort((lhs, rhs) -> {
               if(lhs.pos.y == rhs.pos.y)
                   return lhs.pos.x - rhs.pos.x;
               return lhs.pos.y - rhs.pos.y;
            });
            for(int i = 0; i < carts.size(); ++i){
                Cart cart = carts.get(i);
                if(cart == null)
                    continue;
                cart.move();
                for(int j = 0; j < carts.size(); ++j){
                    if(i == j)
                        continue;
                    Cart other = carts.get(j);
                    if(other == null)
                        continue;
                    if(cart.pos.equals(other.pos)){
                        if(answers.first == null)
                            answers.first = cart.pos;
                        count -= 2;
                        carts.set(i, null);
                        carts.set(j, null);

                        if(count <= 1) {
                            Cart remainingCart = null;
                            for(int k = 0; k < carts.size() ; ++k){
                                remainingCart = carts.get(k);
                                if(remainingCart != null){
                                    if(k > i)
                                        remainingCart.move();
                                    break;
                                }
                            }
                            answers.second = remainingCart.pos;
                            return answers;
                        }
                    }
                }
            }
        }
    }

    public static void parseInput(Day13 obj, String fileName){
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line;
            int row = 0;
            while ((line = reader.readLine()) != null) {
                if (line.length() == 0)
                    continue;
                StringBuilder sb = new StringBuilder();
                for(int col = 0; col < line.length(); ++col)
                    sb.append(processChar(obj, row, col, line.charAt(col)));
                map.add(sb.toString());
                row++;
            }
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
    }

    private static char processChar(Day13 obj, int row, int col, char c) {
        if(c == '^') {
            carts.add(obj.new Cart(col, row, Direction.UP));
            return '|';
        } else if (c == '>') {
            carts.add(obj.new Cart(col, row, Direction.RIGHT));
            return '-';
        } else if (c == 'v') {
            carts.add(obj.new Cart(col, row, Direction.DOWN));
            return '|';
        } else if (c == '<') {
            carts.add(obj.new Cart(col, row, Direction.LEFT));
            return '-';
        }
        return c;
    }



    public static void main(String[] args) {
        Day13 obj = new Day13();
        System.out.println("Day 13:");
        parseInput(obj, "Day13_input.txt");
        Pair<Pos, Pos> answers = getAnswer();
        System.out.println("Question 1: Where is the location of the first crash?");
        System.out.format("Answer: %d,%d%n", answers.first.x, answers.first.y);
        System.out.println("Question 2: What is the location of the last cart at the end of the first tick where it "
                + "is the only cart left?");
        System.out.format("Answer: %d,%d%n", answers.second.x, answers.second.y);
    }
}
