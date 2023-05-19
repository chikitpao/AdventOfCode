// Day8.java
// AoC 2018 Day 8: Memory Maneuver
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
import java.util.stream.Collectors;

public class Day8 {

    class MyTreeNode{
        ArrayList<MyTreeNode> children = new ArrayList<>();
        ArrayList<Integer> metaData = new ArrayList<>();

        public void parse(Iterator<Integer> iterator) {
            Integer childrenCount = iterator.next();
            Integer metaDataCount = iterator.next();
            for(int i = 0; i < childrenCount; ++i){
                MyTreeNode child = new MyTreeNode();
                children.add(child);
                child.parse(iterator);
            }
            for(int i = 0; i < metaDataCount; ++i) {
                metaData.add(iterator.next());
            }
        }
        public int getMetaDataSum(){
            return metaData.stream().reduce(0, Integer::sum)
                    + children.stream().map(c -> c.getMetaDataSum()).reduce(0, Integer::sum);
        }
        public int getMetaDataSum2(){
            if(children.isEmpty())
                return metaData.stream().reduce(0, Integer::sum);

            // node has children
            ArrayList<Integer> metaData2 = new ArrayList<>(metaData);
            metaData2.removeIf(e -> (e <= 0 || e > children.size()));
            return metaData2.stream().map(e -> children.get(e - 1).getMetaDataSum2()).reduce(0, Integer::sum);
        }
    }

    public static int GetAnswer1(MyTreeNode root) {
        return root.getMetaDataSum();
    }

    public static int GetAnswer2(MyTreeNode root) {
        return root.getMetaDataSum2();
    }

    public static MyTreeNode parseInput(Day8 obj, String fileName){
        MyTreeNode root = obj.new MyTreeNode();
        Charset charset = Charset.forName("US-ASCII");
        Path path = FileSystems.getDefault().getPath("input", fileName);
        try (BufferedReader reader = Files.newBufferedReader(path, charset)) {
            String line = reader.readLine();
            ArrayList<Integer> data = new ArrayList<>(Arrays.stream(line.split(" "))
                    .map(s -> Integer.parseInt(s)).collect(Collectors.toList()));
            root.parse(data.iterator());
        } catch (IOException x) {
            System.err.format("IOException: %s", x);
        }
        return root;
    }

    public static void main(String[] args) {
        Day8 obj = new Day8();
        MyTreeNode root = parseInput(obj, "Day8_input.txt");

        System.out.println("Day 8:");
        System.out.println("Question 1: What is the sum of all metadata entries?");
        System.out.println("Answer: " + GetAnswer1(root));
        System.out.println("Question 2: What is the value of the root node?");
        System.out.println("Answer: " + GetAnswer2(root));
    }
}
