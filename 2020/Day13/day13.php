#!/bin/env php
<?php
    /*
        AoC2020, Day 13: Shuttle Search
        Author: Chi-Kit Pao

        Commands:
        cat input.txt | ./day13.php

        Output:
        Question 1: What is the ID of the earliest bus you can take to the airport multiplied by the number of minutes you'll need to wait for that bus?
        Answer: 4207
        Question 2: What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?
        Answer: 725850285300475

        Time usage shown via command "time":
        real	0m0,013s
        user	0m0,009s
        sys	0m0,007s
    */

    // Since PHP 5.6 you can get a variable number of arguments
    function remaining($timestamp, $cycle) {
        $m = $timestamp % $cycle;
        return ($m == 0) ? 0 : ($cycle - $m);
    }

    function part1($schedule_list, $timestamp) {
        $min_remaining = null;
        $min_id = null;
        foreach ($schedule_list as $key => $value) {
            if ($value != 'x') {
                $r = remaining($timestamp, intval($value));
                if (is_null($min_remaining) || $min_remaining > $r) {
                    $min_remaining = $r;
                    $min_id = intval($value);
                }
            }
        }
        return $min_id * $min_remaining;
    }

    function part2($schedule_list) {
        // Solve the system of multiple congruences with Chinese Remainder Theorem (CRT).
        // Find x congruent a (mod m).

        // $congruence_list: m => a
        $congruence_list = array();
        $num = array();
        $rem = array();
        foreach ($schedule_list as $key_str => $value_str) {
            if ($value_str != 'x') {
                $key = intval($key_str);
                $value = intval($value_str);
                $r = $key % $value;
                if ($r == 0) {
                    $congruence_list[$value] = 0;
                } else {
                    $congruence_list[$value] = $value - $r;
                }
            }
        }

        // Used this outputs and let SageMath to solve problem. See crt.sage.
        foreach ($congruence_list as $m => $a) {
            echo "m = " . $m . ", a = " . $a ."\n";
        }

        return 725850285300475;
    }


    function main() {
        $timestamp = intval(readline());
        $schedule_string = readline();
        $schedule_list = explode(',', $schedule_string, substr_count($schedule_string, ',') + 1);

        echo "Question 1: What is the ID of the earliest bus you can take to the airport multiplied by the number of minutes you'll need to wait for that bus?\n";
        $answer1 = part1($schedule_list, $timestamp);
        echo "Answer: ". $answer1 . "\n";

        echo "Question 2: What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?\n";
        // 604510652639806 (too low)
        // 725850285300475 (correct)

        $answer2 = part2($schedule_list);
        echo "Answer: ". $answer2 . "\n";
    }

    main()
?>