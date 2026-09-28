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
        real	0m0,021s
        user	0m0,011s
        sys	0m0,013s

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

    // With non-negative a and b, returns array [g, u, v] such as d = ua + vb with
    //   d = gcd(a, b).
    // Throws exception if both a and b are 0.
    function gcdx($a, $b) {
        if ($a == 0 && $b == 0)
            throw new Exception("gcdx: At least one of the arguments must differ from 0!");

        $swapped = false;
        if ($b > $a) {
            $temp = $b;
            $b = $a;
            $a = $temp;
            $swapped = true;
        }
        if ($b == 0)
            return [$a, 1, 0];
        $q = intdiv($a, $b);
        $r = $a % $b;
        $arr = gcdx($b, $r);

        if (($arr[2] != 0) && ((($q * $arr[2]) / $arr[2]) != $q))
            throw new Exception("gcdx: Overflow after multiplication!");

        if ($swapped) {
            return [$arr[0], $arr[1] - $q * $arr[2], $arr[2]];
        } else {
            return [$arr[0], $arr[2], $arr[1] - $q * $arr[2]];
        }
    }

    // Returns (a + b) (mod m).
    function add_mod(int $a, int $b, int $m) {
        $a %= $m;
        $b %= $m;
        if ($a < 0) {
            $a += $m;
        }
        if ($b < 0) {
            $b += $m;
        }

        if ($a >= $m - $b) {
            return $a - ($m - $b);
        }

        return $a + $b;
    }

    // Returns (a * b) (mod m).
    function mul_mod(int $a, int $b, int $m) {
        $a %= $m;
        $b %= $m;
        if ($a < 0) {
            $a += $m;
        }
        if ($b < 0) {
            $b += $m;
        }
        $result = 0;

        while ($b > 0) {
            if ($b & 1) {
                $result = add_mod($result, $a, $m);
            }

            $a = add_mod($a, $a, $m);
            $b >>= 1;
        }

        return $result;
    }

    // Using the Chinese remainder theorem, find the solution x
    // of the simultaneous congruences:
    //   x congruent to a1 (mod m1), and
    //   x congruent to a2 (mod m2).
    // Throws exception if m1 and m2 are not coprime.
    function crt($a1, $m1, $a2, $m2) {
        if (is_null($a1) || is_null($m1)) {
            return [$a2, $m2];
        }

        $arr = gcdx($m1, $m2);
        if($arr[0] != 1)
            throw new Exception("crt: m1 and m2 must be coprime!");

        $p2 = $m1 * $m2;

        # Overflow after multiplication!
        #$part1 = ((((($a1 % $part2) * $arr[2]) % $part2) * $m2) % $part2)
        #    + ((((($a2 % $part2) * $arr[1]) % $part2) * $m1) % $part2);

        $temp1 = mul_mod($a1, $arr[2], $p2);
        $temp1 = mul_mod($temp1, $m2, $p2);
        $temp2 = mul_mod($a2, $arr[1], $p2);
        $temp2 = mul_mod($temp2, $m1, $p2);
        $p1 = ($temp1 + $temp2) % $p2;

        if ($p1 < 0) {
            $p1 += $p2;
        }

        return [$p1, $p2];
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

        $temp_a = null;
        $temp_m = null;
        foreach ($congruence_list as $m => $a) {
            $arr = crt($temp_a, $temp_m, $a, $m);
            $temp_a = $arr[0];
            $temp_m = $arr[1];
        }

        return $temp_a;
    }


    function main() {
        $timestamp = intval(readline());
        $schedule_string = readline();
        $schedule_list = explode(',', $schedule_string, substr_count($schedule_string, ',') + 1);

        echo "PHP_INT_SIZE: " . PHP_INT_SIZE . "\n";  // 8
        echo "PHP_INT_MAX: " . PHP_INT_MAX . "\n";  // 9223372036854775807
        echo "PHP_INT_MIN: " . PHP_INT_MIN . "\n";  // -9223372036854775808

        echo "Question 1: What is the ID of the earliest bus you can take to the airport multiplied by the number of minutes you'll need to wait for that bus?\n";
        $answer1 = part1($schedule_list, $timestamp);
        echo "Answer: ". $answer1 . "\n";

        echo "Question 2: What is the earliest timestamp such that all of the listed bus IDs depart at offsets matching their positions in the list?\n";
        $answer2 = part2($schedule_list);
        echo "Answer: ". $answer2 . "\n";
    }

    main()
?>