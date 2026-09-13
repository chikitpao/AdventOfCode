#!/usr/bin/bash

# Aoc 2020, Day 2: Password Philosophy
# Author: Chi-Kit Pao
#
# Commands:
# ./day02.sh
#
# Output:
# Advent of code 2020, Day 2
# Question 1: How many passwords are valid according to their policies?
# Answer: 483
# Question 2: How many passwords are valid according to the new interpretation of the policies?
# Answer: 482
#
# Time usage shown via command "time".
# real	0m3,791s
# user	0m2,986s
# sys	0m1,521s
#

answer1=0
occurence=0
answer2=0
# Example line: "1-3 a: abcde"
while IFS=$' \t\n-:' read -r p1 p2 p3 p4; do

    ((occurence=0))
    p4Chars=$(echo $p4 | grep -o .)
    for c in $p4Chars
    do
        if [[ $p3 == $c ]]; then
            ((occurence++))
        fi
    done
    if [[ "$occurence" -ge "$p1" ]] && [[ "$occurence" -le "$p2" ]]; then
        ((answer1++))
    fi

    # Compare with substring (one-based index)
    if [[ "$p3" == "${p4:($p1-1):1}" ]] && [[ "$p3" != "${p4:($p2-1):1}" ]]; then
        ((answer2++))
    elif [[ "$p3" != "${p4:($p1-1):1}" ]] && [[ "$p3" == "${p4:($p2-1):1}" ]]; then
         ((answer2++))
    fi

done < input.txt

printf "Question 1: How many passwords are valid according to their policies?\n"
printf "Answer: %d\n" $answer1
printf "Question 2: How many passwords are valid according to the new interpretation of the policies?\n"
printf "Answer: %d\n" $answer2
