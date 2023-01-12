""" Advent of Code 2015, Day 20
    Author: Chi-Kit Pao
"""

from collections import defaultdict
import sympy as sp
import time


def debug_output(message):
    # print(message)
    pass


def main():
    start_time = time.time()
    
    puzzle_input = 34000000
    puzzle_input_d10 = puzzle_input // 10
    print('Question 1: What is the lowest house number of the house to get at '
        'least as many presents as the number in your puzzle input?')
    i = 0
    while True:
        sum_divisors = sum(sp.divisors(i))
        if sum_divisors >= puzzle_input_d10:
            print(f'Answer: {i}')
            print(f'sum of divisors: {sum_divisors}')
            break
        i += 1

    print('Question 2: What is the new lowest house number of the house to get '
        'at least as many presents as the number in your puzzle input?')
    i = 0
    lookup_dict = defaultdict(int)
    while True:
        divisors = sp.divisors(i)
        divisors = [d for d in divisors if lookup_dict[d] < 50]
        for d in divisors:
            lookup_dict[d] += 1
        sum_divisors = sum(divisors)
        if sum_divisors * 11 >= puzzle_input:
            print(f'Answer: {i}')
            print(f'Number of presents: {sum_divisors * 11}')
            break
        i += 1
    # Time elapsed: 26.008418798446655 s
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()