""" Advent of Code 2015 Day 12
    Author: Chi-Kit Pao
"""

import json
import re
import os
import time


def debug_output(message):
    # print(message)
    pass


def process_json_input(input, ignore_str: str):
    sum_of_numbers = 0

    if isinstance(input, dict):
        if ignore_str in input.values():
            return 0
        for key in input.keys():
            sum_of_numbers += process_json_input(input[key], ignore_str)
    elif isinstance(input, list):
        for item in input:
            sum_of_numbers += process_json_input(item, ignore_str)
    elif isinstance(input, int):
        sum_of_numbers += input
    else:
        ValueError(f'Unhandled input {input}, type {type(input)}')
    
    return sum_of_numbers


def main():
    start_time = time.time()
    file_path = os.path.dirname(__file__)
    line: str = None
    with open(os.path.join(file_path, 'input.txt'), 'r') as f:
        lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
        line = lines[0]
    
    print('Question 1: Sum of all numbers?')
    sum_of_numbers = sum(map(int, re.findall(r'[+-]?\d+', line)))
    print(f'Answer: {sum_of_numbers}')

    print('Question 2: Sum of all numbers (igonring red)?')
    with open(os.path.join(file_path, 'input.txt'), 'r') as f:
        input = json.load(f)
        sum_of_numbers = process_json_input(input, 'red')
        print(f'Answer: {sum_of_numbers}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()