""" Advent of Code 2015, Day 16
    Author: Chi-Kit Pao
"""

from collections import defaultdict
import re
import os
import time


def debug_output(message):
    # print(message)
    pass

aunts = defaultdict(dict)

def check_aunts(exact: bool) -> str:
    ticker: dict[str, int] = {'children': 3,
        'cats': 7,
        'samoyeds': 2,
        'pomeranians': 3,
        'akitas': 0,
        'vizslas': 0,
        'goldfish': 5,
        'trees': 3,
        'cars': 2,
        'perfumes': 1
    }
    global aunts
    for aunt in aunts.items():
        failed = False
        for key, item in aunt[1].items():
            if exact:
                if item != ticker[key]:
                    failed = True
                    break
            else:
                if key in ['cats', 'trees']:
                    if item <= ticker[key]:
                        failed = True
                elif key in ['pomeranians', 'goldfish']:
                    if item >= ticker[key]:
                        failed = True
                elif item != ticker[key]:
                    failed = True
        if not failed:
            return aunt[0]


def main():
    start_time = time.time()
    file_path = os.path.dirname(__file__)
    with open(os.path.join(file_path, 'input.txt'), 'r') as f:
        lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
        # Line Example:
        # Sue 1: goldfish: 9, cars: 0, samoyeds: 9
        for line in lines:
            colon_index = line.index(':')
            name = line[0:colon_index]
            matches = re.findall(r'(\w+): (\d+)', line[colon_index+1:])
            for match in matches:
                global aunts
                aunts[name][match[0]] = int(match[1])
    
    print('Question 1: What is the number of the Sue that got you the gift?')
    print(f'Answer: {check_aunts(True)}')
    print('Question 2: What is the number of the real Sue?')
    print(f'Answer: {check_aunts(False)}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()