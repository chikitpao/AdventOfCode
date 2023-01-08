""" Advent of Code 2015, Day 17
    Author: Chi-Kit Pao
"""

from collections import defaultdict
from itertools import chain, combinations
import os
import time


def debug_output(message):
    # print(message)
    pass


# Source: https://docs.python.org/3/library/itertools.html#itertools-recipes
def powerset(iterable):
    "powerset([1,2,3]) --> () (1,) (2,) (3,) (1,2) (1,3) (2,3) (1,2,3)"
    s = list(iterable)
    return chain.from_iterable(combinations(s, r) for r in range(len(s)+1))


class Quiz:
    def __init__(self, input_file_name: str):
        self.values: list[int] = None
        self._read_data(input_file_name)

    def get_combinations(self, volume: int) -> int:
        count = 0
        for element in powerset(self.values):
            if sum(element) == volume:
                count += 1
        return count

    def get_combinations_v2(self, volume: int) -> int:
        # number of containers -> number of combinations
        valid_combinations = defaultdict(int)
        for element in powerset(self.values):
            if sum(element) == volume:
                valid_combinations[len(element)] += 1
        return valid_combinations[min(valid_combinations.keys())]

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            self.values = list(map(lambda s: int(s.replace('\n', '')), f.readlines()))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: How many different combinations of containers can '
        'exactly fit all 150 liters of eggnog?')
    print(f'Answer: {quiz.get_combinations(150)}')
    print('Question 2: How many different combinations of containers can '
        'exactly fit all 150 liters of eggnog (with minimum numbers of '
        'containers?')
    print(f'Answer: {quiz.get_combinations_v2(150)}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()