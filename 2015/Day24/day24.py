""" Advent of Code 2015, Day 24
    Author: Chi-Kit Pao
"""

from collections import defaultdict
from itertools import accumulate
from math import prod
import os
import time


def debug_output(message):
    # print(message)
    pass

class Sleigh:
    def __init__(self, total_weight=0, bitfield=0 ):
        self.total_weight = total_weight
        self.bitfield = bitfield
        self.qe = 1
        self.weight_count = 0
    
    def calculate_qe(self, quiz: 'Quiz'):
        weights = [quiz.weights[i] for i in range(len(quiz.weights)) if (1 << i) & self.bitfield != 0]
        self.qe = prod(weights)
        self.weight_count = len(weights)

class Quiz:
    def __init__(self, input_file_name: str):
        self.weights: list[int] = []
        self._read_data(input_file_name)

    def find_configuration(self, group_count: int) -> int:
        sum_of_weights = sum(self.weights)
        assert sum_of_weights % group_count == 0
        sleigh_weight = sum_of_weights // group_count

        accumulated_weights = accumulate(self.weights)
        remaining_weights = [sum_of_weights - x for x in accumulated_weights]
        
        sleighs = [Sleigh()]
        for i in range(len(self.weights)):
            current_sleighs = []
            for sleigh in sleighs:
                if sleigh == sleigh_weight:
                    continue
                if sleigh.total_weight + self.weights[i] <= sleigh_weight:
                    new_sleigh = Sleigh(sleigh.total_weight + self.weights[i]
                        , sleigh.bitfield | (1 << i))
                    current_sleighs.append(new_sleigh)
            sleighs.extend(current_sleighs)
            sleighs = [sleigh for sleigh in sleighs if sleigh.total_weight 
                + remaining_weights[i] >= sleigh_weight]
        
        # length of weigts -> sleigh
        min_length = None
        sleigh_dict = defaultdict(list)
        for sleigh in sleighs:
            sleigh.calculate_qe(self)
            min_length = sleigh.weight_count if min_length is None else \
                min(min_length, sleigh.weight_count)
            sleigh_dict[sleigh.weight_count].append(sleigh)
        # Combinations having the desired sleigh weight (for my puzzle input)
        # Answer 1: 417649
        # Answer 2: 102453
        min_qe = None
        debug_output(f'len(sleighs): {len(sleighs)}')
        for sleigh in sleigh_dict[min_length]:
            min_qe = sleigh.qe if min_qe is None else min(min_qe, sleigh.qe)
        return min_qe
                    
    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            for line in lines:
                self.weights.append(int(line))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: What is the quantum entanglement of the first group of '
        'packages in the ideal configuration?')
    print(f'Answer: {quiz.find_configuration(3)}')
    print('Question 2: What is the quantum entanglement of the first group of '
        'packages in the ideal configuration?')
    print(f'Answer: {quiz.find_configuration(4)}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()