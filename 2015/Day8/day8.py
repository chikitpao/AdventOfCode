""" Advent of Code 2015, Day 8
    Author: Chi-Kit Pao
"""

import ast
import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.lines: list[str] = None
        self._read_data(input_file_name)

    def run(self) -> int:
        length_literals = 0
        length_values = 0
        for line in self.lines:
            length_literals += len(line)
            str_value = ast.literal_eval(line)
            length_values += len(str_value)
        return length_literals - length_values

    def run_v2(self) -> int:
        difference = 0
        for line in self.lines:
            # We always need 2 new double quotation marks
            difference += 2
            # For each double quotation mark, we need an additional character.
            # It also applies for backslash.
            char_list = list(line)
            difference += char_list.count('"') + char_list.count('\\')
        return difference

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            self.lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: Number of characters for string literals minus number' \
        ' of characters in memory for string values?')
    quiz = Quiz('input.txt')
    print(f'Answer: {quiz.run()}')

    print('Question 2: Number of characters for newly encoded strings minus ' \
        ' number of characters in string literals?')
    print(f'Answer: {quiz.run_v2()}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()