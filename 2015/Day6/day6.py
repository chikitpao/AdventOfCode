""" Advent of Code 2015, Day 6
    Author: Chi-Kit Pao
    REMARK: Requires numpy to run this program.
"""


import numpy as np
import os
import re
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.lines: list[str] = None
        self.lights: np.ndarray = None
        self._read_data(input_file_name)

    def execute_instructions(self) -> int:
        self.lights = np.full((1000,1000), fill_value=0, dtype=np.int16)
        # Line examples
        # turn on 489,959 through 759,964
        # turn off 820,516 through 871,914
        # toggle 756,965 through 812,992
        pattern = re.compile(r'(\d+),(\d+) through (\d+),(\d+)')
        for line in self.lines:
            match = pattern.search(line)
            assert match, f'Unmatch instruction: {line}'
            top, left, bottom, right = map(int, match.groups())

            if line.startswith('turn on'):
                self.lights[top:bottom+1, left:right+1] = 1
            elif line.startswith('turn off'):
                self.lights[top:bottom+1, left:right+1] = 0
            elif line.startswith('toggle'):
                self.lights[top:bottom+1, left:right+1] ^= 1
        return np.sum(self.lights)

    def execute_instructions_v2(self) -> int:
        self.lights = np.full((1000,1000), fill_value=0, dtype=np.int16)
        # Line examples
        # turn on 489,959 through 759,964
        # turn off 820,516 through 871,914
        # toggle 756,965 through 812,992
        pattern = re.compile(r'(\d+),(\d+) through (\d+),(\d+)')
        for line in self.lines:
            match = pattern.search(line)
            assert match, f'Unmatch instruction: {line}'
            top, left, bottom, right = map(int, match.groups())

            if line.startswith('turn on'):
                self.lights[top:bottom+1, left:right+1] += 1
            elif line.startswith('turn off'):
                self.lights[top:bottom+1, left:right+1] -= 1
            elif line.startswith('toggle'):
                self.lights[top:bottom+1, left:right+1] += 2
            # If values are changed to be less than 0, change them back to 0.
            self.lights[self.lights < 0] = 0
        return np.sum(self.lights)

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            self.lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: How many lights are lit?')
    print(f'Answer: {quiz.execute_instructions()}')

    print('Question 2: What is the total brightness of all lights combined?')
    print(f'Answer: {quiz.execute_instructions_v2()}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()