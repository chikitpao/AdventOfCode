""" Advent of Code 2015, Day 3
    Author: Chi-Kit Pao
"""

import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.line: str = None
        self._read_data(input_file_name)

    def get_houses_with_present(self) -> int:
        """ Number of houses receiving present with one Santa"""
        current_pos = (0, 0)  # row, col
        visited = {current_pos}
        offsets = {'>': (0, 1), 'v': (1, 0), '<': (0, -1), '^': (-1, 0)}
        for ch in self.line:
            current_pos = tuple(v1 + v2 for v1, v2 in zip(current_pos, offsets[ch]))
            visited.add(current_pos)
        return len(visited)

    def get_houses_with_present_v2(self) -> int:
        """ Number of houses receiving present with two Santas"""
        current_pos1 = (0, 0)  # row, col
        visited1 = {current_pos1}
        current_pos2 = (0, 0)  # row, col
        visited2 = {current_pos2}
        offsets = {'>': (0, 1), 'v': (1, 0), '<': (0, -1), '^': (-1, 0)}
        for i, ch in enumerate(self.line):
            if i % 2 == 0:
                current_pos1 = tuple(v1 + v2 for v1, v2 in zip(current_pos1, offsets[ch]))
                visited1.add(current_pos1)
            else:
                current_pos2 = tuple(v1 + v2 for v1, v2 in zip(current_pos2, offsets[ch]))
                visited2.add(current_pos2)
        visited = visited1 | visited2
        return len(visited)

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            self.line = lines[0]


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: How many houses receive at least one present?')
    print(f'Answer: {quiz.get_houses_with_present()}')

    print('Question 2: How many houses receive at least one present?')
    print(f'Answer: {quiz.get_houses_with_present_v2()}')
    
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()