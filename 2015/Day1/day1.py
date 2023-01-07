""" Advent of Code 2015, Day 1
    Author: Chi-Kit Pao
"""

import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.lines: list[str] = None
        self._read_data(input_file_name)

    def get_floor(self) -> int:
        char_list = list(self.lines[0])
        up_count = char_list.count('(')
        down_count = char_list.count(')')
        return up_count - down_count

    def get_pos_for_entering(self) -> int:
        """ position of the character that causes Santa to first enter the 
        basement """
        current_floor = 0
        for i, ch in enumerate(self.lines[0], 1):
            if ch =='(':
                current_floor += 1
            else:
                current_floor -= 1
            if current_floor == -1:
                return i

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            self.lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: To what floor do the instructions take Santa?')
    print(f'Answer: Floor {quiz.get_floor()}')

    print('Question 2: What is the position of the character that causes ' \
        'Santa to first enter the basement?')
    print(f'Answer: Floor {quiz.get_pos_for_entering()}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()