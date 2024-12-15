""" Advent of Code 20??, Day ??
    Author: Chi-Kit Pao
"""

import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self._read_data(input_file_name)

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()