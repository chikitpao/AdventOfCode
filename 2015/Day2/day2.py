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
        self.data: list[tuple[int, int, int, int]] = []
        self._read_data(input_file_name)

    def get_ribbon(self) -> int:
        ribbon = 0
        for l, w, h, paper in self.data:
            bow = l * w * h
            ribbon += bow + 2 * (l + w + h - max(l, w, h))
        return ribbon

    def get_wrapping_paper(self) -> int:
        return sum(paper for l, w, h, paper in self.data)

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            for line in lines:
                l, w, h = map(int, line.split('x'))
                lw = l * w
                wh = w * h
                hl = h * l
                slack = min(lw, wh, hl)
                paper = 2 * (lw + wh + hl) + slack
                self.data.append((l, w, h, paper))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: How many total square feet of wrapping paper should ' \
        'they order?')
    print(f'Answer: {quiz.get_wrapping_paper()}')

    print('Question 2: How many total feet of ribbon should they order? ')
    print(f'Answer: {quiz.get_ribbon()}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()