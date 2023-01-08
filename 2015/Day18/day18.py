""" Advent of Code 2015, Day 18
    Author: Chi-Kit Pao
"""

import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.grid: list[list[bool]] = []
        self._read_data(input_file_name)

    def count_lights_on(self) -> int:
        count = 0
        for line in self.grid:
            count += line.count(True)
        return count

    def run(self, rounds: int, stuck_on: bool = False):
        height = len(self.grid)
        width =  len(self.grid[0])
        stuck_on_coordinates = {(0, 0), (0, width-1), (height-1, 0), (height-1, width-1)}
        for _ in range(rounds):
            if stuck_on:
                for co in stuck_on_coordinates:
                    self.grid[co[0]][co[1]] = True
            new_grid = []
            for i in range(height):
                new_grid.append([False] * width)
            for i in range(height):
                for j in range(width):
                    # Check neighbors
                    on_count = 0
                    check_up = (i > 0)
                    check_down = (i + 1 < height)
                    check_left = (j > 0)
                    check_right = (j + 1 < width)
                    if check_up:
                        if check_left and self.grid[i-1][j-1]:
                            on_count += 1
                        if self.grid[i-1][j]:
                            on_count += 1
                        if check_right and self.grid[i-1][j+1]:
                            on_count += 1
                    if check_left and self.grid[i][j-1]:
                        on_count += 1
                    if check_right and self.grid[i][j+1]:
                        on_count += 1
                    if check_down:
                        if check_left and self.grid[i+1][j-1]:
                            on_count += 1
                        if self.grid[i+1][j]:
                            on_count += 1
                        if check_right and self.grid[i+1][j+1]:
                            on_count += 1
                    # own_value
                    if self.grid[i][j]:
                        new_grid[i][j] = True if on_count == 2 or on_count == 3 else False 
                    else:
                        new_grid[i][j] = True if on_count == 3 else False
            self.grid = new_grid
            if stuck_on:
                for co in stuck_on_coordinates:
                    self.grid[co[0]][co[1]] = True


    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            for line in lines:
                assert len(line) == 100
                grid_line = [False] * len(line)
                self.grid.append(grid_line)
                for i, ch in enumerate(line):
                    if line[i] == '#':
                        grid_line[i] = True
            assert len(self.grid) == 100


def main():
    start_time = time.time()
    print('Question 1: How many lights are on after 100 steps?')
    quiz = Quiz('input.txt')
    quiz.run(100)
    print(f'Answer: {quiz.count_lights_on()}')
    print('Question 2: How many lights are on after 100 steps (with lights '
        'stuck on)?')
    quiz = Quiz('input.txt')
    quiz.run(100, True)
    print(f'Answer: {quiz.count_lights_on()}')


    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()