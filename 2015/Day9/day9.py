""" Advent of Code 2015, Day 9
    Author: Chi-Kit Pao
"""

from collections import defaultdict
from itertools import permutations
import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        # usage: distance = self.distances[place_from][place_to]
        self.distances = defaultdict(defaultdict)
        # usage: routes = self.routes[distance]
        self.routes = defaultdict(list)
        self._read_data(input_file_name)
        self._calculate_route_distances()

    def _calculate_route_distances(self):
        routes = list(permutations(self.distances.keys()))
        for route in routes:
            route_distance = self._get_route_distance(route)
            self.routes[route_distance].append(route)

    def _get_route_distance(self, route: list[str]) -> int:
        distance = 0
        for i in range(len(route) - 1):
            distance += self.distances[route[i]][route[i+1]]
        return distance

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            # Line Example:
            # Faerun to Norrath = 129
            for line in lines:
                tokens = line.split()
                place_from = tokens[0]
                place_to = tokens[2]
                distance = int(tokens[4])
                self.distances[place_from][place_to] = distance
                self.distances[place_to][place_from] = distance


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: What is the distance of the shortest route?')
    min_distance = min(quiz.routes.keys())
    print(f'Answer: {min_distance}')
    print(f'Routes: {quiz.routes[min_distance]}')

    print('Question 2: What is the distance of the longest route?')
    max_distance = max(quiz.routes.keys())
    print(f'Answer: {max_distance}')
    print(f'Routes: {quiz.routes[max_distance]}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()