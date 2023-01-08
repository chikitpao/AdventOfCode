""" Advent of Code 2015, Day 14
    Author: Chi-Kit Pao
"""

from collections import defaultdict
import re
import os
import time


def debug_output(message):
    # print(message)
    pass


class Deer:
    def __init__(self, name: str, velocity: int, fly: int, rest: int):
        self.name = name
        self.velocity = velocity
        self.fly_time = fly
        self.rest_time = rest

    def fly(self, seconds: int) -> int:
        """ km flown after seconds """
        q = seconds // (self.fly_time + self.rest_time)
        r = seconds % (self.fly_time + self.rest_time)
        r = min(r, self.fly_time)
        return self.velocity * (q * self.fly_time + r)


class Quiz:
    def __init__(self, input_file_name: str):
        self.deers: list[Deer] = []
        self._read_data(input_file_name)

    def run(self, seconds: int) -> int:
        """ km flown by the winning reindeer after seconds """
        return max(deer.fly(seconds) for deer in self.deers)

    def run_v2(self, seconds: int) -> int:
        """ points by the winning reindeer after seconds """
        distances = defaultdict(int)
        points = defaultdict(int)
        for sec in range(seconds):
            for deer in self.deers:
                if sec % (deer.fly_time + deer.rest_time) < deer.fly_time:
                    distances[deer.name] += deer.velocity
            max_distance = max(distances.values())
            for item in distances.items():
                if item[1] == max_distance:
                    points[item[0]] += 1
        return max(points.values())

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            # Line Example:
            # Rudolph can fly 22 km/s for 8 seconds, but then must rest for 165 seconds.            
            for line in lines:
                match = re.match(r'(?P<deer>\w+) can fly (?P<velocity>\d+) km/s'
                    r' for (?P<fly>\d+) seconds, but then must rest for '
                    r'(?P<rest>\d+) seconds.'
                    , line)
                groupdict = match.groupdict()
                deer = groupdict['deer']
                velocity = int(groupdict['velocity'])
                fly = int(groupdict['fly'])
                rest = int(groupdict['rest'])
                self.deers.append(Deer(deer, velocity, fly, rest))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: Distance the winning reindeer traveled after exactly '
        '2503 seconds?')
    print(f'Answer: {quiz.run(2503)}')
    
    print('Question 2: Points the winning reindeer has after exactly '
        '2503 seconds?')
    print(f'Answer: {quiz.run_v2(2503)}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()