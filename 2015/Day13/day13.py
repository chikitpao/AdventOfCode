""" Advent of Code 2015, Day 13
    Author: Chi-Kit Pao
"""

from collections import defaultdict
from itertools import permutations
import re
import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str, include_myself = False):
        # usage: happiness = self.happiness[person1][person2]
        self.happiness = defaultdict(defaultdict)
        # usage: arrangement = self.arrangements[happiness]
        self.arrangements = defaultdict(list)
        self._read_data(input_file_name, include_myself)
        self._calculate_arrangement_happiness()

    def _calculate_arrangement_happiness(self):
        arrangements = list(permutations(self.happiness.keys()))
        for arrangement in arrangements:
            happiness = self._get_arrangement_happiness(arrangement)
            self.arrangements[happiness].append(arrangement)

    def _get_arrangement_happiness(self, arrangement: list[str]) -> int:
        happiness = 0
        for i in range(len(arrangement)):
            if i + 1 < len(arrangement):
                happiness += self.happiness[arrangement[i]][arrangement[i+1]]
            else:
                happiness += self.happiness[arrangement[i]][arrangement[0]]
            if i != 0:
                happiness += self.happiness[arrangement[i]][arrangement[i-1]]
            else:
                happiness += self.happiness[arrangement[i]][arrangement[-1]]
        return happiness

    def _read_data(self, input_file_name: str, include_myself):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            # Line Examples:
            # Alice would lose 2 happiness units by sitting next to Bob.
            # Alice would gain 65 happiness units by sitting next to David.
            for line in lines:
                match = re.match(r'(?P<person1>\w+) would (?P<gain>\w+) '
                    r'(?P<happiness>\d+) happiness units by sitting next to '
                    r'(?P<person2>\w+).'
                    , line)
                groupdict = match.groupdict()
                person1 = groupdict['person1']
                person2 = groupdict['person2']
                gain = groupdict['gain']
                happiness = int(groupdict['happiness'])
                if gain != 'gain':
                    happiness = -happiness
                self.happiness[person1][person2] = happiness
            
            if include_myself:

                for person in list(self.happiness.keys()):
                    self.happiness[person]['me'] = 0
                    self.happiness['me'][person] = 0



def main():
    start_time = time.time()
    
    print('Question 1: Total change in happiness for the optimal seating '
        'arrangement?')
    quiz = Quiz('input.txt', False)
    max_happiness = max(quiz.arrangements.keys())
    print(f'Answer: {max_happiness}')
    # REMARK: 8 identical arrangements (start with different person) 
    # + 8 mirrored arrangements
    print(f'Arrangements: {quiz.arrangements[max_happiness]}')

    print('Question 2: Total change in happiness for the optimal seating '
        'arrangement (including myself)?')
    quiz = Quiz('input.txt', True)
    max_happiness = max(quiz.arrangements.keys())
    print(f'Answer: {max_happiness}')
    # REMARK: 9 identical arrangements (start with different person) 
    # + 9 mirrored arrangements
    print(f'Arrangements: {quiz.arrangements[max_happiness]}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()