""" Advent of Code 2015, Day 19
    Author: Chi-Kit Pao
"""

from collections import defaultdict
import os
import re
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.replacements = defaultdict(list)
        self.reductions = defaultdict(str)
        self.start_molecule: str = None
        self.non_terminals: set[str] = None
        self.terminals: set[str] = set()
        # non-terminal symbols -> targets containing them
        self.non_terminal_targets = defaultdict(set)
        # terminal symbols -> targets containing them
        self.terminal_targets = defaultdict(set)
        self._read_data(input_file_name)

    def do_replacement(self) -> set[str]:
        start = self.start_molecule
        result = set()
        for key, values in self.replacements.items():
            matches = re.finditer(rf'{key}', start)
            for match in matches:
                for value in values:
                    new_molecule = ''.join([start[0: match.start()], value, \
                        start[match.end():]])
                    result.add(new_molecule)
        return result

    def do_reduction(self) -> int:
        steps = 0
        molecule = self.start_molecule
        while molecule != 'e':
            for target, source in self.reductions.items():
                if target in molecule:
                    molecule = molecule.replace(target, source, 1)
                    steps += 1
        return steps

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            assert len(lines[-2]) == 0
            self.start_molecule = lines[-1]
            for line in lines[:-2]:
                tokens = line.split()
                self.replacements[tokens[0]].append(tokens[2])
            self.non_terminals = set(self.replacements.keys())
            for source, targets in self.replacements.items():
                for target in targets:
                    assert target not in self.reductions
                    self.reductions[target] = source
            for target in self.reductions.keys():
                matches = re.findall(r'([A-Z][a-z]?)', target)
                for match in matches:
                    if match not in self.non_terminals:
                        self.terminals.add(match)
                        self.terminal_targets[match].add(target)
                    else:
                        self.non_terminal_targets[match].add(target)
            debug_output(f'Terminals: {self.terminals}')
            debug_output(f'Non-terminals: {self.non_terminals}')
            debug_output(f'Non-Terminals -> Targets:')
            for key, item in self.non_terminal_targets.items():
                debug_output(f'  {key}: {item}')
            debug_output(f'Terminals -> Targets:')
            for key, item in self.terminal_targets.items():
                debug_output(f'  {key}: {item}')
            debug_output(f'Reductions:')
            for key, item in self.reductions.items():
                debug_output(f'  {key}: {item}')

def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: How many distinct molecules can be created after all '
        'the different ways you can do one replacement on the medicine '
        'molecule?')
    print(f'Answer: {len(quiz.do_replacement())}')
    print('Question 2: What is the fewest number of steps to go from e to the'
        ' medicine molecule?')
    print(f'Answer: {quiz.do_reduction()}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()