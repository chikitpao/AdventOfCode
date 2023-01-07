""" Advent of Code 2015, Day 5
    Author: Chi-Kit Pao
"""


from collections import defaultdict
import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.lines: list[str] = None
        self._read_data(input_file_name)

    def count_nice_strings(self) -> int:
        count = 0
        for line in self.lines:
            if Quiz._is_nice_string(line):
                count += 1
        return count

    def count_nice_strings_v2(self) -> int:
        count = 0
        for line in self.lines:
            if Quiz._is_nice_string_v2(line):
                count += 1
        return count

    @staticmethod
    def _find_vowels(string: str) -> int:
        count = 0
        char_list = list(string)
        for ch in ['a', 'e', 'i', 'o', 'u']:
            count += char_list.count(ch)
        return count

    @staticmethod
    def _find_double_letters(string: str) -> bool:
        assert len(string) > 1
        for i in range(len(string) - 1):
            if string[i] == string[i + 1]:
                return True
        return False

    @staticmethod
    def _is_nice_string(string: str) -> bool:
        # 1 It contains at least three vowels.
        if Quiz._find_vowels(string) < 3:
            return False
        # 2 It contains at least one letter that appears twice in a row
        if not Quiz._find_double_letters(string):
            return False
        # 3 It does not contain the strings ab, cd, pq, or xy.
        forbidden_strings = ['ab', 'cd', 'pq', 'xy']
        for s in forbidden_strings:
            if s in string:
                return False
        return True

    @staticmethod
    def _is_nice_string_v2(string: str) -> bool:
        # 1 It contains a pair of any two letters that appears at least twice 
        # in the string without overlapping.
        # To achieve this, store position of all pairs and find 
        # non-overlapping one
        cond1 = False
        pair_dict = defaultdict(list)
        for i in range(0, len(string), 2):
            if i + 1 < len(string): 
                pair_dict[string[i:i+2]].append(i)
        for i in range(1, len(string), 2):
            if i + 1 < len(string): 
                pair_dict[string[i:i+2]].append(i)
        for values in pair_dict.values():
            if max(values) - min(values) > 1:
                cond1 = True
                break
        # 2 It contains at least one letter which repeats with exactly one 
        # letter between them.
        cond2 = False
        assert len(string) >= 3
        for i in range(len(string) - 2):
            if string[i] == string[i + 2]:
                cond2 = True
                break
        return cond1 and cond2

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            self.lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: How many strings are nice?')
    print(f'Answer: {quiz.count_nice_strings()}')

    print('Question 2: How many strings are nice under the new rules?')
    print(f'Answer: {quiz.count_nice_strings_v2()}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()