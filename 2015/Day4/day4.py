""" Advent of Code 2015, Day 4
    Author: Chi-Kit Pao
"""

import hashlib
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, secret_key: str):
        self.secret_key = secret_key

    def find_input(self, start: str) -> int:
        test_var = 1
        while True:
            data = self.secret_key + str(test_var)
            result = hashlib.md5(data.encode())
            if result.hexdigest().startswith(start):
                return test_var
            test_var += 1


def main():
    start_time = time.time()
    quiz = Quiz('ckczppom')
 
    print('Question 1: Input for MD5 hash to start with five zeros?')
    print(f'Answer: {quiz.find_input("0" * 5)}')

    print('Question 2: Input for MD5 hash to start with six zeros?')
    print(f'Answer: {quiz.find_input("0" * 6)}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()