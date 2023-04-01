""" Advent of Code 2016, Day 5
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
        password: str = ''
        while True:
            data = self.secret_key + str(test_var)
            result = hashlib.md5(data.encode())
            if result.hexdigest().startswith(start):
                password += result.hexdigest()[len(start):len(start)+1]
                if len(password) >= 8:
                    return password
            test_var += 1

    def find_input_v2(self, start: str) -> int:
        test_var = 1
        password: str = 'XXXXXXXX'
        filled = 0
        while True:
            data = self.secret_key + str(test_var)
            result = hashlib.md5(data.encode())
            if result.hexdigest().startswith(start):
                try:
                    # int(...) may throw TypeError
                    pos = int(result.hexdigest()[len(start):len(start)+1])
                    value = result.hexdigest()[len(start)+1:len(start)+2]
                    if pos < 8 and password[pos:pos+1] == 'X':
                        password = password[0:pos] + value + password[pos+1:]
                        filled += 1
                        if filled >= 8:
                            return password
                except ValueError:
                    pass
               
            test_var += 1

def main():
    start_time = time.time()
    quiz = Quiz('cxdnnyjw')
 
    print('Question 1: Given the actual Door ID, what is the password?')
    print(f'Answer: {quiz.find_input("0" * 5)}')

    print('Question 2: Given the actual Door ID and this new method, what is the password?')
    print(f'Answer: {quiz.find_input_v2("0" * 5)}')

    # Time elapsed: 56.74828910827637 s
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()