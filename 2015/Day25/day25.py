""" Advent of Code 2015, Day 25
    Author: Chi-Kit Pao
"""

import time


def get_index(r, c):
    # index of (r, 1): 1 + Sum from 1 to (r - 1)
    # index of (r, c), c > 1: index of (r + c - 1, 1) + (c - 1)
    offset = c - 1
    row = r + offset
    row_1_index = row if row <= 2 else 1 + (row * (row - 1)) // 2
    return row_1_index + offset

def main():
    start_time = time.time()

    row = 2978
    column = 3083
    factor = 20151125
    base = 252533
    mod_value = 33554393
    # result = (factor * (base ** (index - 1))) mod mod_value
    # Since mod_value is prime and base != 1 mod mod_value, there is a cycle 
    # of (mod_value - 1). Actually we don't make use of the cycle here
    # because index < mod_value.
    index = get_index(row, column)
    print(index, mod_value - 1 - index)
    exponent = (index - 1) % (mod_value - 1)
    # Too slow:
    # result = (factor * (base ** exponent)) % mod_value
    # Faster:
    result = 1
    for i in range(exponent):
        result *= base
        result %= mod_value
    result *= factor
    result %= mod_value

    print('Question: What code do you give the machine?')
    print(f'Answer: {result}')
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()