""" Advent of Code 2015, Day 10
    Author: Chi-Kit Pao
"""


import time


def debug_output(message):
    # print(message)
    pass


def generate_slices(sequence: str):
    current_pos = 0
    length = len(sequence)
    while current_pos < length:
        ch = sequence[current_pos]
        next_pos = current_pos + 1
        while next_pos < length and sequence[next_pos] == ch:
            next_pos += 1
        yield sequence[current_pos:next_pos]
        current_pos = next_pos

def get_new_sequence(sequence: str, steps: int):
    new_sequence = sequence
    for _ in range(steps):
        old_sequence = new_sequence
        new_sequence = ''
        for s in generate_slices(old_sequence):
            new_sequence += str(len(s)) + s[0]
    return new_sequence


def main():
    start_time = time.time()
    start_sequence = '1321131112'
 
    print('Question 1: Length of sequence after 40 steps?')
    new_sequence = get_new_sequence(start_sequence, 40)
    print(f'Answer: {len(new_sequence)}')

    print('Question 2: Length of sequence after 50 steps?')
    new_sequence = get_new_sequence(new_sequence, 10)
    print(f'Answer: {len(new_sequence)}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()