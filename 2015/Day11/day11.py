""" Advent of Code 2015, Day 11
    Author: Chi-Kit Pao
"""


import time


def debug_output(message):
    # print(message)
    pass


def get_new_password(old_password: str) -> str:
    end_password = 'z' * len(old_password)
    ascending_triples = [chr(c)+chr(c+1)+chr(c+2) for c in range(ord('a'), ord('z') - 1)]
    forbidden_letters = ['i','o', 'l']
    pairs = [chr(c)*2 for c in range(ord('a'), ord('z') + 1)]
    test_password = old_password
    while True:
        failed = False
        test_password = increment_password(test_password)

        # Passwords must include one increasing straight of at least three 
        # letters.
        triple_found = False
        for triple in ascending_triples:
            if triple in test_password:
                triple_found = True
                break
        if not triple_found:
            failed = True

        # Passwords may not contain the letters i, o, or l.
        if not failed:
            for ch in forbidden_letters:
                if ch in test_password:
                    failed = True

        # Passwords must contain at least two different, non-overlapping pairs
        # of letters
        if not failed:
            pairs_count = 0
            for pair in pairs:
                if pair in test_password:
                    pairs_count += 1
            if pairs_count < 2:
                failed = True

        if failed:
            if test_password == end_password:
                raise ValueError(f'No result found for {old_password}')
        else:
            return test_password

def increment_password(password: str) -> str:
    str_list = list(password)
    current_pos = len(str_list) - 1
    while True:
        if str_list[current_pos] == 'z':
            str_list[current_pos] = 'a'
            current_pos -= 1
            if current_pos < 0:
                break
        else:
            str_list[current_pos] = chr(ord(str_list[current_pos]) + 1)
            break
    return ''.join(str_list)


def main():
    start_time = time.time()
    old_password = 'vzbxkghb'
 
    print('Question 1: Next password?')
    new_password = get_new_password(old_password)
    print(f'Answer: {new_password}')

    print('Question 2: Next password?')
    new_password = get_new_password(new_password)
    print(f'Answer: {new_password}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()