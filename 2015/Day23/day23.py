""" Advent of Code 2015, Day 23
    Author: Chi-Kit Pao
"""

import os
import time


def debug_output(message):
    # print(message)
    pass


class Quiz:
    def __init__(self, input_file_name: str):
        self.lines: list[str] = None
        self.registers = [0, 0]  # a, b
        self.inst_ptr = 0
        self._read_data(input_file_name)

    def execute_program(self):
        while self.inst_ptr < len(self.lines):
            debug_output(f'Next instruction: {self.lines[self.inst_ptr]}')
            if self.lines[self.inst_ptr].startswith('hlf '):
                # hlf r sets register r to half its current value, then 
                # continues with the next instruction.
                reg_index = 0 if self.lines[self.inst_ptr][4] == 'a' else 1
                self.registers[reg_index] //= 2
                self.inst_ptr += 1
            elif self.lines[self.inst_ptr].startswith('tpl '):
                # tpl r sets register r to triple its current value, then 
                # continues with the next instruction.
                reg_index = 0 if self.lines[self.inst_ptr][4] == 'a' else 1
                self.registers[reg_index] *= 3
                self.inst_ptr += 1
            elif self.lines[self.inst_ptr].startswith('inc '):
                # inc r increments register r, adding 1 to it, then continues
                #  with the next instruction.
                reg_index = 0 if self.lines[self.inst_ptr][4] == 'a' else 1
                self.registers[reg_index] += 1
                self.inst_ptr += 1
            elif self.lines[self.inst_ptr].startswith('jmp '):
                # jmp offset is a jump; it continues with the instruction 
                # offset away relative to itself.
                self.inst_ptr += int(self.lines[self.inst_ptr][4:])
            elif self.lines[self.inst_ptr].startswith('jie '):
                # jie r, offset is like jmp, but only jumps if register r is 
                # even ("jump if even").
                reg_index = 0 if self.lines[self.inst_ptr][4] == 'a' else 1
                if self.registers[reg_index] % 2 == 0:
                    self.inst_ptr += int(self.lines[self.inst_ptr][7:])
                else:
                    self.inst_ptr += 1
            elif self.lines[self.inst_ptr].startswith('jio '):
                # jio r, offset is like jmp, but only jumps if register r is 1 
                # ("jump if one", not odd).
                reg_index = 0 if self.lines[self.inst_ptr][4] == 'a' else 1
                if self.registers[reg_index] == 1:
                    self.inst_ptr += int(self.lines[self.inst_ptr][7:])
                else:
                    self.inst_ptr += 1
            debug_output(f' After instruction: inst_ptr {self.inst_ptr} '
                f'registers {self.registers}')

    def reset(self):
        self.registers = [0, 0]
        self.inst_ptr = 0

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            self.lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: What is the value in register b when the program is '
        'finished executing?')
    quiz.execute_program()
    print(f'Answer: {quiz.registers[1]}')

    print('Question 2: What is the value in register b after the program is '
    'finished executing if register a starts as 1 instead?')
    quiz.reset()
    quiz.registers[0] = 1
    quiz.execute_program()
    print(f'Answer: {quiz.registers[1]}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()