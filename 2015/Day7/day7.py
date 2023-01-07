""" Advent of Code 2015, Day 7
    Author: Chi-Kit Pao
"""

import os
import time


def debug_output(message):
    # print(message)
    pass

class Wire:
    def __init__(self, name: str, expression: list[str]):
        self.name = name,
        self.input1: str = None
        self.input_value1: int = None
        self.input2: str = None
        self.input_value2: int = None
        self.operation: str = None
        self.signal_: int = None
        if len(expression) == 3:
            assert expression[1] in ['AND', 'OR', 'LSHIFT', 'RSHIFT']
            self.input1 = expression[0]
            self.operation = expression[1]
            self.input2 = expression[2]
        elif len(expression) == 2:
            assert expression[0] == 'NOT'
            self.operation = expression[0]
            self.input1 = expression[1]
        elif len(expression) == 1:
            self.operation = ''
            self.input1 = expression[0]
        if self.input1 is not None and self.input1.isdigit():
            self.input_value1 = int(self.input1)
        if self.input2 is not None and self.input2.isdigit():
            self.input_value2 = int(self.input2)

        if self.can_do_operation():
            self.do_operation()

    def can_do_operation(self) -> bool:
        if self.operation in ['AND', 'OR', 'LSHIFT', 'RSHIFT']:
            return self.input_value1 is not None and self.input_value2 is not None
        elif self.operation == 'NOT' or self.operation == '':
            return self.input_value1 is not None

    def do_operation(self):
        if self.operation == '':
            self.signal_ = self.input_value1
        elif self.operation == 'NOT':
            self.signal_ = (~self.input_value1) & 0xFFFF
        elif self.operation == 'AND':
            self.signal_ = self.input_value1 & self.input_value2
        elif self.operation == 'OR':
            self.signal_ = self.input_value1 | self.input_value2
        elif self.operation == 'LSHIFT':
            self.signal_ = self.input_value1 << self.input_value2
        elif self.operation == 'RSHIFT':
            self.signal_ = self.input_value1 >> self.input_value2
    
    def try_substitution(self, quiz: 'Quiz') -> bool:
        done_substitution = False
        if self.operation in ['AND', 'OR', 'LSHIFT', 'RSHIFT', 'NOT', '']:
            if self.input_value1 is None:
                wire = quiz.wires[self.input1]
                if wire.signal_ is not None:
                    self.input_value1 = wire.signal_
                    done_substitution = True
        if self.operation in ['AND', 'OR', 'LSHIFT', 'RSHIFT']:
            if self.input_value2 is None:
                wire = quiz.wires[self.input2]
                if wire.signal_ is not None:
                    self.input_value2 = wire.signal_
                    done_substitution = True
        return done_substitution
                

class Quiz:
    def __init__(self, input_file_name: str, replacement: list[str] = None):
        # name -> Wire
        self.wires: dict[str, Wire] = {} 
        self.pending_wires: list[Wire] = []
        self._read_data(input_file_name, replacement)

    def run(self) -> int:
        while len(self.pending_wires) > 0:
            done_substitution: bool = False
            for wire in self.pending_wires:
                if wire.try_substitution(self):
                    done_substitution = True
                    if wire.can_do_operation():
                        wire.do_operation()
                        self.pending_wires.remove(wire)                    
            assert done_substitution
        return self.wires['a'].signal_

    def _read_data(self, input_file_name: str, replacement: list[str]):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
            for line in lines:
                # Line examples:
                # NOT lk -> ll
                # hz RSHIFT 1 -> is
                # du OR dt -> dv
                # 0 -> c: Use '' as operation
                tokens = line.split()
                assert len(tokens) >= 3 and len(tokens) <= 5
                assert tokens[-2] == '->'
                name = tokens[-1]
                if replacement is not None and name == replacement[0]:
                    wire = Wire(name, [replacement[1]])
                else:
                    wire = Wire(name, tokens[0:-2])
                self.wires[name] = wire
                if wire.signal_ is None:
                    self.pending_wires.append(wire)


def main():
    start_time = time.time()
    print('Question 1: What signal is ultimately provided to wire a?')
    quiz = Quiz('input.txt')
    print(f'Answer: {quiz.run()}')

    print('Question 2: What new signal is ultimately provided to wire a?')
    quiz = Quiz('input.txt', ['b', str(956)])
    print(f'Answer: {quiz.run()}')

    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()