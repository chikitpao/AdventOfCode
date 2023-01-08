""" Advent of Code 2015, Day 15
    Author: Chi-Kit Pao
    REMARK: Requires library PuLP to run this program.
"""

import itertools
import math
import os
import re
import time


def debug_output(message):
    # print(message)
    pass

class Ingredient:
    CAPACITY = 0
    DURABILITY = 1
    FLAVOR = 2
    TEXTURE = 3
    CALORIES = 4
    MAX_PROPERTIES = 5

    def __init__(self, name: str):
        self.name = name
        self.index = None
        self.properties: list[int] = [0] * Ingredient.MAX_PROPERTIES


class Quiz:
    def __init__(self, input_file_name: str):
        self.ingredients: list[Ingredient] = []
        self._read_data(input_file_name)

    def run(self, totalCalories: int = None) -> int:
        """ Total score of the highest-scoring cookie (ignoring calories) """
        AMOUNT = 100
        max_score = 0
        for t in itertools.product(range(AMOUNT+1), repeat=len(self.ingredients)):
            if sum(t) != AMOUNT:
                continue
            # REMARK: 176851 tuples
            if totalCalories is not None:
                calories = 0
                for i, amount in enumerate(t):
                    calories += amount * self.ingredients[i].properties[Ingredient.CALORIES]
                if calories != totalCalories:
                    continue

            scores = [0] * (Ingredient.TEXTURE + 1) # Exclude calories
            for j in range(Ingredient.TEXTURE + 1):
                for i, amount in enumerate(t):
                    scores[j] += amount * self.ingredients[i].properties[j]
                scores[j] = max(0, scores[j])
            max_score = max(max_score, math.prod(scores))
        return max_score

    def _read_data(self, input_file_name: str):
        file_path = os.path.dirname(__file__)
        with open(os.path.join(file_path, input_file_name), 'r') as f:
            lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
        # Line Example:
        # Sugar: capacity 3, durability 0, flavor 0, texture -3, calories 2.
        for line in lines:
            match = re.match(r'(?P<ingredient>\w+): capacity '
                r'(?P<capacity>[-]?\d+), durability (?P<durability>[-]?\d+), '
                r'flavor (?P<flavor>[-]?\d+), texture (?P<texture>[-]?\d+), '
                r'calories (?P<calories>[-]?\d+)'
                , line)
            groupdict = match.groupdict()
            ingredient = Ingredient(groupdict['ingredient'])
            ingredient.index = len(self.ingredients)
            ingredient.properties[ingredient.CAPACITY] = int(groupdict['capacity'])
            ingredient.properties[ingredient.DURABILITY] = int(groupdict['durability'])
            ingredient.properties[ingredient.FLAVOR] = int(groupdict['flavor'])
            ingredient.properties[ingredient.TEXTURE] = int(groupdict['texture'])
            ingredient.properties[ingredient.CALORIES] = int(groupdict['calories'])
            self.ingredients.append(ingredient)


def main():
    start_time = time.time()
    quiz = Quiz('input.txt')

    print('Question 1: Total score of the highest-scoring cookie (ignoring '
        'calories)?')
    print(f'Answer: {quiz.run()}')

    print('Question 2: Total score of the highest-scoring cookie (with 500 '
        'calories)?')
    print(f'Answer: {quiz.run(totalCalories=500)}')

    # REMARK: ca. 27 s
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()