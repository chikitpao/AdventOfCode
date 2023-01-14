""" Advent of Code 2015, Day 21
    Author: Chi-Kit Pao
"""

from itertools import combinations
import time


def debug_output(message):
    print(message)
    pass

class Character:
    def __init__(self, hp: int, damage = 0, armor = 0):
        self.hp = hp
        self.damage = damage
        self.armor = armor

class Game:
    def __init__(self):
        self.me = Character(100)
        self.boss = Character(109, 8, 2)
        self.weapons = [  # Weapons:    Cost  Damage  Armor
            (8, 4, 0),  # Dagger        8     4       0
            (10, 5, 0), # Shortsword   10     5       0                    
            (25, 6, 0),  # Warhammer    25     6       0
            (40, 7, 0),  # Longsword    40     7       0
            (74, 8, 0),  # Greataxe     74     8       0
        ]
        self.armor = [ # Armor:      Cost  Damage  Armor
            (0, 0, 0),  # (No armor)
            (13, 0, 1),  # Leather      13     0       1
            (31, 0, 2),  # Chainmail    31     0       2
            (53, 0, 3),  # Splintmail   53     0       3
            (75, 0, 4),  # Bandedmail   75     0       4
            (102, 0, 5),  # Platemail   102     0       5
        ]
        self.rings = [ # Rings:      Cost  Damage  Armor
            (25, 1, 0),  # Damage +1    25     1       0
            (50, 2, 0),  # Damage +2    50     2       0
            (100, 3, 0),  # Damage +3   100     3       0
            (20, 0, 1),  # Defense +1   20     0       1
            (40, 0, 2),  # Defense +2   40     0       2
            (80, 0, 3),  # Defense +3   80     0       3
        ]
    
    def get_configurations(self):
        for weapon in self.weapons:
            for armor in self.armor:
                # No ring
                cost = weapon[0] + armor[0]
                damage = weapon[1] + armor[1]
                armor_ = weapon[2] + armor[2]
                yield(cost, damage, armor_)
                # 1 ring
                for ring in self.rings:
                    cost = weapon[0] + armor[0] + ring[0]
                    damage = weapon[1] + armor[1] + ring[1]
                    armor_ = weapon[2] + armor[2] + ring[2]
                    yield(cost, damage, armor_)
                # 2 rings
                for ring1, ring2 in combinations(self.rings, 2):
                    cost = weapon[0] + armor[0] + ring1[0] + ring2[0]
                    damage = weapon[1] + armor[1] + ring1[1] + ring2[1]
                    armor_ = weapon[2] + armor[2] + ring1[2] + ring2[2]
                    yield(cost, damage, armor_)

    def hit(self, c1: Character, c2: Character):
        c2.hp -= max(1, c1.damage - c2.armor)

    def reset(self, damage: int, armor: int):
        self.me = Character(100, damage, armor)
        self.boss = Character(109, 8, 2)


def main():
    start_time = time.time()
    game = Game()

    costs_to_win = []
    costs_to_lose = []
    for config in game.get_configurations():
        game.reset(config[1], config[2])
        while True:
            game.hit(game.me, game.boss)
            if game.boss.hp <= 0:
                costs_to_win.append(config[0])
                break
            game.hit(game.boss, game.me)
            if game.me.hp <= 0:
                costs_to_lose.append(config[0])
                break        

    print('Question 1: What is the least amount of gold you can spend and '
        'still win the fight?')
    print(f'Answer: {min(costs_to_win)}')
    print('Question 2: What is the most amount of gold you can spend and '
        'still lose the fight?')
    print(f'Answer: {max(costs_to_lose)}')
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()