""" Advent of Code 2015, Day 22
    Author: Chi-Kit Pao
"""

import copy
import time


def debug_output(message):
    # print(message)
    pass


g_mana_to_win = None

def reset_mana_to_win():
    global g_mana_to_win
    g_mana_to_win = None

def update_mana_to_win(mana: int):
    global g_mana_to_win
    g_mana_to_win = mana if g_mana_to_win is None else min(g_mana_to_win, mana)


class GameState:
    MIN_NANA_FOR_SPELL = 53
    def __init__(self, hard_mode: bool):
        self.hard_mode = hard_mode
        self.mana_used = 0
        self.me = [50, 500]  # hp, mana
        self.boss = [71, 10] # hp, damage
        self.current_spells = [] # list of spell and remaining turns

    def boss_attack(self):
        has_shield = any(item[0] == 2 for item in self.current_spells)
        armor_points = 7 if has_shield else 0
        self.me[0] -= max(1, self.boss[1] - armor_points)

        for spell_ in self.current_spells:
            if spell_[0] == 3:
                self.boss[0] -= 3
            elif spell_[0] == 4:
                self.me[1] += 101
        self.current_spells = [x for x in self.current_spells if x[1] > 0]
        for x in self.current_spells:
            x[1] -= 1

    def cast_spell(self, spell: int):
        spell_to_add = None
        if spell == 0:
            # Magic Missile costs 53 mana. It instantly does 4 damage.
            self.me[1] -= 53
            self.mana_used += 53
            self.boss[0] -= 4
        elif spell == 1:
            # Drain costs 73 mana. It instantly does 2 damage and heals you 
            # for 2 hit points.
            self.me[1] -= 73
            self.mana_used += 73
            self.boss[0] -= 2
            self.me[0] += 2
        elif spell == 2:
            # Shield costs 113 mana. It starts an effect that lasts for 6 
            # turns. While it is active, your armor is increased by 7.
            self.me[1] -= 113
            self.mana_used += 113
            spell_to_add = [2, 5]
        elif spell == 3:
            # Poison costs 173 mana. It starts an effect that lasts for 6 
            # turns. At the start of each turn while it is active, it deals 
            # the boss 3 damage.
            self.me[1] -= 173
            self.mana_used += 173
            spell_to_add = [3, 5]
        elif spell == 4:
            # Recharge costs 229 mana. It starts an effect that lasts for 5 
            # turns. At the start of each turn while it is active, it gives 
            # you 101 new mana.
            self.me[1] -= 229
            self.mana_used += 229
            spell_to_add = [4, 4]

        for spell_ in self.current_spells:
            if spell_[0] == 3:
                self.boss[0] -= 3
            elif spell_[0] == 4:
                self.me[1] += 101
        self.current_spells = [x for x in self.current_spells if x[1] > 0]
        for x in self.current_spells:
            x[1] -= 1
        if spell_to_add is not None:
            self.current_spells.append(spell_to_add)

    def do_next(self):
        possible_spells = self.get_possible_spells()
        if len(possible_spells) == 0:
            return -1
        for spell in possible_spells:
            new_state = copy.deepcopy(self)
            if new_state.hard_mode:
                new_state.me[0] -= 1
            new_state.cast_spell(spell)
            if new_state.boss[0] <= 0:
                update_mana_to_win(new_state.mana_used)
                return 1
            new_state.boss_attack()
            if new_state.me[0] <= 0:
                return -1
            new_state.do_next()
        return 0 

    def get_possible_spells(self):
        if self.me[1] < GameState.MIN_NANA_FOR_SPELL:
            return []
        result = [0, 1, 2, 3, 4]
        for spell in self.current_spells:
            if spell[1] > 0:
                result.remove(spell[0])
        return result

def main():
    start_time = time.time()

    print('Question 1: What is the least amount of mana you can spend and '
        'still win the fight?')
    state = GameState(False)
    state.do_next()
    print(f'Answer: {g_mana_to_win}')

    print('Question 2: What is the least amount of mana you can spend and '
        'still win the fight (in hard mode)?')
    reset_mana_to_win()
    state = GameState(True)
    state.do_next()
    print(f'Answer: {g_mana_to_win}')

    # Time elapsed: 15.947471141815186 s
    print(f'Time elapsed: {time.time() - start_time} s')


if __name__ == '__main__':
    main()