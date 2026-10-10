""" AoC 2020 Day 24: Lobby Layout
    Author: Chi-Kit Pao

    Outputs:
    Question 1: How many tiles are left with the black side up?
    Answer: 339
    Question 2: How many tiles will be black after 100 days?
    Answer: 3794
    Time elapsed: 1.6182103157043457 s
"""

import os
import time

NORTHEAST = 0
EAST = 1
SOUTHEAST = 2
SOUTHWEST = 3
WEST = 4
NORTHWEST = 5

def doStep(pos, step):
    if step == NORTHEAST:
        return (pos[0] + 1, pos[1] + 1) if (pos[0] % 2 == 0) else (pos[0] + 1, pos[1])
    elif step == EAST:
        return (pos[0] + 2, pos[1])
    elif step == SOUTHEAST:
        return (pos[0] + 1, pos[1]) if (pos[0] % 2 == 0) else (pos[0] + 1, pos[1] - 1)
    elif step == SOUTHWEST:
        return (pos[0] - 1, pos[1]) if (pos[0] % 2 == 0) else (pos[0] - 1, pos[1] - 1)
    elif step == WEST:
        return (pos[0] - 2, pos[1])
    elif step == NORTHWEST:
        return (pos[0] - 1, pos[1] + 1) if (pos[0] % 2 == 0) else (pos[0] - 1, pos[1])
    else:
        raise RuntimeError('Unknown direction')

def toggleTile(tiles, pos):
    if pos in tiles:
        tiles.remove(pos)
    else:
        tiles.add(pos)

def read_data(input_file_name: str):
    instructions = []
    file_path = os.path.dirname(__file__)
    with open(os.path.join(file_path, input_file_name), 'r') as f:
        lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
        for line in lines:
            current_instruction = []
            cursor = 0
            while cursor < len(line):
                if line[cursor] == 'e':
                    current_instruction.append(EAST)
                    cursor += 1
                elif line[cursor] == 'w':
                    current_instruction.append(WEST)
                    cursor += 1
                elif line[cursor] == 's':
                    if line[cursor+1] == 'e':
                       current_instruction.append(SOUTHEAST)
                    elif line[cursor+1] == 'w':
                       current_instruction.append(SOUTHWEST)
                    else:
                        raise RuntimeError('Unknown token')
                    cursor += 2
                elif line[cursor] == 'n':
                    if line[cursor+1] == 'e':
                       current_instruction.append(NORTHEAST)
                    elif line[cursor+1] == 'w':
                       current_instruction.append(NORTHWEST)
                    else:
                        raise RuntimeError('Unknown token')
                    cursor += 2
                else:
                    raise RuntimeError('Unknown token')
            instructions.append(current_instruction)
    return instructions

def getNeighbors(tile):
    result = []
    for i in range(6):
        result.append(doStep(tile, i))
    return result

def getWhiteCandidates(tiles):
    result = set()
    for tile in tiles:
        neighbors = getNeighbors(tile)
        for neighbor in neighbors:
            if not (neighbor in tiles):
                result.add(neighbor)
    return result

def getBlackToWhite(tiles):
    result = set()
    for tile in tiles:
        neighbors = getNeighbors(tile)
        blackNeighborCount = list(map(lambda n: n in tiles, neighbors)).count(True)
        if not (blackNeighborCount in [1, 2]):
            result.add(tile)
    return result

def getWhiteToBlack (tiles, whiteCandidates):
    result = set()
    for tile in whiteCandidates:
        neighbors = getNeighbors(tile)
        blackNeighborCount = list(map(lambda n: n in tiles, neighbors)).count(True)
        if blackNeighborCount == 2:
            result.add(tile)
    return result

def part2(tiles):
    newTiles = tiles.copy()
    for _ in range(100):
        blackToWhite = getBlackToWhite(newTiles)
        whiteCandidates = getWhiteCandidates(newTiles)
        whiteToBlack = getWhiteToBlack(newTiles, whiteCandidates)
        newTiles = newTiles.difference(blackToWhite)
        newTiles = newTiles.union(whiteToBlack)
    return len(newTiles)

def main():
    start_time = time.time()
    instructions = read_data('input.txt')
    
    print('Question 1: How many tiles are left with the black side up?')
    tiles = set()
    for instruction in instructions:
        pos = (0, 0)
        for step in instruction:
            pos = doStep(pos, step)
        toggleTile(tiles, pos)
    answer1 = len(tiles)
    print(f'Answer: {answer1}')

    print('Question 2: How many tiles will be black after 100 days?')
    answer2 = part2(tiles)
    print(f'Answer: {answer2}')

    print(f'Time elapsed: {time.time() - start_time} s')

if __name__ == '__main__':
    main()
