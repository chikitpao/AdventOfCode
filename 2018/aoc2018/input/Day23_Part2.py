"""  
    Day23_Part2.py
    AoC 2018 Day 23, Part 2: Experimental Emergency Teleportation
    
    Author: Chi-Kit Pao
    REMARK: Requires library PuLP to run this program.
"""

import os
import pulp as plp
import re

class Nanobot:
    def __init__(self, x, y, z, r):
        self.pos = [x, y, z]
        self.r = r
        self.maxReachable = [x + r, y + r, z + r]
        self.minReachable = [x - r, y - r, z - r]

def main():
    file_path = os.path.dirname(__file__)
    nanobots = []
    with open(os.path.join(file_path, 'Day23_input.txt'), 'r') as f:
        lines = list(map(lambda s: s.replace('\n', ''), f.readlines()))
        for line in lines:
            match = re.match(r'pos=<(-?\d+),(-?\d+),(-?\d+)>, r=(\d+)',
                    line)
            bot = Nanobot(int(match.group(1)), int(match.group(2)), int(match.group(3)), int(match.group(4)))
            nanobots.append(bot)

    maxM = abs(nanobots[0].pos[0])
    for i in range(0, 3):
        maxM = max(maxM, abs(max(nanobots, key = lambda bot: bot.maxReachable[i]).maxReachable[i]))
        maxM = max(maxM, abs(min(nanobots, key = lambda bot: bot.minReachable[i]).minReachable[i]))
    maxM = 6 * (maxM + 1)

    # Objective: find coordinate with maximum coverage
    problem = plp.LpProblem(f'FindMaxCoverage', plp.LpMaximize)
    coverageCount = plp.LpVariable("TOTALCOV", 0, None, cat=plp.LpInteger)
    problem += coverageCount
    # Other decision variables: coordinates x, y, and z
    x = plp.LpVariable('X') #, None, None, cat=plp.LpInteger)
    y = plp.LpVariable('Y') #, None, None, cat=plp.LpInteger)
    z = plp.LpVariable('Z') #, None, None, cat=plp.LpInteger)
    # Add Constraints:
    coverages = [plp.LpVariable(f'COV{i:04d}', 0, 1, cat=plp.LpInteger) for i in range(len(nanobots))]
    problem += coverageCount == sum(coverages)

    for i, bot in enumerate(nanobots):
        for subIndex in range(8):
            signs = [1 if (subIndex & 1) == 0 else -1,
                        1 if (subIndex & 2) == 0 else -1,
                        1 if (subIndex & 4) == 0 else -1]
            temp = bot.r + (bot.pos[0] * signs[0]) + (bot.pos[1] * signs[1]) + (bot.pos[2] * signs[2])
            problem += (signs[0] * x + signs[1] * y + signs[2] * z + maxM * coverages[i]) <= (temp + maxM)

    problem.writeMPS(os.path.join(file_path, 'Day23_Part2_orig.mps'))
    status = problem.solve(plp.PULP_CBC_CMD(msg=False))
    # Output: status: 1
    print(f'status: {status}')
    if status == 1:
        # Output:
        # total coverage: 976.0
        # x: 47812313.0, y: 20634366.0, z: 11715983.0, distance to origin: 80162662.0
        print(f'total coverage: {problem.objective.value()}')
        distanceToOrigin = abs(x.value()) + abs(y.value()) + abs(z.value())
        print(f'x: {x.value()}, y: {y.value()}, z: {z.value()}, distance to origin: {distanceToOrigin}')


if __name__ == '__main__':
    main()
