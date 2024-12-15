# https://github.com/drathke924/userscripts/blob/master/Advent_of_Code_2016/19.py

import math 

# data = 3018458
data = 3012210


binary = bin(data)[3:] + bin(data)[2]
print("Part one")
print(int(binary, 2))



#Slow and useless
'''
def josephus(data):
	elves = []
	for i in range(1, data + 1):
		elves.append(i)

	size = len(elves)
	while size > 1:
		half = math.floor(size/2)
		elves.pop(half)
		elves.append(elves.pop(0))
		size = len(elves)
	return elves[0]
'''


largest = 1
current = 1
working = 1
for i in range(1, data + 1):
	current = i
	if working + 2 > current:
		largest = working
		working = 1
	elif working < largest:
		working += 1
	else:
		working += 2

print("Part two")
print(working)

#####
def f(n):
    largest = 1
    current = 1
    working = 1
    for i in range(1, n + 1):
        current = i
        if working + 2 > current:
            largest = working
            working = 1
        elif working < largest:
            working += 1
        else:
            working += 2
    return working

for i in range(1, 100):
  print(f'i {i}: f(i) {f(i)}')