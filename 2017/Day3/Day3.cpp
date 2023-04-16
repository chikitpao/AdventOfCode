// Day3.cpp
// AoC 2017 Day 3: Spiral Memory
// Author: Chi-Kit Pao
//

#include <iostream>
#include <map>
#include <numeric>
#include <utility>

static int part1(int puzzleInput)
{
	int root = static_cast<int>(floor(sqrt(puzzleInput)));
	if (root % 2 == 0) {
		root--;
	}
	const int remainder = puzzleInput - (root * root);
	// root^2 < remainder <= root^2 + (root + 1) => right side
	// root^2 + (root + 1) <= remainder <= root^2 + 2 * (root + 1) => upper side
	// root^2 + 2 * (root + 1)  <= remainder <= root^2 + 3 * (root + 1) => left side
	// root^2 + 3 * (root + 1)  <= remainder <= root^2 + 4 * (root + 1) => lower side 
	//    (with root^2 + 4 * (root + 1) = (root + 1)^2 in the lower right corner)
	// Number is in the corner where ranges intersect.
	int x = 0;
	int y = 0;
	int offset = remainder % (root + 1);
	int quotient = remainder / (root + 1);
	switch (quotient)
	{
	case 0:
	{
		x = (root + 1) / 2;
		y = (root + 1) / 2 + offset;
		break;
	}
	case 1:
	{
		x = (root + 1) / 2 - offset;
		y = -(root + 1) / 2;
		break;
	}
	case 2:
	{
		x = -(root + 1) / 2;
		y = -(root + 1) / 2 + offset;
		break;
	}
	case 3:
	{
		x = -(root + 1) / 2 + offset;
		y = (root + 1) / 2;
		break;
	}
	case 4:
	{
		x = (root + 1) / 2;
		y = (root + 1) / 2;
		break;
	}
	default:
		throw std::logic_error("Invalid value");
	}
	int steps = abs(x) + abs(y);
}

static int part2(int puzzleInput)
{
	std::map<std::pair<int, int>, int> memory;
	memory.emplace(std::pair<int, int>(0, 0), 1);
	auto getValue = [&memory](int x, int y)
	{
		int neighbors[8];
		neighbors[0] = (memory.find(std::make_pair(y - 1, x)) != memory.end()) ? (memory.find(std::make_pair(y - 1, x))->second) : 0;
		neighbors[1] = (memory.find(std::make_pair(y - 1, x + 1)) != memory.end()) ? (memory.find(std::make_pair(y - 1, x + 1))->second) : 0;
		neighbors[2] = (memory.find(std::make_pair(y, x + 1)) != memory.end()) ? (memory.find(std::make_pair(y, x + 1))->second) : 0;
		neighbors[3] = (memory.find(std::make_pair(y + 1, x + 1)) != memory.end()) ? (memory.find(std::make_pair(y + 1, x + 1))->second) : 0;
		neighbors[4] = (memory.find(std::make_pair(y + 1, x)) != memory.end()) ? (memory.find(std::make_pair(y + 1, x))->second) : 0;
		neighbors[5] = (memory.find(std::make_pair(y + 1, x - 1)) != memory.end()) ? (memory.find(std::make_pair(y + 1, x - 1))->second) : 0;
		neighbors[6] = (memory.find(std::make_pair(y, x - 1)) != memory.end()) ? (memory.find(std::make_pair(y, x - 1))->second) : 0;
		neighbors[7] = (memory.find(std::make_pair(y - 1, x - 1)) != memory.end()) ? (memory.find(std::make_pair(y - 1, x - 1))->second) : 0;
		return std::accumulate(std::begin(neighbors), std::end(neighbors), 0, std::plus<int>());
	};
	int i = 1;
	while (true)
	{
		// right
		int x = i;
		int y;
		for (y = i - 1; y >= -i; --y)
		{
			int value = getValue(x, y);
			if (value > puzzleInput)
				return value;
			memory.emplace(std::pair<int, int>(y, x), value);
		}
		// upper
		y = -i;
		for (x = i - 1; x >= -i; --x)
		{
			int value = getValue(x, y);
			if (value > puzzleInput)
				return value;
			memory.emplace(std::pair<int, int>(y, x), value);
		}
		// left
		x = -i;
		for (y = -i + 1; y <= i; ++y)
		{
			int value = getValue(x, y);
			if (value > puzzleInput)
				return value;
			memory.emplace(std::pair<int, int>(y, x), value);
		}
		// lower
		y = i;
		for (x = -i + 1; x <= i; ++x)
		{
			int value = getValue(x, y);
			if (value > puzzleInput)
				return value;
			memory.emplace(std::pair<int, int>(y, x), value);
		}

		i++;
	}
	return 0;
}

int main()
{
	const int puzzleInput = 265149; // 513^2 < 265149 < 515^2
	int steps = part1(puzzleInput);
	int answer2 = part2(puzzleInput);

	std::cout << "Day 3: " << "\n";
	std::cout << ("Question 1: How many steps are required to carry the data from the square identified in your puzzle input all the way to the access port?\n");
	std::cout << "Answer: " << steps << "\n";
	std::cout << ("Question 2: What is the first value written that is larger than your puzzle input?\n");
	std::cout << "Answer: " << answer2 << "\n";
	std::cout << std::endl;
}
