// Day5.cpp
// AoC 2017 Day 5: A Maze of Twisty Trampolines, All Alike
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

static int CountSteps(std::vector<int>& values, bool v2 = false)
{
	auto copy = values;
	int index = 0;
	int steps = 0;
	while (index >= 0 && index < copy.size())
	{
		steps++;
		int offset = copy[index];
		if (v2 && offset >= 3)
			copy[index]--;
		else
			copy[index]++;
		index += offset;
	}
	return steps;
}

int main()
{
	std::fstream inFile("input.txt");
	std::string line;
	std::vector<int> values;
	while (std::getline(inFile, line))
	{
		std::istringstream iss(line);
		int value;
		iss >> value;
		values.push_back(value);
	}
	int answer1 = CountSteps(values);
	int answer2 = CountSteps(values, true);

	std::cout << "Day 5: " << "\n";
	std::cout << ("Question 1: How many steps does it take to reach the exit?\n");
	std::cout << "Answer: " << answer1 << "\n";
	std::cout << ("Question 2: How many steps does it now take to reach the exit?\n");
	std::cout << "Answer: " << answer2 << "\n";
	std::cout << std::endl;
}
