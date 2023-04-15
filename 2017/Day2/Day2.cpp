// Day2.cpp
// AoC 2017 Day 2: Corruption Checksum
// Author: Chi-Kit Pao
//

#include <algorithm>
#include <fstream>
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

// Returns difference between min and max values.
static int evaluateRowPart1(std::vector<int>& values)
{
	if (values.size() <= 1)
		return 0;
	bool firstValue = true;
	int min = 0;
	int max = 0;
	for (auto value: values)
	{
		if (firstValue)
		{
			firstValue = false;
			min = value;
			max = value;
		}
		else
		{
			min = std::min(min, value);
			max = std::max(max, value);
		}
	}
	return max - min;
}

// Returns quotient of two evenly divisible values
static int evaluateRowPart2(std::vector<int>& values)
{
	if (values.size() <= 1)
		return 0;
	std::sort(values.begin(), values.end());
	for (size_t i = 0; i + 1 < values.size(); ++i)
	{
		for (size_t j = i + 1; j < values.size(); ++j)
		{
			if (values[j] % values[i] == 0)
				return values[j] / values[i];
		}
	}
	return 0;
}

int main()
{
	std::fstream inFile("input.txt");
	std::string line;
	const char delimiter = '\t';
	int checksum = 0;
	int answer2 = 0;
	while (std::getline(inFile, line))
	{
		std::string token;
		std::istringstream iss(line);
		std::vector<int> values;
		while (std::getline(iss, token, delimiter))
		{
			if (token.empty())
				continue;
			std::istringstream token_ss(token);
			int value;
			token_ss >> value;
			values.push_back(value);
		}
		checksum += evaluateRowPart1(values);
		answer2 += evaluateRowPart2(values);
	}

	std::cout << "Day 2: " << "\n";
	std::cout << ("Question 1: What is the checksum for the spreadsheet in your puzzle input?\n");
	std::cout << "Answer: " << checksum << "\n";
	std::cout << ("Question 2: What is the sum of each row's result in your puzzle input?\n");
	std::cout << "Answer: " << answer2 << "\n";
	std::cout << std::endl;
}
