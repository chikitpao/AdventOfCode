// Day6.cpp
// AoC 2017 Day 6: Memory Reallocation
// Author: Chi-Kit Pao
//

#include <algorithm>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

static const std::string GetHash(std::vector<int> values) {
	std::stringstream strstream;
	for (auto v : values)
		strstream << v << ",";
	return strstream.str();
}

static int DistributeBlocks(std::vector<int>& values, std::set<std::string>& seen)
{
	int steps = 0;
	std::string hash = GetHash(values);
	seen.insert(hash);
	while (true)
	{
		auto max_element = std::max_element(values.begin(), values.end());
		auto index = (std::distance(values.begin(), max_element) + 1) % values.size();
		int blocks = *max_element;
		*max_element = 0;
		while (blocks > 0) {
			++values[index];
			index = (index + 1) % values.size();
			--blocks;
		}
		++steps;
		hash = GetHash(values);
		if (seen.find(hash) != seen.end())
			break;
		seen.insert(hash);
	}
	return steps;
}

int main()
{
	std::fstream inFile("input.txt");
	std::string line;
	if (!std::getline(inFile, line))
		return 1;
	
	std::string token;
	std::istringstream iss(line);
	std::vector<int> values;
	while (std::getline(iss, token, '\t'))
	{
		if (token.empty())
			continue;
		std::istringstream token_ss(token);
		int value;
		token_ss >> value;
		values.push_back(value);
	}
	
	std::set<std::string> seen;
	int answer1 = DistributeBlocks(values, seen);
	seen.clear();
	int answer2 = DistributeBlocks(values, seen);

	std::cout << "Day 6: " << "\n";
	std::cout << ("Question 1: Given the initial block counts in your puzzle "
		"input, how many redistribution cycles must be completed before a "
		"configuration is produced that has been seen before?\n");
	std::cout << "Answer: " << answer1 << "\n";
	std::cout << ("Question 2: How many cycles are in the infinite loop that "
		"arises from the configuration in your puzzle input?\n");
	std::cout << "Answer: " << answer2 << "\n";
	std::cout << std::endl;
	return 0;
}
