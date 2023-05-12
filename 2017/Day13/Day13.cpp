// Day13.cpp
// AoC 2017 Day 13: Packet Scanners
// Author: Chi-Kit Pao
//

// #include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
//#include <map>
//#include <set>
#include <sstream>
#include <utility>
#include <vector>

// violation count, severity
std::pair<int, int> CalculateSeverity(std::vector<std::pair<int, int>>& scanners, int start)
{
	int violations = 0;
	int severity = 0;
	for (const auto& entry : scanners)
	{
		int enterTime = start + entry.first;
		bool violated = (enterTime % ((entry.second - 1) * 2)) == 0;
		if (violated)
		{
			violations++;
			severity += entry.first * entry.second;
		}
	}
	return { violations, severity };
}

int main()
{
	std::fstream inFile("input.txt");
	std::string line;
	std::vector<std::pair<int, int>> scanners; // depth, range
	while (std::getline(inFile, line))
	{
		int lhs = atoi(line.c_str());
		auto pos = line.find(":");
		assert(pos != std::string::npos);
		if (pos == std::string::npos)
			continue;
		int depth = atoi(line.substr(0, pos).c_str());
		assert(depth >= 0);
		int range = atoi(line.substr(pos + 1).c_str());
		assert(range > 0);
		scanners.emplace_back(depth, range);
	}
	
	std::pair<int, int> severity1 = CalculateSeverity(scanners, 0);
	int test = 1;
	while (true)
	{
		std::pair<int, int> severity2 = CalculateSeverity(scanners, test);
		if (!severity2.first)
			break;
		++test;
	};

	std::cout << "Day 13: " << "\n";
	std::cout << ("Question 1: How many programs are in the group that contains program ID 0?\n");
	std::cout << "Answer: " << severity1.second << " (" << severity1.first << " violations)\n";
	std::cout << ("Question 2: What is the fewest number of picoseconds that you need to delay the packet to pass through the firewall without being caught?\n");
	std::cout << "Answer: " << test << "\n";

	std::cout << std::endl;
	return 0;
}
