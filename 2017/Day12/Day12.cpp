// Day12.cpp
// AoC 2017 Day 12:Digital Plumber
// Author: Chi-Kit Pao
//

// #include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <set>
#include <sstream>
#include <vector>

typedef std::map<int, std::vector<int>> Connections;
Connections connections;

static void DetermineGroup(int currentProgram, std::vector<int>& remainingPrograms, std::vector<std::set<int>>& programGroups){
	std::set<int> connected;
	std::vector<int> workingSet;
	std::set<int> newWorkingSet;
	connected.insert(currentProgram);
	workingSet.push_back(currentProgram);
	bool foundNewConnection = false;
	do
	{
		foundNewConnection = false;
		for (auto c : workingSet)
		{
			for (auto rhs : connections[c])
			{
				if (connected.find(rhs) == connected.end())
				{
					foundNewConnection = true;
					connected.insert(rhs);
					newWorkingSet.insert(rhs);
					remainingPrograms.erase(std::remove_if(remainingPrograms.begin(), remainingPrograms.end(), [rhs](auto v)
						{
							return v == rhs;
						}), remainingPrograms.end());
				}
			}
		}
		workingSet.clear();
		for (auto entry : newWorkingSet)
			workingSet.push_back(entry);
		newWorkingSet.clear();
	} while (foundNewConnection);
	programGroups.emplace_back(connected);
}


int main()
{
	std::fstream inFile("input.txt");
	std::string line;
	std::vector<int> remainingPrograms;
	int counter = 0;
	while (std::getline(inFile, line))
	{
		int lhs = atoi(line.c_str());
		assert(counter == lhs);
		connections.emplace(lhs, std::vector<int>());
		remainingPrograms.push_back(lhs);
		auto pos = line.find("<->");
		assert(pos != std::string::npos);
		if (pos == std::string::npos)
			continue;
		counter++;
		
		pos += 3;
		std::stringstream ss(line.substr(pos));
		std::string word;
		while (ss >> word) {
			int rhs = (word[word.size() - 1] == ',') ? atoi(word.substr(0, word.size() - 1).c_str()) : atoi(word.c_str());
			if (lhs != rhs)
				connections.find(lhs)->second.push_back(rhs);
		}
	}

	
	std::vector<std::set<int>> programGroups;
	do
	{
		int currentProgram = remainingPrograms[0];
		remainingPrograms.erase(remainingPrograms.begin());
		DetermineGroup(currentProgram, remainingPrograms, programGroups);
	} while (remainingPrograms.size());	

	std::cout << "Day 12: " << "\n";
	std::cout << ("Question 1: How many programs are in the group that contains program ID 0?\n");
	std::cout << "Answer: " << programGroups[0].size() << "\n";
	std::cout << ("Question 2: How many groups are there in total?\n");
	std::cout << "Answer: " << programGroups.size() << "\n";

	std::cout << std::endl;
	return 0;
}
