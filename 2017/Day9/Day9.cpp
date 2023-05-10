// Day9.cpp
// AoC 2017 Day 9: Stream Processing 
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <sstream>

int totalScore = 0;
int groupCount = 0;
bool garbageMode = false;
bool cancelCharacter = false;
int garbageCount = 0;
int totalGarbageCount = 0;

static void Parse(char c)
{
	switch (c)
	{
	case '{':
		if (!garbageMode)
			groupCount++;
		else if (!cancelCharacter)
			garbageCount++;
		break;
	case '}':
		if (!garbageMode)
		{
			totalScore += groupCount;
			groupCount--;
		}
		else if (!cancelCharacter)
			garbageCount++;
		break;
	case '<':
		if(!garbageMode)
			garbageMode = true;
		else if(!cancelCharacter)
			garbageCount++;
		break;
	case '>':
		if (garbageMode && !cancelCharacter)
		{
			garbageMode = false;
			totalGarbageCount += garbageCount;
			garbageCount = 0;
		}
		break;
	case '!':
		if (garbageMode && !cancelCharacter)
		{
			cancelCharacter = true;
			return;
		}			
		break;
	default:
		if (garbageMode && !cancelCharacter)
			garbageCount++;
		break;
	}
	cancelCharacter = false;
}

int main()
{
	std::fstream inFile("input.txt");

	std::string line;
	if (!std::getline(inFile, line))
		return 1;
	for (auto c : line)
		Parse(c);
	
	std::cout << "Day 9: " << "\n";
	std::cout << ("Question 1: What is the total score for all groups in your input?\n");
	std::cout << "Answer: " << totalScore << "\n";
	std::cout << ("Question 2: How many non-canceled characters are within the garbage in your puzzle input?\n");
	std::cout << "Answer: " << totalGarbageCount << "\n";

	std::cout << std::endl;
	return 0;
}
