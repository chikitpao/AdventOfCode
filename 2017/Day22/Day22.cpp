// Day22.cpp
// AoC 2017 Day 22: Sporifica Virus
// Author: Chi-Kit Pao
//

#include <cassert>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <vector>

enum class Direction
{
	Up = 0,
	Right,
	Down,
	Left
};

class Infection
{
public:
	// carrier properties
	int currentRow = 0;
	int currentColumn = 0;
	Direction currentDir = Direction::Up;
	int infectionCount = 0;

	std::map<std::pair<int, int>, char> nodeMap;	// (row, column) with nodes which is not clean

	virtual void Run()
	{
		std::pair<int, int> currentPosition(currentRow, currentColumn);
		bool infected = (nodeMap.find(currentPosition) != nodeMap.end());
		if (infected)
		{
			TurnRight();
			nodeMap.erase(currentPosition);
		}
		else
		{
			TurnLeft();
			nodeMap.emplace(currentPosition, '#');
			assert(infectionCount + 1 > infectionCount);
			infectionCount++;
		}
	}
protected:
	virtual void TurnLeft()
	{
		switch (currentDir)
		{
		case Direction::Up:
		{
			currentColumn--;
			currentDir = Direction::Left;
		}
		break;
		case Direction::Right:
		{
			currentRow--;
			currentDir = Direction::Up;
		}
		break;
		case Direction::Down:
		{
			currentColumn++;
			currentDir = Direction::Right;
		}
		break;
		case Direction::Left:
		{
			currentRow++;
			currentDir = Direction::Down;
		}
		break;
		}
	}
	virtual void TurnRight()
	{
		switch (currentDir)
		{
		case Direction::Up:
		{
			currentColumn++;
			currentDir = Direction::Right;
		}
		break;
		case Direction::Right:
		{
			currentRow++;
			currentDir = Direction::Down;
		}
		break;
		case Direction::Down:
		{
			currentColumn--;
			currentDir = Direction::Left;
		}
		break;
		case Direction::Left:
		{
			currentRow--;
			currentDir = Direction::Up;
		}
		break;
		}
	}
};

class Infection2 : public Infection
{
public:
	virtual void Run() override
	{
		std::pair<int, int> currentPosition(currentRow, currentColumn);
		auto it = nodeMap.find(currentPosition);
		if (it == nodeMap.end())	// clean
		{
			TurnLeft();
			nodeMap.emplace(currentPosition, 'W');
		}
		else
		{
			if (it->second == 'W')	// weakend
			{
				GoForward();
				it->second = '#';
				assert(infectionCount + 1 > infectionCount);
				infectionCount++;
			}
			else if (it->second == '#')	// infected
			{
				TurnRight();
				it->second = 'F';
			}
			else if (it->second == 'F') // flagged
			{
				GoBackward();
				nodeMap.erase(currentPosition);
			}
			else
				throw std::runtime_error("unknwon state");
		}
	}
protected:
	virtual void GoForward()
	{
		switch (currentDir)
		{
		case Direction::Up:
			currentRow--;
			break;
		case Direction::Right:
			currentColumn++;
			break;
		case Direction::Down:
			currentRow++;
			break;
		case Direction::Left:
			currentColumn--;
			break;
		}
	}
	virtual void GoBackward()
	{
		switch (currentDir)
		{
		case Direction::Up:
		{
			currentRow++;
			currentDir = Direction::Down;
		}
		break;
		case Direction::Right:
		{
			currentColumn--;
			currentDir = Direction::Left;
		}
		break;
		case Direction::Down:
		{
			currentRow--;
			currentDir = Direction::Up;
		}
		break;
		case Direction::Left:
		{
			currentColumn++;
			currentDir = Direction::Right;
		}
		break;
		}
	}
};

Infection infectionMap;
Infection2 infectionMap2;

static void ParseInput(const std::string& fileName)
{
	std::fstream inFile(fileName);
	std::string line;
	std::vector<std::pair<int, int>> infectedNodes;
	int lineCount = 0;
	int columnCount = 0;
	int columnOffset = 0;

	while (std::getline(inFile, line))
	{
		if (lineCount == 0)
		{
			columnCount = static_cast<int>(line.size());
			assert(columnCount % 2);
			columnOffset = -(static_cast<int>(columnCount) / 2);
			
		}
		for (int i = 0; i < line.size(); ++i)
		{
			if (line[i] == '#')
				infectedNodes.emplace_back(lineCount, i);
		}

		lineCount++;
	}
	assert(lineCount % 2);
	int rowOffset = -(static_cast<int>(lineCount) / 2);
	for (const auto& v : infectedNodes)
	{
		infectionMap.nodeMap.emplace(std::pair<int, int>(v.first + rowOffset, v.second + columnOffset), '#');
		infectionMap2.nodeMap.emplace(std::pair<int, int>(v.first + rowOffset, v.second + columnOffset), '#');
	}
}

int main()
{
	ParseInput("input.txt");

	std::cout << "Day 22: " << "\n";
	
	std::cout << "Question 1: Given your actual map, after 10000 bursts of "
		"activity, how many bursts cause a node to become infected?\n";
	for (int i = 0; i < 10000; ++i)
		infectionMap.Run();
	std::cout << "Answer: " << infectionMap.infectionCount << std::endl;
	
	std::cout << "Question 2: Given your actual map, after 10000000 bursts "
		"of activity, how many bursts cause a node to become infected?" << std::endl;
	for (int i = 0; i < 10000000; ++i)
		infectionMap2.Run();
	std::cout << "Answer: " << infectionMap2.infectionCount << std::endl;

	std::cout << std::endl;
	return 0;
}
