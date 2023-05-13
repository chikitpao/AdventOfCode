// Day19.cpp
// AoC 2017 Day 19: A Series of Tubes
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <sstream>
#include <utility>
#include <vector>

enum class Direction
{
	Up = 0,
	Left,
	Down,
	Right
};

typedef struct tagSTATE {
	int x;
	int y;
	Direction dir;
} STATE;

class Map
{
private:
	std::vector<std::string> puzzleMap;
	std::vector<char> visited;
	int height = 0;
	int width = 0;
	int stepCount = 1;

public:
	void AddLine(std::string& line);
	bool GetNextStep(STATE& state);
	STATE GetStartState() const;
	int GetStepCount() const { return stepCount; };
	std::string GetVisitedString() const;
};

void Map::AddLine(std::string& line)
{
	if (!width)
		width = static_cast<int>(line.size());
	height++;
	puzzleMap.push_back(line);
}

bool Map::GetNextStep(STATE& state)
{
	char currentTile = puzzleMap[state.y][state.x];
	switch (state.dir)
	{
	case Direction::Up:
	{
		bool validStep = false;
		bool incrementStep = true;
		if (isalpha(currentTile))
		{
			visited.push_back(currentTile);
			if (puzzleMap[state.y - 1][state.x] == ' ')
				break;
			validStep = true;
		}

		if (!validStep && currentTile == '|')
			validStep = true;

		if (!validStep && currentTile == '-')
		{
			if (state.y - 1 < 0)
				break;
			char nextTile = puzzleMap[state.y - 1][state.x];
			if (nextTile == '|' || nextTile == '+' || isalpha(nextTile))
				validStep = true;
			else
				break;
		}

		if (!validStep && currentTile == '+')
		{
			incrementStep = false;
			if (state.x + 1 < width)
			{
				char nextTile = puzzleMap[state.y][state.x + 1];
				if (nextTile == '-')
				{
					state.dir = Direction::Right;
					stepCount++;
					state.x += 1;
					validStep = true;
				}
			}
			if (!validStep && state.x - 1 >= 0)
			{
				char nextTile = puzzleMap[state.y][state.x - 1];
				if (nextTile == '-')
				{
					state.dir = Direction::Left;
					stepCount++;
					state.x -= 1;
					validStep = true;
				}
			}
		}

		if (validStep)
		{
			if (incrementStep)
			{
				stepCount++;
				state.y -= 1;
			}			
			return true;
		}
	}
	break;
	case Direction::Right:
	{
		bool validStep = false;
		bool incrementStep = true;
		if (isalpha(currentTile))
		{
			visited.push_back(currentTile);
			if (puzzleMap[state.y][state.x + 1] == ' ')
				break;
			validStep = true;
		}

		if (!validStep && currentTile == '-')
			validStep = true;

		if (!validStep && currentTile == '|')
		{
			if (state.x + 1 >= width)
				break;
			char nextTile = puzzleMap[state.y][state.x + 1];
			if (nextTile == '-' || nextTile == '+' || isalpha(nextTile))
				validStep = true;
			else
				break;
		}

		if (!validStep && currentTile == '+')
		{
			incrementStep = false;
			if (state.y + 1 < height)
			{
				char nextTile = puzzleMap[state.y + 1][state.x];
				if (nextTile == '|')
				{
					state.dir = Direction::Down;
					stepCount++;
					state.y += 1;
					validStep = true;
				}
			}
			if (!validStep && state.y - 1 >= 0)
			{
				char nextTile = puzzleMap[state.y - 1][state.x];
				if (nextTile == '|')
				{
					state.dir = Direction::Up;
					stepCount++;
					state.y -= 1;
					validStep = true;
				}
			}
		}

		if (validStep)
		{
			if (incrementStep)
			{
				stepCount++;
				state.x += 1;
			}
			return true;
		}
	}
	break;
	case Direction::Down:
	{
		bool validStep = false;
		bool incrementStep = true;
		if (isalpha(currentTile))
		{
			visited.push_back(currentTile);
			if(puzzleMap[state.y + 1][state.x] == ' ')
				break;
			validStep = true;
		}

		if (!validStep && currentTile == '|')
			validStep = true;

		if (!validStep && currentTile == '-')
		{
			if (state.y + 1 >= height)
				break;
			char nextTile = puzzleMap[state.y + 1][state.x];
			if (nextTile == '|' || nextTile == '+' || isalpha(nextTile))
				validStep = true;
			else
				break;
		}

		if (!validStep && currentTile == '+')
		{
			incrementStep = false;
			if (state.x + 1 < width)
			{
				char nextTile = puzzleMap[state.y][state.x + 1];
				if (nextTile == '-')
				{
					state.dir = Direction::Right;
					stepCount++;
					state.x += 1;
					validStep = true;
				}
			}
			if (!validStep && state.x - 1 >= 0)
			{
				char nextTile = puzzleMap[state.y][state.x - 1];
				if (nextTile == '-')
				{
					state.dir = Direction::Left;
					stepCount++;
					state.x -= 1;
					validStep = true;
				}
			}
		}
		
		if(validStep)
		{
			if (incrementStep)
			{
				stepCount++;
				state.y += 1;
			}
			return true;
		}
	}
	break;
	case Direction::Left:
	{
		bool validStep = false;
		bool incrementStep = true;
		if (isalpha(currentTile))
		{
			visited.push_back(currentTile);
			if (puzzleMap[state.y][state.x - 1] == ' ')
				break;
			validStep = true;
		}

		if (!validStep && currentTile == '-')
			validStep = true;

		if (!validStep && currentTile == '|')
		{
			if (state.x - 1 < 0)
				break;
			char nextTile = puzzleMap[state.y][state.x - 1];
			if (nextTile == '-' || nextTile == '+' || isalpha(nextTile))
				validStep = true;
			else
				break;
		}

		if (!validStep && currentTile == '+')
		{
			incrementStep = false;
			if (state.y + 1 < height)
			{
				char nextTile = puzzleMap[state.y + 1][state.x];
				if (nextTile == '|')
				{
					state.dir = Direction::Down;
					stepCount++;
					state.y += 1;
					validStep = true;
				}
			}
			if (!validStep && state.y - 1 >= 0)
			{
				char nextTile = puzzleMap[state.y - 1][state.x];
				if (nextTile == '|')
				{
					state.dir = Direction::Up;
					stepCount++;
					state.y -= 1;
					validStep = true;
				}
			}
		}

		if (validStep)
		{
			if (incrementStep)
			{
				stepCount++;
				state.x -= 1;
			}
			return true;
		}
	}
	break;
	}

	return false;
}

STATE Map::GetStartState() const
{
	return { static_cast<int>(puzzleMap[0].find('|')), 0, Direction::Down };
}


std::string Map::GetVisitedString() const
{
	std::stringstream strStream;
	for (auto c : visited)
		strStream << c;
	return strStream.str();
}


int main()
{
	Map map;
	std::fstream inFile("input.txt");	
	std::string line;
	while (std::getline(inFile, line))
		map.AddLine(line);

	STATE currentState = map.GetStartState();
	while (map.GetNextStep(currentState))
		;


	std::cout << "Day 19: " << "\n";
	std::cout << ("Question 1: What letters will the network packet see (in the "
		"order it would see them) if it follows the path?\n");
	std::cout << "Answer: " << map.GetVisitedString() << "\n";
	std::cout << ("Question 2: How many steps does the packet need to go?\n");
	std::cout << "Answer: " << map.GetStepCount() << "\n";

	std::cout << std::endl;
	return 0;
}
