// Day11.cpp
// AoC 2017 Day 11: Hex Ed
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <sstream>
#include <vector>

typedef std::pair<int, int> Pos;
void ProcessStep(Pos& coordinates, const std::string& step)
{
    // My definitions:
    // Going north from even to odd: y coordinate remains the same.
    // Going south from even to odd: y coordinate decreases by one.
    // Going north from odd to even: y coordinate increases by one.
    // Going south from odd to even: y coordinate remains the same.
    if (step == "n")
    {
        coordinates.second++;
    }
    else if (step == "ne")
    {
        if(coordinates.first % 2)
            coordinates.second++;
        coordinates.first++;
        
    }
    else if (step == "se")
    {
        if (coordinates.first % 2 == 0)
            coordinates.second--;
        coordinates.first++;
    }
    else if (step == "s")
    {
        coordinates.second--;
    }
    else if (step == "sw")
    {
        if (coordinates.first % 2 == 0)
            coordinates.second--;
        coordinates.first--;
    }
    else if (step == "nw")
    {
        if (coordinates.first % 2)
            coordinates.second++;
        coordinates.first--;
    }
    else
    {
        throw std::exception("unknown step");
    }
}

int GetSteps(const Pos& coordinates)
{
    if (abs(coordinates.second) >= abs(coordinates.first))
    {
        // Move diagonally until column (and spare half of horizontal step counts), then move vertically.
        return abs(coordinates.second) + abs(coordinates.first) / 2;
    }
    else
    {
        // Move diagonally until row (and spare half of horizontal step counts), then move horizontally.
        return abs(coordinates.first) + abs(coordinates.second) / 2;
    }
}

int main()
{
    std::fstream inFile("input.txt");

    std::string line;
    if (!std::getline(inFile, line))
        throw std::exception("getline failed");

    std::vector<std::string> steps;
    do
    {
        auto pos = line.find(',');
        if (pos != std::string::npos)
        {
            steps.push_back(line.substr(0, pos));
            line = line.substr(pos + 1);
        }
        else
        {
            steps.push_back(line.c_str());
            line = "";
        }
    } while (line.size());

    Pos coordinates(0, 0);
    int maxSteps = 0;
    for (auto& step : steps)
    {
        ProcessStep(coordinates, step);
        maxSteps = std::max(maxSteps, GetSteps(coordinates));
    }

    std::cout << "Day 11: " << "\n";
    std::cout << ("Question 1: How many steps are required to reach the child process?\n");
    std::cout << "Answer: (" << coordinates.first << "," << coordinates.second << ") => " << GetSteps(coordinates) << " steps\n";
    std::cout << ("Question 2: How many steps away is the furthest he ever got from his starting position?\n");
    std::cout << "Answer: " << maxSteps << " steps\n";

    std::cout << std::endl;
    return 0;
}
