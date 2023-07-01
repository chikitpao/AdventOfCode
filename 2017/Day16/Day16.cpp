// Day16.cpp
// AoC 2017 Day 16: Permutation Promenade
// Author: Chi-Kit Pao
//

#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <vector>

static void ReadInput(std::vector<std::string>& steps)
{
    steps.clear();

    std::fstream inFile("input.txt");
    std::string line;
    if (!std::getline(inFile, line))
        throw std::exception("getline failed");
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
}

static void ProcessStep(std::vector<char>& programs, const std::string& step)
{
    char instruction = step[0];
    if (step[0] == 's')
    {
        // Spin (sX)
        int spinSize = atoi(step.substr(1).c_str());
        size_t rotateSize = programs.size() - spinSize;
        std::rotate(programs.begin(), programs.begin() + rotateSize, programs.end());
    }
    else if (step[0] == 'x')
    {
        // Exchange (xA/B)
        std::regex regex1("x(\\d+)/(\\d+)");
        std::smatch match1;
        if (!std::regex_match(step, match1, regex1))
            throw std::exception("Invalid syntax for Exchange instruction");
        std::swap(programs[atoi(match1[1].str().c_str())], programs[atoi(match1[2].str().c_str())]);
    }
    else if (step[0] == 'p')
    {
        // Partner (pA/B)
        auto it1 = std::find(programs.begin(), programs.end(), step[1]);
        auto it2 = std::find(programs.begin(), programs.end(), step[3]);
        std::iter_swap(it1, it2);
    }
}

std::string Part1()
{
    std::vector<char> programs;
    for (int i = 0; i < 16; ++i)
        programs.push_back('a' + i);

    std::vector<std::string> steps;
    ReadInput(steps);
    for (const auto& step : steps)
        ProcessStep(programs, step);
    std::stringstream strStream;
    for (auto c : programs)
        strStream << c;
    return strStream.str();
}

std::string Part2()
{
    std::vector<std::vector<char>> history;

    std::vector<char> programs;
    for (int i = 0; i < 16; ++i)
        programs.push_back('a' + i);

    history.emplace_back(std::vector<char>(programs));
    
    std::vector<std::string> steps;
    ReadInput(steps);

    bool foundCycle = false;
    const int end = 1000000000;
    for (int i = 0; i < end; ++i)
    {
        for (const auto& step : steps)
            ProcessStep(programs, step);
        if (!foundCycle)
        {
            auto it = std::find(history.begin(), history.end(), programs);
            if (it != history.end())
            {
                int distance = static_cast<int>(std::distance(history.begin(), it));
                std::cout << "## i " << i << " identical with entry " << distance << "\n";
                int cycle = i - distance + 1;
                int q = (end - i) / cycle;
                i += (q * cycle);
                foundCycle = true;
            }
            history.emplace_back(std::vector<char>(programs));
        }
    }

    std::stringstream strStream;
    for (auto c : programs)
        strStream << c;
    return strStream.str();
}

int main()
{
    std::cout << "Day 16: " << "\n";
    std::cout << ("Question 1: In what order are the programs standing after their dance?\n");
    std::cout << "Answer: " << Part1() << "\n";
    std::cout << ("Question 2: In what order are the programs standing after their billion dances?\n");
    std::cout << "Answer: " << Part2() << "\n";

    std::cout << std::endl;
    return 0;
}
