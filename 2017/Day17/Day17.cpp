// Day17.cpp
// AoC 2017 Day 17: Spinlock 
// Author: Chi-Kit Pao
//

#include <cassert>
#include <iostream>
#include <vector>

int Part1(const int puzzleInput, const int end, int valueAfter)
{
    std::vector<int> buffer;
    buffer.reserve(end);
    buffer.push_back(0);
    int index = 0;
    int answer = -1;
    for (int i = 1; i < end; ++i)
    {
        // index: position to be inserted before
        index = (index + puzzleInput) % buffer.size() + 1;
        buffer.insert(buffer.begin() + index, i);
    }
    auto it = std::find(buffer.begin(), buffer.end(), valueAfter);
    it++;
    if (it == buffer.end())
        return buffer[0];
    return *it;
}

int Part2(const int puzzleInput, const int end, int /*valueAfter*/)
{
    int index = 0;
    int size = 1;
    int answer = -1;
    for (int i = 1; i < end; ++i)
    {
        // index: position to be inserted before
        index = (index + puzzleInput) % size + 1;
        size++;
        // 0 is always at the first position (no value can be inserted before).
        // Remember value when it's inserted after 0.
        if (index == 1)
            answer = i;
    }
    return answer;
}

int main()
{
    const int puzzleInput = 356;

    std::cout << "Day 17: " << "\n";
    std::cout << ("Question 1: What is the value after 2017 in your completed circular buffer?\n");
    std::cout << "Answer: " << Part1(puzzleInput, 2018, 2017) << "\n";
    std::cout << ("Question 2: What is the value after 0 the moment 50000000 is inserted?\n");
    std::cout << "Answer: " << Part2(puzzleInput, 50000000, 0) << "\n";

    std::cout << std::endl;
    return 0;
}
