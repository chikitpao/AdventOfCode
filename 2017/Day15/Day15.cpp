// Day15.cpp
// AoC 2017 Day 15: Dueling Generators
// Author: Chi-Kit Pao
//

#include <iostream>

const long long multipliers[] = { 16807, 48271 };
const long long divisor = 2147483647;

int CountPart1()
{
    long long workingValues[] = { 516, 190 };
    long long round = 0;
    int count = 0;
    while (true)
    {
        for (int i = 0; i < 2; ++i)
            workingValues[i] = (workingValues[i] * multipliers[i]) % divisor;
        bool compareResult = (workingValues[0] & 0x0000FFFF) == (workingValues[1] & 0x0000FFFF);
        if (compareResult)
            count++;

        round++;
        if (round >= 40000000)
            break;
    }
    return count;
}

int CountPart2()
{
    long long workingValues[] = { 516, 190 };
    long long round = 0;
    int count = 0;
    while (true)
    {
        do
        {
            workingValues[0] = (workingValues[0] * multipliers[0]) % divisor;
        } while ((workingValues[0] & 0x3 ) != 0);
        do
        {
            workingValues[1] = (workingValues[1] * multipliers[1]) % divisor;
        } while ((workingValues[1] & 0x7) != 0);
         
        bool compareResult = (workingValues[0] & 0x0000FFFF) == (workingValues[1] & 0x0000FFFF);
        if (compareResult)
            count++;

        round++;
        if (round >= 5000000)
            break;
    }
    return count;
}

int main()
{
    std::cout << "Day 15: " << "\n";
    std::cout << ("Question 1: After 40 million pairs, what is the judge's final count?\n");
    int count1 = CountPart1();
    std::cout << "Answer: " << count1 << "\n";
    std::cout << ("Question 2: After 5 million pairs, but using this new generator logic, what is the judge's final count?\n");
    int count2 = CountPart2();
    std::cout << "Answer: " << count2 << "\n";

    std::cout << std::endl;
    return 0;
}
