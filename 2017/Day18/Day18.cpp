// Day18.cpp
// AoC 2017 Day 18: Duet
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <sstream>
#include <thread>

#include "Program.hpp"

static void Execute(Program* p)
{
	p->ProcessInstructions();
}

int main()
{
	Program program1;
	Program program20(2, 0);
	Program program21(2, 1);

	std::fstream inFile("input.txt");
	std::string line;
	while (std::getline(inFile, line))
	{
		program1.ParseInstruction(line);
		program20.ParseInstruction(line);
		program21.ParseInstruction(line);
	}

	std::cout << "Day 18: " << "\n";
	std::cout << ("Question 1: What is the value of the recovered frequency "
		"(the value of the most recently played sound) the first time a rcv "
		"instruction is executed with a non-zero value?\n");
	program1.ProcessInstructions();
	// First Output:
	// Recovers frequency of last sound played : 7071.
	std::cout << ("Question 2: Once both of your programs have terminated "
		"(regardless of what caused them to do so), how many times did program "
		"1 send a value?\n");
	program20.AddPartner(&program21);
	program21.AddPartner(&program20);
	std::thread t1(Execute, &program20);
	std::thread t2(Execute, &program21);
	t1.join();
	t2.join();
	std::cout << "Answer: sendCount of program 1: " << program21.sendCount << "\n";

	std::cout << std::endl;
	return 0;
}
