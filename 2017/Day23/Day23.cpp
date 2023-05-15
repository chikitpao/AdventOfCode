// Day23.cpp
// AoC 2017 Day 23: Coprocessor Conflagration
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

static long long func();
static int func2();

int main()
{
	Program program1;
	Program program2(2);

	std::fstream inFile("input.txt");
	std::string line;
	while (std::getline(inFile, line))
	{
		program1.ParseInstruction(line);
		program2.ParseInstruction(line);
	}

	std::cout << "Day 23: " << "\n";
	std::cout << "Question 1: How many times is the mul instruction invoked?\n";
	program1.ProcessInstructions();
	std::cout << "Answer: " << program1.GetMulCounter() << "\n";
	std::cout << "Question 2: After setting register a to 1, if the program were "
		"to run to completion, what value would be left in register h?" << std::endl;
	//program2.ProcessInstructions();
	//std::cout << "Answer: " << program2.GetRegister('h') << "\n";
	std::cout << "Answer: " << func2() << "\n";
	
	std::cout << std::endl;
	return 0;
}

static long long func()
{
	long long a = 1, b = 0, c = 0, d = 0, e = 0, f = 0, g = 0, h = 0;
	b = 67;
	c = b;
	if (a) goto label1;
	goto label2;
label1: b *= 100;
	b -= (-100000);
	c = b;
	c -= (-17000);
label2: f = 1;
	d = 2;
label5: e = 2;
label4: g = d;
	g *= e;
	g -= b;
	if (g) goto label3;
	f = 0;
label3: e -= (-1);
	g = e;
	g -= b;
	if (g) goto label4; // loop until e = b (with b = 106700 + n * 17)
	d -= (-1);
	g = d;
	g -= b;
	if (g) goto label5; // continue with next line if g = 0 => d = b  (with b = 106700 + n * 17)
	if (f) goto label6; // continue with next line if f != 0 => none of d values were factor of b
	h -= (-1);
label6: g = b;
	g -= c;
	if (g) goto label7;
	return h;
label7: b -= (-17);		// b increases by 17, until c (123700) is reached. h stores how many of them are not prime!
	goto label2;
}

// Go through numbers between 106700 and 123700 with step 17, returns how many of them are not prime.
static int func2()
{
	int h = 0;
	for (int i = 106700; i <= 123700; i += 17)
	{
		for (int d = 2; d < i; ++d)
		{
			if ((i % d) == 0)
			{
				h++;
				break;
			}
		}
	}
	return h;
}

