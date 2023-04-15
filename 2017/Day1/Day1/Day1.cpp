// Day1.cpp
// AoC 2017 Day 1: Inverse Captcha
// Author: Chi-Kit Pao
//


#include <fstream>
#include <iostream>

int main()
{
	std::fstream inFile("input.txt");
	std::string str;
	inFile >> str;

	// Part 1:
	int sum1 = 0;
	for (size_t i = 1; i < str.size(); ++i) {
		if (str[i - 1] == str[i])
			sum1 += str[i] - '0';
	}
	if (str.size() > 1 && str[0] == str[str.size() - 1])
		sum1 += str[0] - '0';

	// Part 2:
	int sum2 = 0;
	size_t halfSize = str.size() / 2;
	for (size_t i = 0; i < str.size(); ++i) {
		if (str[i] == str[(i + halfSize) % str.size()])
			sum2 += str[i] - '0';
	}

	std::cout << "Day 1: " << "\n";
	std::cout << ("Question 1: What is the solution to your captcha?\n");
	std::cout << "Answer: " << sum1 << "\n";
	std::cout << ("Question 2: What is the solution to your new captcha?\n");
	std::cout << "Answer: " << sum2 << "\n";
	std::cout << std::endl;
}

