// Day8.cpp
// AoC 2017 Day 8: I Heard You Like Registers
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <regex>
#include <map>
#include <sstream>

std::map<std::string, int> registers;
int highestValueEver = 0;
static void PrepareRegister(const std::string& registerName)
{
	if (registers.find(registerName) == registers.end())
		registers.emplace(registerName, 0);
}

static bool Eval(const std::string& reg, const std::string& op, int value)
{
	int registerValue = registers.find(reg)->second;
	if (op == ">")
		return registerValue > value;
	else if (op == "<")
		return registerValue < value;
	else if (op == ">=")
		return registerValue >= value;
	else if (op == "<=")
		return registerValue <= value;
	else if (op == "==")
		return registerValue == value;
	else if (op == "!=")
		return registerValue != value;
	throw std::exception((std::string("Unknown operator ") + op).c_str());
}

static void Execute(const std::string& reg, const std::string& op, int change)
{
	int change2 = (op == "inc") ? change : -change;
	int newValue = registers.find(reg)->second;
	newValue += change2;
	registers[reg] = newValue;

	highestValueEver = std::max(highestValueEver, newValue);
}

int main()
{
	std::fstream inFile("input.txt");

	// Parse instructions
	std::string line;
	while (std::getline(inFile, line))
	{
		// Example lines:
		// av inc 167 if f > -9
		// njb dec 861 if l <= 3
		std::regex regex1("(\\w+) (inc|dec) (-?\\d+) if (\\w+) (>|<|>=|<=|==|!=) (-?\\d+)");
		std::smatch match1;
		if (std::regex_match(line, match1, regex1))
		{
			PrepareRegister(match1[1]);
			PrepareRegister(match1[4]);
			if (Eval(match1[4].str(), match1[5].str(), atoi(match1[6].str().c_str())))
				Execute(match1[1].str(), match1[2].str(), atoi(match1[3].str().c_str()));
		}
	}
	auto maxElement = std::max_element(std::begin(registers), std::end(registers), [](const auto& lhs, const auto& rhs)
		{
			return lhs.second < rhs.second;
		});

	std::cout << "Day 8: " << "\n";
	std::cout << ("Question 1: What is the largest value in any register after"
		" completing the instructions in your puzzle input?\n");
	std::cout << "Answer: " << maxElement->second << "\n";
	std::cout << ("Question 2: What is the highest value held in any register during this process?\n");
	std::cout << "Answer: " << highestValueEver << "\n";

	std::cout << std::endl;
	return 0;
}
