// Program.cpp
// AoC 2017 Day 23: Coprocessor Conflagration
// Author: Chi-Kit Pao
//

#include "Program.hpp"

#include <cassert>
#include <chrono>
#include <iostream>
#include <regex>

Program::Program(int question): question(question)
{
	for(char c = 'a'; c <= 'h'; ++c)
		registers.emplace(c, 0);
	if (question == 2)
		registers.find('a')->second = 1;
};

void Program::ParseInstruction(const std::string& line)
{
	std::regex regex1("(\\w+) (-?\\w+)( (-?\\w+))?");
	std::smatch match1;
	if (!std::regex_match(line, match1, regex1))
		throw std::exception("Invalid syntax for instruction");

	std::vector<std::string> instruction;
	for (size_t i = 1; i < match1.size(); ++i)
	{
		if (i == 3)
			continue;
		if (match1[i].matched)
			instruction.push_back(match1[i].str());
	}
	
	instructions.push_back(instruction);
}

void Program::ProcessInstructions()
{
	auto getValue = [this](const std::string& arg) -> long long
	{
		if (isalpha(arg[0]))
			return this->registers[arg[0]];
		return atoll(arg.c_str());
	};
	auto setValue = [this](const std::string& arg, long long value)
	{
		assert(isalpha(arg[0]));
		this->registers[arg[0]] = value;
	};

	int ip = 0;
	while (ip >= 0 && ip < instructions.size())
	{
		const auto& instruction = instructions[ip];
		if (instruction[0] == "set")
		{
			setValue(instruction[1], getValue(instruction[2]));
		}
		else if (instruction[0] == "sub")
		{
			setValue(instruction[1], getValue(instruction[1]) - getValue(instruction[2]));
		}
		else if (instruction[0] == "mul")
		{
			mulCounter++;
			setValue(instruction[1], getValue(instruction[1]) * getValue(instruction[2]));
		}
		else if (instruction[0] == "jnz")
		{
			if (getValue(instruction[1]) != 0)
			{
				ip += static_cast<int>(getValue(instruction[2]));
				continue;
			}
		}
		else
		{
			throw std::runtime_error("Invalid instruction");
		}
		ip++;
	}
}
