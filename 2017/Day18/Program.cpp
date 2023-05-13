// Program.cpp
// AoC 2017 Day 18: Duet
// Author: Chi-Kit Pao
//

#include "Program.hpp"

#include <cassert>
#include <chrono>
#include <iostream>
#include <regex>

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

	if (isalpha(instruction[1][0]))
		AddRegister(instruction[1][0]);
	if (instruction.size() > 2 && isalpha(instruction[2][0]))
		AddRegister(instruction[2][0]);
	
	instructions.push_back(instruction);
}

void Program::ProcessInstructions()
{
	if(question == 2)
		registers['p'] = ID;

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
	long long lastFrequency = -1;
	while (ip >= 0 && ip < instructions.size())
	{
		const auto& instruction = instructions[ip];
		if (instruction[0] == "snd")
		{
			if (question == 1)
			{
				lastFrequency = getValue(instruction[1]);
			}
			else
			{
				std::lock_guard<std::mutex> lock(sendBufferMutex);
				sendCount++;
				sendBuffer.push_back(getValue(instruction[1]));
			}
		}
		else if (instruction[0] == "set")
		{
			setValue(instruction[1], getValue(instruction[2]));
		}
		else if (instruction[0] == "add")
		{
			setValue(instruction[1], getValue(instruction[1]) + getValue(instruction[2]));
		}
		else if (instruction[0] == "mul")
		{
			setValue(instruction[1], getValue(instruction[1]) * getValue(instruction[2]));
		}
		else if (instruction[0] == "mod")
		{
			assert(getValue(instruction[2]) > 0);
			long long modResult = getValue(instruction[1]) % getValue(instruction[2]);
			if (modResult < 0)
				modResult += getValue(instruction[2]);
			setValue(instruction[1], modResult);
		}
		else if (instruction[0] == "rcv")
		{
			if (question == 1)
			{
				if (getValue(instruction[1]))
				{
					std::cout << "Answer: Recovers frequency of last sound played: " << lastFrequency << "." << std::endl;
					return;
				}
			}
			else
			{
				bool failed = false;
				do
				{
					{
						std::lock_guard<std::mutex> lock(partner->sendBufferMutex);
						if (!partner->sendBuffer.empty())
						{
							long long value = partner->sendBuffer.front();
							partner->sendBuffer.pop_front();
							setValue(instruction[1], value);
							failed = false;
						}
						else
						{
							failed = true;
							waitingToReceive = true;
							// Deadlock?
							if (sendBuffer.empty() && partner->waitingToReceive)
								return;
						}
					}
					if(failed)
						std::this_thread::sleep_for(std::chrono::milliseconds(200));
				} while (failed);

			}
		}
		else if (instruction[0] == "jgz")
		{
			if (getValue(instruction[1]) > 0)
			{
				ip += static_cast<int>(getValue(instruction[2]));
				continue;
			}
		}
		else
		{
			throw std::exception("Invalid instruction");
		}
		ip++;
	}
}


void Program::AddRegister(char reg)
{
	if (registers.find(reg) == registers.end())
		registers.emplace(reg, 0);
}