// Program.hpp
// AoC 2017 Day 23: Coprocessor Conflagration
// Author: Chi-Kit Pao
//

#include <deque>
#include <map>
#include <mutex>
#include <string>
#include <vector>


class Program
{
private:
	int question;

	std::map<char, long long> registers;
	std::vector<std::vector<std::string>> instructions;
	int mulCounter = 0;

public:
	//int sendCount = 0;

	Program(int question = 1);

	int GetMulCounter() const { return mulCounter; }
	long long GetRegister(char reg) const { return registers.find(reg)->second; }
	void ParseInstruction(const std::string& line);
	void ProcessInstructions();
};