// Program.hpp
// AoC 2017 Day 18: Duet
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
	int ID;

	std::map<char, long long> registers;
	std::vector<std::vector<std::string>> instructions;
	Program* partner = nullptr;
	std::mutex sendBufferMutex;
	std::deque<long long> sendBuffer;
	bool waitingToReceive = false;

public:
	int sendCount = 0;

	Program(int question = 1, int ID = 0) : question(question), ID(ID) {};

	void AddPartner(Program* partner) { this->partner = partner; }
	void ParseInstruction(const std::string& line);
	void ProcessInstructions();

private:
	void AddRegister(char reg);
};