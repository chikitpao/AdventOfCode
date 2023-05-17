// Day25.cpp
// AoC 2017 Day 25: The Halting Problem
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <map>
#include <regex>
#include <set>
#include <sstream>
#include <vector>


static std::string Parse(std::fstream& inFile, std::string expr)
{
	std::string line;
	std::getline(inFile, line);
	std::regex regex1(expr);
	std::smatch match1;
	std::regex_match(line, match1, regex1);
	return match1[1].str();
}

class TuringMachine
{
public:

	class Transition
	{
	public:
		int currentValue;
		int newValue;
		int movement;
		char nextState;
		Transition() = default;
		Transition(int currentValue, int newValue, const std::string& movementStr, char nextState) :
			currentValue(currentValue), newValue(newValue), nextState(nextState)
		{
			movement = (movementStr == "right") ? 1 : -1;
		}
	};

	class State
	{
	public:
		char state;
		Transition transitions[2] = {};
		State(char state) : state(state) {}
		void ParseTransitions(std::fstream& inFile)
		{
			ParseTransition(inFile);
			ParseTransition(inFile);
		}
	private:
		void ParseTransition(std::fstream& inFile)
		{
			// Example input:
			//  If the current value is 0:
			//    - Write the value 1.
			//    - Move one slot to the right.
			//    - Continue with state B.
			auto parse = [&inFile](std::string expr)
			{
				std::string line;
				std::getline(inFile, line);
				std::regex regex1(expr);
				std::smatch match1;
				std::regex_match(line, match1, regex1);
				return match1[1].str();
			};
			int currentValue = atoi(Parse(inFile, "  If the current value is (\\d):").c_str());
			int newValue = atoi(Parse(inFile, "    - Write the value (\\d).").c_str());
			std::string movement = Parse(inFile, "    - Move one slot to the (right|left).").c_str();
			char nextState = Parse(inFile, "    - Continue with state ([A-Z]).")[0];
			transitions[currentValue] = Transition(currentValue, newValue, movement, nextState);
		}
	};

private:
	std::set<int> ones;
	int pos = 0;
public:
	char currentState = 0;
	int steps = 0;
	std::map<char, State> states;

	TuringMachine() = default;
	void AddState(State&& state)
	{
		states.emplace(state.state, state);
	}
	size_t Run()
	{
		for (int i = 0; i < steps; ++i)
		{
			auto it = states.find(currentState);
			int currentValue = (ones.find(pos) != ones.end());
			currentState = it->second.transitions[currentValue].nextState;
			int newValue = it->second.transitions[currentValue].newValue;
			if (newValue != currentValue)
			{
				if (!newValue)
					ones.erase(pos);
				else
					ones.insert(pos);
			}
			pos += it->second.transitions[currentValue].movement;
			
		}
		return ones.size();
	}

};

static TuringMachine turingMachine;

static void ParseInput(const std::string& fileName)
{
	std::fstream inFile(fileName);
	std::string line;

	// Example input:
	//Begin in state A.
	turingMachine.currentState = Parse(inFile, "Begin in state ([A-Z]).")[0];
	// Example input:
	//Perform a diagnostic checksum after 12317297 steps.
	turingMachine.steps = atoi(Parse(inFile, "Perform a diagnostic checksum after (\\d+) steps.").c_str());

	// Example input:
	//In state A:
	std::regex regex2("In state ([A-Z]):");
	std::smatch match2;
	while (std::getline(inFile, line))
	{
		if (line.size() == 0)
			continue;
		if (std::regex_match(line, match2, regex2))
		{
			TuringMachine::State state(match2[1].str()[0]);
			state.ParseTransitions(inFile);
			turingMachine.AddState(std::move(state));
		}
	}
}

int main()
{
	ParseInput("input.txt");
	

	std::cout << "Day 25: " << "\n";
	std::cout << "Question: What is the diagnostic checksum it produces once it's working again?" << std::endl;
	std::cout << "Answer: " << turingMachine.Run() << "\n";


	std::cout << std::endl;
	return 0;
}
