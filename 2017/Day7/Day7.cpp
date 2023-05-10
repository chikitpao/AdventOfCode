// Day7.cpp
// AoC 2017 Day 7: Recursive Circus
// Author: Chi-Kit Pao
//

#include <algorithm>
#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <vector>

class Program{
public:
	std::string name;
	int weight;
	const Program* parent = nullptr;
	std::vector<Program*> children;
	int children_weight = -1;
	Program(const char* name, int weight) : name(name), weight(weight) {}
};

// Depth-First Search of children for invalid weight
// true if children are ok, false else
static bool CheckChildren(Program* program)
{
	if (program->children.empty())
	{
		program->children_weight = 0;
		return true;
	}	

	// About to accumulate children weights
	program->children_weight = 0;

	std::vector<int> weights;
	for (auto* c : program->children)
	{
		if (!CheckChildren(c))
			return false;
		int weight = c->weight + c->children_weight;
		program->children_weight += weight;
		weights.push_back(weight);
	}
	int first_weight = weights[0];
	if (std::all_of(weights.begin() + 1, weights.end(), [first_weight](int i) { return i == first_weight; }))
		return true;

	std::cout << "## " << program->name << " has children with wrong weight!" << std::endl;
	for (size_t i = 0; i < program->children.size(); ++i)
	{
		std::cout << "## " << i << " name: " << program->children[i]->name << " weight: " << program->children[i]->weight
			<< " weight: " <<  weights[i]<< std::endl;
	}

	// Output of my program:
	// ## 0 name: ptshtrn weight: 526 weight: 1122
	// ## 1 name: mxgpbvf weight: 52 weight: 1117
	// ## 2 name: cfqdsb weight: 556 weight: 1117
	// ## 3 name: yfejqb weight: 493 weight: 1117
	
	// Solution: ptshtrn shall have weight of 521 instead of 526.
	
	return false;
}

int main()
{
	std::fstream inFile("input.txt");

	// Parse all programs data. Store all possible child parent pairs temporarily.
	std::vector<Program> programs;
	std::vector<std::pair<std::string, std::string>> parent_pairs; // child -> parent
	std::string line;
	while (std::getline(inFile, line))
	{
		std::regex program_regex("(\\w+) \\((\\d+)\\)");
		std::smatch program_match;
		if (std::regex_search(line, program_match, program_regex))
		{
			programs.emplace_back(program_match[1].str().c_str(), atoi(program_match[2].str().c_str()));
			auto pos = line.find("->");
			if (pos != std::string::npos)
			{
				pos += 2;
				
				std::stringstream ss(line.substr(pos));
				std::string word;
				while (ss >> word){
					std::string word2 = (word[word.size() - 1] == ',') ? word.substr(0, word.size() - 1) : word;
					parent_pairs.emplace_back(word2, programs.back().name);
				}
			}
		}
	}
	// Fill parent pointers
	for (const auto& v : parent_pairs)
	{
		auto child = std::find_if(programs.begin(), programs.end(), [v](const auto& entry){
			return entry.name == v.first;
			});
		auto parent = std::find_if(programs.begin(), programs.end(), [v](const auto& entry) {
			return entry.name == v.second; 
			});
		child->parent = &(*parent);
		parent->children.push_back(&(*child));
	}

	// Find program without parent
	auto bottomProgram = std::find_if(programs.begin(), programs.end(), [](const auto& entry) {
		return entry.parent == nullptr;
		});


	std::cout << "Day 7: " << "\n";
	std::cout << ("Question 1: What is the name of the bottom program?\n");
	std::cout << "Answer: " << bottomProgram->name  << "\n";
	std::cout << ("Question 2: Given that exactly one program is the wrong "
		"weight, what would its weight need to be to balance the entire "
		"tower?\n");
	CheckChildren(&(*bottomProgram));

	std::cout << std::endl;
	return 0;
}
