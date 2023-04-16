// Day4.cpp
// AoC 2017 Day 4: High-Entropy Passphrases
// Author: Chi-Kit Pao
//

#include <algorithm>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <string>
#include <vector>

// Returns true if passphrase is valid, false else.
static bool EvaluateRowPart1(std::vector<std::string>& values)
{
	if (values.size() <= 1)
		return true;
	std::set<std::string> found;
	for (const auto& value : values)
	{
		if (found.find(value) != found.end())
			return false;
		found.insert(value);
	}
	return true;
}

std::string GetAnagram(const std::string& value)
{
	std::vector<char> temp;
	for (auto ch : value)
		temp.push_back(ch);
	std::sort(temp.begin(), temp.end());
	std::stringstream strstream;
	for (auto ch : temp)
		strstream << ch;
	return strstream.str();
}


// Returns true if passphrase is valid, false else.
static bool EvaluateRowPart2(std::vector<std::string>& values)
{
	if (values.size() <= 1)
		return true;
	std::set<std::string> found;
	for (const auto& value : values)
	{
		std::string anagram = GetAnagram(value);
		if (found.find(anagram) != found.end())
			return false;
		found.insert(anagram);
	}
	return true;
}

int main()
{
	std::fstream inFile("input.txt");
	std::string line;
	const char delimiter = ' ';
	int answer1 = 0;
	int answer2 = 0;
	while (std::getline(inFile, line))
	{
		std::string token;
		std::istringstream iss(line);
		std::vector<std::string> values;
		while (std::getline(iss, token, delimiter))
		{
			if (token.empty())
				continue;
			std::istringstream token_ss(token);
			std::string value;
			token_ss >> value;
			values.push_back(value);
		}
		answer1 += EvaluateRowPart1(values);
		answer2 += EvaluateRowPart2(values);
	}

	std::cout << "Day 4: " << "\n";
	std::cout << ("Question 1: The system's full passphrase list is available as your puzzle input. How many passphrases are valid?\n");
	std::cout << "Answer: " << answer1 << "\n";
	std::cout << ("Question 2: How many passphrases are valid under the new system policy?\n");
	std::cout << "Answer: " << answer2 << "\n";
	std::cout << std::endl;
}
