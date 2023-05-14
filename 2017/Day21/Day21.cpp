// Day21.cpp
// AoC 2017 Day 21: Fractal Art
// Author: Chi-Kit Pao
//

#include <cassert>
#include <fstream>
#include <iostream>
#include <regex>
#include <map>
#include <sstream>
#include <vector>

class Pattern
{
public:
	static std::map<int, int> rule2;
	static std::map<int, int> rule3;

	int size;
	std::string pattern; // size * size

	Pattern(int size) :  size(size)
	{
		pattern = std::string(size * size, ' ');
	}
	Pattern(int size, const std::string& pattern) : size(size)
	{
		this->pattern = pattern;
	}
	size_t GetOnPixelCount() const
	{
		assert(std::count(pattern.cbegin(), pattern.cend(), ' ') == 0);
		return std::count(pattern.cbegin(), pattern.cend(), '#');
	}
	Pattern Iterate()
	{
		if (((size % 2) != 0) && ((size % 3) != 0))
			throw std::runtime_error("invalid size");

		if ((size % 2) == 0)
		{
			int newSize = size * 3 / 2;
			Pattern newPattern(newSize);
			for (int i = 0; i < (size / 2); ++i)
			{
				for (int j = 0; j < (size / 2); ++j)
				{
					int currentPatternBit = 1;
					int currentPatternBits = 0;
					for (int k = 0; k < 2; ++k)
					{
						for (int l = 0; l < 2; ++l)
						{
							if (pattern[(2 * i + k) * size + (j * 2) + l] == '#')
								currentPatternBits |= currentPatternBit;
							currentPatternBit <<= 1;
						}
					}
					auto correspondingPattern = Pattern::rule2.find(currentPatternBits);
					assert(correspondingPattern != Pattern::rule2.end());
					int startRow = i * 3;
					int startColumn = j * 3;
					currentPatternBit = 1;
					for (int k = 0; k < 3; ++k)
					{
						for (int l = 0; l < 3; ++l)
						{
							newPattern.pattern[(startRow + k) * newSize + startColumn + l]
								= (correspondingPattern->second & currentPatternBit) ? '#' : '.';
							currentPatternBit <<= 1;
						}
					}
				}
			}
			return newPattern;
		}

		// (size % 3) == 0)
		int newSize = size * 4 / 3;
		Pattern newPattern(newSize);
		for (int i = 0; i < (size / 3); ++i)
		{
			for (int j = 0; j < (size / 3); ++j)
			{
				int currentPatternBit = 1;
				int currentPatternBits = 0;
				for (int k = 0; k < 3; ++k)
				{
					for (int l = 0; l < 3; ++l)
					{
						if (pattern[(3 * i + k) * size + (j * 3) + l] == '#')
							currentPatternBits |= currentPatternBit;
						currentPatternBit <<= 1;
					}
				}
				auto correspondingPattern = Pattern::rule3.find(currentPatternBits);
				assert(correspondingPattern != Pattern::rule3.end());
				int startRow = i * 4;
				int startColumn = j * 4;
				currentPatternBit = 1;
				for (int k = 0; k < 4; ++k)
				{
					for (int l = 0; l < 4; ++l)
					{
						newPattern.pattern[(startRow + k) * newSize + startColumn + l]
							= (correspondingPattern->second & currentPatternBit) ? '#' : '.';
						currentPatternBit <<= 1;
					}
				}
			}
		}
		return newPattern;
	}
};
std::map<int, int> Pattern::rule2;
std::map<int, int> Pattern::rule3;

static std::vector<int> FindModifications2(int pattern)
{
	std::vector<int> result;
	result.reserve(16);

	result.push_back(pattern);

	// rotate counter-clockwise
	auto rotate = [](int pattern) -> int
	{
		int a = ((pattern & 2) >> 1) | ((pattern & 8) >> 2);
		int b = ((pattern & 1) << 2) | ((pattern & 4) << 1);
		return a | b;
	};

	for (int i = 0; i < 3; ++i)
		result.push_back(rotate(result.back()));

	// flip vertically
	int a = pattern & 0x3;
	int b = pattern & 0xc;
	result.push_back((a << 2) | (b >> 2));

	for (int i = 0; i < 3; ++i)
		result.push_back(rotate(result.back()));

	return result;
}

static std::vector<int> FindModifications3(int pattern)
{
	std::vector<int> result;
	result.reserve(512);
	result.push_back(pattern);

	// rotate counter-clockwise
	auto rotate = [](int pattern) -> int
	{
		int a = ((pattern & 4) >> 2) | ((pattern & 32) >> 4) | ((pattern & 256) >> 6);
		int b = ((pattern & 2) << 2) | (pattern & 16) | ((pattern & 128) >> 2);
		int c = ((pattern & 1) << 6) | ((pattern & 8) << 4) | ((pattern & 64) << 2);
		return a | b | c;
	};

	for (int i = 0; i < 3; ++i)
		result.push_back(rotate(result.back()));

	// flip vertically
	int a = pattern & 0x7;
	int b = pattern & 0x38;
	int c = pattern & 0x1c0;
	result.push_back((a << 6) | b | (c >> 6));

	for (int i = 0; i < 3; ++i)
		result.push_back(rotate(result.back()));

	// flip horizontally
	a = ((pattern & 4) >> 2) | (pattern & 2) | ((pattern & 1) << 2);
	b = ((pattern & 32) >> 2) | (pattern & 16) | ((pattern & 8) << 2);
	c = ((pattern & 256) >> 2) | (pattern & 128) | ((pattern & 64) << 2);
	result.push_back(a | b | c);

	for (int i = 0; i < 3; ++i)
		result.push_back(rotate(result.back()));

	for (int i = 0; i < 3; ++i)
		result.push_back(rotate(result.back()));

	return result;
}


static void ParseInput(const std::string& fileName)
{
	auto parseRow = [](const std::string& str, int row) {
		auto size = str.size();
		int value = 0;
		for (int i = 0; i < size; ++i)
		{
			if (str[i] == '#')
				value |= (1 << i);
		}
		return value << (row * size);
	};
	
	std::fstream inFile(fileName);
	std::string line;
	while (std::getline(inFile, line))
	{
		auto pos = line.find(" => ");
		assert(pos == 5 || pos == 11);
		if (pos == 5)
		{
			std::regex regex1("([#\\.]{2})/([#\\.]{2}) => ([#\\.]{3})/([#\\.]{3})/([#\\.]{3})");
			std::smatch match1;
			if (!std::regex_match(line, match1, regex1))
				throw std::exception("Invalid syntax for instruction");
			int lhs = parseRow(match1[1], 0) | parseRow(match1[2], 1);
			int rhs = parseRow(match1[3], 0) | parseRow(match1[4], 1) | parseRow(match1[5], 2);
			std::vector<int> modifications = FindModifications2(lhs);
			for (const auto m : modifications)
				Pattern::rule2.emplace(m, rhs);
		}
		else if (pos == 11)
		{
			std::regex regex1("([#\\.]{3})/([#\\.]{3})/([#\\.]{3}) => ([#\\.]{4})/([#\\.]{4})/([#\\.]{4})/([#\\.]{4})");
			std::smatch match1;
			if (!std::regex_match(line, match1, regex1))
				throw std::exception("Invalid syntax for instruction");
			int lhs = parseRow(match1[1], 0) | parseRow(match1[2], 1) | parseRow(match1[3], 2);
			int rhs = parseRow(match1[4], 0) | parseRow(match1[5], 1) | parseRow(match1[6], 2) | parseRow(match1[7], 3);
			std::vector<int> modifications = FindModifications3(lhs);
			for(const auto m: modifications)
				Pattern::rule3.emplace(m, rhs);
		}
	}
}

int main()
{
	// Start Pattern:
	// .#.
	// ..#
	// ###
	Pattern startPattern(3, ".#...####");

	ParseInput("input.txt");

	Pattern workingPattern = startPattern;
	size_t pixelCount5 = 0;
	for (int i = 0; i < 18; ++i)
	{
		workingPattern = workingPattern.Iterate();
		if (i == 4)
			pixelCount5 = workingPattern.GetOnPixelCount();
	}

	std::cout << "Day 21: " << "\n";
	std::cout << ("Question 1: How many pixels stay on after 5 iterations?\n");
	std::cout << "Answer: " << pixelCount5 << std::endl;
	std::cout << ("Question 2: How many pixels stay on after 18 iterations?\n");
	std::cout << "Answer: " << workingPattern.GetOnPixelCount() << std::endl;

	std::cout << std::endl;
	return 0;
}
