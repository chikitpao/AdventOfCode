// Day24.cpp
// AoC 2017 Day 24: Electromagnetic Moat
// Author: Chi-Kit Pao
//

#include <algorithm>
#include <cassert>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

class Component
{
public:
	int value1;
	int value2;
	int sum;
	std::string name;

	// properties for search
	int used = 0; // 0 unused, 1 if value1 connected, -1 if value2 connected
	Component* predecessor = nullptr;
	std::vector<std::pair<int, Component*>> compatibleComponents;
	
	Component(int value1, int value2, const std::string& name) :
		value1(value1), value2(value2), name(name)
	{
		sum = value1 + value2;
	}

	static const int COMPATIBLE = 0x1;
	static const int LHS_INVERTED = 0x2;
	static const int RHS_INVERTED = 0x4;
	static const int BSC_SAME_POLARITY = 0x8;	// Both sides compatible, same polarity
	static const int BSC_OPPOSITE_POLARITY = 0x8; // Both sides compatible, opposite polarity
	int IsCompatible(const Component& other) const
	{
		int retval = 0;
		int tempValue = (this->value1 == other.value1)
			| ((this->value1 == other.value2) << 1)
			| ((this->value2 == other.value1) << 2)
			| ((this->value2 == other.value2) << 3);
		switch (tempValue)
		{
		case 0:
			return 0;
		case 0x1:
			return (COMPATIBLE | LHS_INVERTED);
		case 0x2:
			return (COMPATIBLE | LHS_INVERTED | RHS_INVERTED);
		case 0x4:
			return COMPATIBLE;
		case 0x8:
			return (COMPATIBLE | RHS_INVERTED);
		case 0x6:
			return (COMPATIBLE | BSC_SAME_POLARITY);
		case 0x9:
			return (COMPATIBLE | BSC_OPPOSITE_POLARITY);
		default:
			// Impossible. It means at some component has same values on both side (already filtered out).
			return 0;
		}
	}
};

static std::vector<Component> components;
static std::set<int> doubleEndComponents; // component with value1 = value2
int maxStrength = 0;
int maxLength = 0;

static void ParseInput(const std::string& fileName)
{
	std::fstream inFile(fileName);
	std::string line;

	while (std::getline(inFile, line))
	{
		auto pos = line.find('/');
		assert(pos != std::string::npos);
		int value1 = atoi(line.substr(0, pos).c_str());
		int value2 = atoi(line.substr(pos + 1).c_str());
		if (value1 == value2)
		{
			// Outputs: no duplicated "double end components", no "0/0".
			// doubleEndComponent 2/2
			// doubleEndComponent 20/20
			// doubleEndComponent 12/12
			// doubleEndComponent 5/5
			// doubleEndComponent 18/18
			// doubleEndComponent 4/4
			// doubleEndComponent 49/49
			// doubleEndComponent 29/29
			// std::cout << "doubleEndComponent " << line << std::endl;
			doubleEndComponents.insert(value1);
			continue;
		}
		components.emplace_back(value1, value2, line);
	}
	for (int i = 0; i < components.size(); ++i)
	{
		Component& lhs = components[i];
		for (int j = 0; j < components.size(); ++j)
		{
			if (i == j)
				continue;
			Component& rhs = components[j];
			int compatibility = lhs.IsCompatible(rhs);
			// REMARK: No assertion.
			assert((compatibility & (Component::BSC_SAME_POLARITY | Component::BSC_OPPOSITE_POLARITY)) == 0);
			if (compatibility)
				lhs.compatibleComponents.emplace_back(compatibility, &rhs);
		}
	}
}

static int AdditionalSum(std::vector<int> connections, int* length = nullptr)
{
	std::vector<int> connectionsCopy(connections);
	std::sort(connectionsCopy.begin(), connectionsCopy.end());
	std::vector<int> intersect;
	std::set_intersection(connectionsCopy.begin(), connectionsCopy.end(),
		doubleEndComponents.begin(), doubleEndComponents.end(),
		std::inserter(intersect, intersect.begin()));
	int sum = 0;
	if (length)
		*length = static_cast<int>(intersect.size());
	for (auto v : intersect)
		sum += v;
	return 2 * sum;
}

static void SearchStrongestBridge(Component* p, int sum, std::vector<int>& connections)
{
	for (auto& c : p->compatibleComponents)
	{
		auto* component = c.second;
		if (component->used)
			continue;
		if (c.first & Component::LHS_INVERTED)
		{
			if (p->used != -1)
				continue;
		}
		else
		{
			if (p->used != 1)
				continue;
		}

		component->predecessor = p;
		int end;
		if (c.first & Component::RHS_INVERTED)
		{
			component->used = -1;
			end = component->value1;
		}
		else
		{
			component->used = 1;
			end = component->value2;
		}
		connections.push_back(end);
		maxStrength = std::max(maxStrength, sum + component->sum + AdditionalSum(connections));
		SearchStrongestBridge(component, sum + component->sum, connections);
		connections.pop_back();
		component->used = 0;
		component->predecessor = nullptr;
	}
}

static int Part1()
{
	// Depth first search, recursive using function SearchStrongestBridge
	std::vector<int> connections;
	connections.reserve(56);
	for (auto& c : components)
	{
		int end;
		if (c.value1 == 0)
		{
			c.used = 1;
			end = c.value2;
		}
		else if (c.value2 == 0)
		{
			c.used = -1;
			end = c.value1;
		}

		if (c.used)
		{
			connections.push_back(end);
			maxStrength = std::max(maxStrength, c.sum + AdditionalSum(connections));
			SearchStrongestBridge(&c, c.sum, connections);
			connections.pop_back();
			c.used = 0;
		}
	}
	return maxStrength;
}

static void SearchLongestBridge(Component* p, int sum, int length, std::vector<int>& connections)
{
	for (auto& c : p->compatibleComponents)
	{
		auto* component = c.second;
		if (component->used)
			continue;
		if (c.first & Component::LHS_INVERTED)
		{
			if (p->used != -1)
				continue;
		}
		else
		{
			if (p->used != 1)
				continue;
		}

		component->predecessor = p;
		int end;
		if (c.first & Component::RHS_INVERTED)
		{
			component->used = -1;
			end = component->value1;
		}
		else
		{
			component->used = 1;
			end = component->value2;
		}
		connections.push_back(end);
		int additionalLength = 0;
		int currentStrength = static_cast<int>(sum + component->sum + AdditionalSum(connections, &additionalLength));
		int currentLengthTotal = 1 + additionalLength;
		if (currentLengthTotal > maxLength)
		{
			maxLength = currentLengthTotal;
			maxStrength = currentStrength;
		}
		else if (currentLengthTotal == maxLength)
		{
			if (currentStrength > maxStrength)
			{
				maxLength = currentLengthTotal;
				maxStrength = currentStrength;
			}
		}
		SearchLongestBridge(component, sum + component->sum, length + 1, connections);
		connections.pop_back();
		component->used = 0;
		component->predecessor = nullptr;
	}
}

static int Part2()
{
	// Depth first search, recursive using function SearchLongestBridge
	maxStrength = 0;

	std::vector<int> connections;
	connections.reserve(56);
	for (auto& c : components)
	{
		int end = 0;
		if (c.value1 == 0)
		{
			c.used = 1;
			end = c.value2;
		}
		else if (c.value2 == 0)
		{
			c.used = -1;
			end = c.value1;
		}

		if (c.used)
		{
			connections.push_back(end);
			int additionalLength = 0;
			int currentStrength = static_cast<int>(c.sum + AdditionalSum(connections, &additionalLength));
			int currentLengthTotal = 1 + additionalLength;
			if (currentLengthTotal > maxLength)
			{
				maxLength = currentLengthTotal;
				maxStrength = currentStrength;
			}
			else if (currentLengthTotal == maxLength)
			{
				if (currentStrength > maxStrength)
				{
					maxLength = currentLengthTotal;
					maxStrength = currentStrength;
				}
			}
			SearchLongestBridge(&c, c.sum, 1, connections);
			connections.pop_back();
			c.used = 0;
		}
	}
	return maxStrength;
}

int main()
{
	ParseInput("input.txt");

	std::cout << "Day 24: " << "\n";

	int answer1 = Part1();
	int answer2 = Part2();

	std::cout << "Question 1: What is the strength of the strongest bridge you "
		"can make with the components you have available?\n";
	std::cout << "Answer: " << answer1 << "\n";
	std::cout << "Question 2: What is the strength of the longest bridge you can make?\n";
	std::cout << "Answer: " << answer2 << "\n";

	std::cout << std::endl;
	return 0;
}
