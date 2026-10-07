// Day22.cpp
// AoC 2020 Day 22: Crab Combat
// Author: Chi-Kit Pao
//
// Outputs:
// Question 1: What is the winning player's score?
// Answer: 34127
// Question 2: What is the winning player's score?
// Answer : 32054
// Duration: 2.945 s

#include <algorithm>
#include <cassert>
#include <chrono>
#include <deque>
#include <fstream>
#include <iostream>
#include <set>
#include <sstream>
#include <vector>

using steady_clock = std::chrono::steady_clock;
static double ToSecond(steady_clock::time_point begin, steady_clock::time_point end)
{
	auto ms = std::chrono::duration_cast<std::chrono::milliseconds>(end - begin).count();
	return ms / 1000.0;
}

static void ParseInput(std::deque<int>* cards1, std::deque<int>* cards2)
{
	assert(cards1);
	assert(cards2);

	std::fstream inFile("input.txt");
	std::string line;
	int state = 0;
	while (std::getline(inFile, line))
	{
		if (state == 0)
		{
			assert(line == "Player 1:");
			state++;
		}
		else if (state == 1)
		{
			if (line.empty())
			{
				state++;
			}
			else
			{
				std::istringstream token_ss(line);
				int value;
				token_ss >> value;
				cards1->push_back(value);
			}
		}
		else if (state == 2)
		{
			assert(line == "Player 2:");
			state++;
		}
		else if (state == 3)
		{
			std::istringstream token_ss(line);
			int value;
			token_ss >> value;
			cards2->push_back(value);
		}
	}
}

static int GetScore(const std::deque<int>& cards)
{
	int score = 0;
	for (int i = 1; i <= cards.size(); ++i)
	{
		score += i * cards[cards.size() - i];
	}
	return score;
}

static int GetScore(const std::vector<int>& cards)
{
	int score = 0;
	// Top card at the end of vector.
	for (int i = 0; i < cards.size(); ++i)
	{
		score += (i + 1) * cards[i];
	}
	return score;
}

static int Part1(std::deque<int> cards1, std::deque<int> cards2)
{
	while (!cards1.empty() && !cards2.empty())
	{
		int card1 = cards1.front();
		cards1.pop_front();
		int card2 = cards2.front();
		cards2.pop_front();
		if (card1 == card2)
			throw std::runtime_error("No rule is defined on draw! Exit program!");
		if (card1 > card2)
		{
			cards1.push_back(card1);
			cards1.push_back(card2);
		}
		else
		{
			cards2.push_back(card2);
			cards2.push_back(card1);
		}
	}

	if (!cards1.empty())
		return GetScore(cards1);
	else
		return GetScore(cards2);
}

class Game
{
public:
	typedef std::deque<int>::const_iterator cdequeit;
	Game(std::deque<int> cards1, std::deque<int> cards2, bool needScore)
	{
		std::copy(cards1.crbegin(), cards1.crend(), std::back_inserter(m_cards1));
		std::copy(cards2.crbegin(), cards2.crend(), std::back_inserter(m_cards2));
		m_needScore = needScore;
	}
	typedef std::vector<int>::const_iterator cvectorit;
	Game(cvectorit begin1, cvectorit end1, cvectorit begin2, cvectorit end2, bool needScore)
	{
		m_cards1.assign(begin1, end1);
		m_cards2.assign(begin2, end2);
		m_needScore = needScore;
	}
	int Play()
	{
		while (!m_cards1.empty() && !m_cards2.empty())
		{
			// Check instant win. Otherwise store card string.
			std::string situation = GetSituationString(m_cards1, m_cards2);
			if (m_history.count(situation))
			{
				if(m_needScore)
					throw std::runtime_error("Instant win for play 1 while we need score! Exit program!");
				return 1;
			}
			m_history.emplace(situation);
			
			// Draw cards
			int card1 = m_cards1.back();
			m_cards1.pop_back();
			int card2 = m_cards2.back();
			m_cards2.pop_back();
			if (m_cards1.size() >= card1 && m_cards2.size() >= card2)
			{
				// Play a new game of Recursive Combat
				if (Game(m_cards1.end() - card1, m_cards1.end(), m_cards2.end() - card2, m_cards2.end(), false).Play() > 0)
				{
					m_cards1.insert(m_cards1.begin(), { card2, card1 });
					//m_cards1.insert(m_cards1.begin(), card2);
					
				}
				else
				{
					m_cards2.insert(m_cards2.begin(), { card1, card2 });
					//m_cards2.insert(m_cards2.begin(), card1);
				}
			}
			else
			{
				// Normal round
				if (card1 == card2)
					throw std::runtime_error("No rule is defined on draw! Exit program!");
				if (card1 > card2)
				{
					m_cards1.insert(m_cards1.begin(), { card2, card1 });
					//m_cards1.insert(m_cards1.begin(), card2);
				}
				else
				{
					m_cards2.insert(m_cards2.begin(), { card1, card2 });
					//m_cards2.insert(m_cards2.begin(), card1);
				}
			}
		}

		if (!m_cards1.empty())
			return m_needScore ? GetScore(m_cards1) : 1;
		else
			return m_needScore ? -GetScore(m_cards2) : -1;
	}
	static std::string GetSituationString(const std::vector<int>& cards1, const std::vector<int>& cards2)
	{
		// The previous implementation using "std::stringstream" was the biggest
		// performance bottleneck. Now use "std::string" instead.
		std::string result;

		// It doens't matter when the cards are stored backwards. :-)
		for (auto i : cards1)
			result += std::to_string(i) + ",";
		
		result += ";";
		
		for (auto i : cards2)
			result += std::to_string(i) + ",";
		
		return result;
	}
private:
	// Cards are stored in reverse order for performance. Top card is at the 
	// end of the vector.
	std::vector<int> m_cards1;
	std::vector<int> m_cards2;
	std::set<std::string> m_history;
	bool m_needScore;
};

static int Part2(const std::deque<int>& cards1, const std::deque<int>& cards2)
{;
	auto game = Game(cards1, cards2, true);
	return std::abs(game.Play());
}

int main()
{
	steady_clock::time_point begin = steady_clock::now();

	std::deque<int> cards1;
	std::deque<int> cards2;
	ParseInput(&cards1, &cards2);

	std::cout << "Day 22" << "\n";

	std::cout << "Question 1: What is the winning player's score?\n";
	int score1 = Part1(cards1, cards2);
	std::cout << "Answer: " << score1 << "\n";

	std::cout << "Question 2: What is the winning player's score?\n";
	int score2 = Part2(cards1, cards2);
	std::cout << "Answer: " << score2 << "\n";
	std::cout << "Duration: " << ToSecond(begin, steady_clock::now()) << " s\n" << std::endl;
	return 0;
}
