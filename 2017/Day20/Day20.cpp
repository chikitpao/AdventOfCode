// Day20.cpp
// AoC 2017 Day 20: Particle Swarm
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iostream>
#include <regex>
#include <sstream>
#include <vector>

typedef int UNIT;

class Particle
{
public:
	UNIT px;
	UNIT py;
	UNIT pz;
	UNIT vx;
	UNIT vy;
	UNIT vz;
	UNIT ax;
	UNIT ay;
	UNIT az;
	void SetP(UNIT px, UNIT py, UNIT pz)
	{
		this->px = px;
		this->py = py;
		this->pz = pz;
	}
	void SetV(UNIT vx, UNIT vy, UNIT vz)
	{
		this->vx = vx;
		this->vy = vy;
		this->vz = vz;
	}
	void SetA(UNIT ax, UNIT ay, UNIT az)
	{
		this->ax = ax;
		this->ay = ay;
		this->az = az;
	}
	void Update()
	{
		vx += ax;
		vy += ay;
		vz += az;
		px += vx;
		py += vy;
		pz += vz;
	}
	UNIT GetNormA() const
	{
		return abs(ax) + abs(ay) + abs(az);
	}
	bool HasCollision(const Particle& other)
	{
		return this->px == other.px && this->py == other.py && this->pz == other.pz;
	}
};

static std::vector<Particle> particles;

static void RunSimulation()
{
	// Not optimal. Just a guess that 1000 cycles would be enough. :-(
	for (int i = 0; i < 1000; ++i)
	{
		for (auto& particle : particles)
			particle.Update();

		// Nested loops to remove dupliates (then also remove the first occurence!).
		for (auto it = particles.begin(); it != particles.end(); )
		{
			bool hasDuplicates = false;
			for (auto it2 = it + 1; it2 != particles.end(); )
			{
				if (it->HasCollision(*it2))
				{
					it2 = particles.erase(it2);
					hasDuplicates = true;
				}
				else
				{
					++it2;
				}
			}
			if (hasDuplicates)
				it = particles.erase(it);
			else
				++it;
		}
		// i = 38 -> 574 particles left
		/*if (particles.size() == 574)
			break;*/
	}
}

int main()
{
	std::fstream inFile("input.txt");
	std::string line;
	while (std::getline(inFile, line))
	{
		// Example line:
		// p=<-3770,-455,1749>, v=<-4,-77,53>, a=<11,7,-9>
		std::regex regex1("p=<(-?\\d+),(-?\\d+),(-?\\d+)>, v=<(-?\\d+),(-?\\d+),(-?\\d+)>, a=<(-?\\d+),(-?\\d+),(-?\\d+)>");
		std::smatch match1;
		if (!std::regex_match(line, match1, regex1))
			throw std::exception("Invalid syntax for instruction");

		Particle particle;
		particle.SetP(atoi(match1[1].str().c_str()), atoi(match1[2].str().c_str()), atoi(match1[3].str().c_str()));
		particle.SetV(atoi(match1[4].str().c_str()), atoi(match1[5].str().c_str()), atoi(match1[6].str().c_str()));
		particle.SetA(atoi(match1[7].str().c_str()), atoi(match1[8].str().c_str()), atoi(match1[9].str().c_str()));
		particles.emplace_back(particle);
	}

	// Particle with the least acceleration will stay closest to position <0,0,0> in the long term.
	auto minAccerlation = std::min_element(particles.begin(), particles.end(), [](const auto& lhs, const auto& rhs)
		{
			return lhs.GetNormA() < rhs.GetNormA();
		});
	UNIT normA = minAccerlation->GetNormA();
	auto count = std::count_if(particles.begin(), particles.end(), [normA](const auto& v)
		{
			return v.GetNormA() == normA;
		});

	std::cout << "Day 20: " << "\n";
	std::cout << ("Question 1: Which particle will stay closest to position <0,0,0> in the long term?\n");
	// count == 1:  Only 1 particle with this acceleration.
	std::cout << "Acceleration count: " << count << "\n";
	std::cout << "Answer: " << std::distance(particles.begin(), minAccerlation) << std::endl;
	std::cout << ("Question 2: How many particles are left after all collisions are resolved?\n");
	RunSimulation();
	std::cout << "Answer: " << particles.size() << "\n";

	std::cout << std::endl;
	return 0;
}
