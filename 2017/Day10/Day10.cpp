// Day10.cpp
// AoC 2017 Day 10: Knot Hash
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iomanip>
#include <iostream>
#include <optional>
#include <sstream>
#include <vector>

template <typename T>
class CircularBuffer
{
private:
    size_t capacity = 0;
    std::vector<T> data;
    size_t start = 0;
    size_t end = 0; // exclusive
    bool full = false;
public:
    CircularBuffer(size_t capacity)
    {
        this->capacity = capacity;
        this->data = std::move(std::vector<T>(capacity));
    }
    std::optional<T> get(size_t index) const
    {
        if (this->isEmpty())
            return std::nullopt;
        if (index >= this->size())
            return std::nullopt;
        //if (!this->data[(this->start + index) % this->capacity])
        //    throw new std::exception("get returns invalid data!");
        return this->data[(this->start + index) % this->capacity];
    }
    T get_real(size_t index) const
    {
        return this->data[index];
    }
    void set(size_t index, T value)
    {
        if (this->isEmpty())
            throw new std::exception("set while buffer is empty!");
        if (index >= this->size())
            throw new std::exception("set behind buffer end!");
        this->data[(this->start + index) % this->capacity] = value;
    }
    std::optional<T> pop()
    {
        if (this->isEmpty())
            return std::nullopt;
        T result = this->data[this->start];
        this->start++;
        this->start %= this->capacity;
        this->full = false;
        return result;
    }
    void push(T value)
    {
        if (this->full)
            throw new std::exception("Buffer is full");

        this->data[this->end] = value;
        this->end++;
        this->end %= this->capacity;
        if (this->end == this->start)
            this->full = true;
    }
    size_t size() const
    {
        if (this->full)
            return this->capacity;
        if (this->start <= this->end)
            return this->end - this->start;
        return this->end + this->capacity - this->start;
    }
    bool isEmpty() const
    {
        return !this->full && (this->start == this->end);
    }
    bool isFull() const
    {
        return this->full;
    }
    void rebase(size_t offset)
    {
        if (!this->full)
            throw new std::exception("rebase is only allowed for full buffer");
        this->start = (this->start + offset) % this->capacity;
        this->end = this->start;
    }
};

static void Reverse(CircularBuffer<int>& nodeList, int length, size_t skipSize)
{
    for (int i = 0; i < length / 2; ++i)
    {
        int temp = *nodeList.get(i);
        nodeList.set(i, *nodeList.get(length - 1 - i));
        nodeList.set(length - 1 - i, temp);
    }
    nodeList.rebase(length + skipSize);
}

static int GetAnswerPart1()
{
    std::fstream inFile("input.txt");

    std::string line;
    if (!std::getline(inFile, line))
        return 1;

    std::vector<int> lengthList;
    do
    {
        auto pos = line.find(',');
        if (pos != std::string::npos)
        {
            lengthList.push_back(atoi(line.substr(0, pos + 1).c_str()));
            line = line.substr(pos + 1);
        }
        else
        {
            lengthList.push_back(atoi(line.c_str()));
            line = "";
        }
    } while (line.size());

    const size_t nodeLength = 256;
    CircularBuffer<int> nodeList(nodeLength);
    for (int i = 0; i < nodeLength; ++i)
        nodeList.push(i);

    size_t skipSize = 0;
    for (auto length : lengthList)
    {
        Reverse(nodeList, length, skipSize);
        skipSize++;
    }
    return nodeList.get_real(0) * nodeList.get_real(1);
}

static std::string GetAnswerPart2()
{
    std::fstream inFile("input.txt");

    std::string line;
    if (!std::getline(inFile, line))
        return "Error!"; // ;-)

    std::vector<int> lengthList;
    for (char c : line)
        lengthList.push_back(static_cast<int>(static_cast<unsigned char>(c)));
    lengthList.push_back(17);
    lengthList.push_back(31);
    lengthList.push_back(73);
    lengthList.push_back(47);
    lengthList.push_back(23);

    const size_t nodeLength = 256;
    CircularBuffer<int> nodeList(nodeLength);
    for (int i = 0; i < nodeLength; ++i)
        nodeList.push(i);

    size_t skipSize = 0;
    for (int round = 0; round < 64; ++round)
    {
        for (auto length : lengthList)
        {
            Reverse(nodeList, length, skipSize);
            skipSize++;
        }
    }

    std::stringstream strStream;
    for (int i = 0; 16 * i < nodeLength; ++i)
    {
        int start = i * 16;
        int hashPart = 0;
        for (int j = 0; j < 16; ++j)
        {
            hashPart = hashPart ^ nodeList.get_real(start + j);
        }
        strStream << std::setfill('0') << std::setw(2) << std::hex << hashPart;
    }
    return strStream.str();
}

int main()
{
    std::cout << "Day 10: " << "\n";
	std::cout << ("Question 1: What is the result of multiplying the first two numbers in the list?\n");
    int answer1 = GetAnswerPart1();
	std::cout << "Answer: " << answer1 << "\n";
    std::cout << ("Question 2: Treating your puzzle input as a string of ASCII characters, what is the Knot Hash of your puzzle input?\n");
    std::string answer2 = GetAnswerPart2();
    std::cout << "Answer: " << answer2 << "\n";

	std::cout << std::endl;
	return 0;
}
