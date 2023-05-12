// Day14.cpp
// AoC 2017 Day 14: Disk Defragmentation
// Author: Chi-Kit Pao
//

#include <fstream>
#include <iomanip>
#include <iostream>
#include <map>
#include <set>
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

static std::string GetKnotHash(const std::string& str)
{
    std::vector<int> lengthList;
    for (char c : str)
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

static std::set<int> used;
static std::map<char, std::vector<int>> hexMap;
void InitializeHexMap()
{
    for (int i = 0; i < 16; ++i)
    {
        char startChar = (i < 10) ? '0' : ('a' - 10);
        std::vector<int> temp;
        temp.push_back(!!(i & 0x8));
        temp.push_back(!!(i & 0x4));
        temp.push_back(!!(i & 0x2));
        temp.push_back(!!(i & 0x1));
        temp.push_back(static_cast<int>(std::count(temp.cbegin(), temp.cend(), 1)));
        hexMap.emplace(startChar + i, std::move(temp));
    }   
}

static void DetermineRegions(int current, const std::set<int>& used, std::vector<int>& remaining, std::vector<std::set<int>>& regions)
{
    std::set<int> connected;
    std::vector<int> workingSet;
    std::set<int> newWorkingSet;
    connected.insert(current);
    workingSet.push_back(current);
    bool foundNewConnection = false;
    do
    {
        foundNewConnection = false;
        for (auto c : workingSet)
        {
            int row = c / 128;
            int column = c % 128;
            std::vector<int> connections;
            if (row > 0)
                connections.push_back((row - 1) * 128 + column);
            if (row < 127)
                connections.push_back((row + 1) * 128 + column);
            if (column > 0)
                connections.push_back(row * 128 + column - 1);
            if (column < 127)
                connections.push_back(row * 128 + column + 1);      
            for (auto rhs : connections)
            {
                if (used.find(rhs) == used.end())
                    continue;
                if (connected.find(rhs) == connected.end())
                {
                    foundNewConnection = true;
                    connected.insert(rhs);
                    newWorkingSet.insert(rhs);
                    remaining.erase(std::remove_if(remaining.begin(), remaining.end(), [rhs](auto v)
                        {
                            return v == rhs;
                        }), remaining.end());
                }
            }
        }
        workingSet.clear();
        for (auto entry : newWorkingSet)
            workingSet.push_back(entry);
        newWorkingSet.clear();
    } while (foundNewConnection);
    regions.emplace_back(connected);
}

int main()
{
    InitializeHexMap();

    const std::string puzzleInput = "nbysizxe";
    int count = 0;
    for (int i = 0; i < 128; ++i)
    {
        std::stringstream strStream;
        strStream << puzzleInput << "-" << i;
        std::string hash = GetKnotHash(strStream.str());
        int j = 0;
        for (auto c : hash)
        {
            const auto& temp = hexMap[c];
            count += temp[4];   // bit count
            for (int k = 0; k < 4; ++k)
            {
                if (temp[k])
                    used.insert(i * 128 + j + k);
            }
            j += 4;
        }
    }

    std::vector<int> remaining;
    std::copy(used.begin(), used.end(), std::back_inserter(remaining));
    std::vector<std::set<int>> regions;
    do
    {
        int current = remaining[0];
        remaining.erase(remaining.begin());
        DetermineRegions(current, used, remaining, regions);
    } while (remaining.size());

    std::cout << "Day 14: " << "\n";
    std::cout << ("Question 1: Given your actual key string, how many squares are used?\n");
    std::cout << "Answer: " << count << "\n";
    std::cout << ("Question 2: How many regions are present given your key string?\n");
    std::cout << "Answer: " << regions.size() << "\n";

    std::cout << std::endl;
    return 0;
}
