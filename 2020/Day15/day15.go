/*
	AoC2020, Day 15: Rambunctious Recitation
	Author: Chi-Kit Pao

	Commands (puzzle input saved as input.txt):
	cat input.txt | go run day15.go

	Output:
    Question 1: What will be the 2020th number spoken?
	Answer: 232
	Question 2: What will be the 30000000th number spoken?
	Answer: 18929178

    Time usage shown via command "time":
	real	0m1,759s
	user	0m1,742s
	sys	0m0,315s

*/


package main

import (
	"fmt"
	"strings"
	"strconv"
)

func calculate(inputString string, rounds int) (result int) {
	inputList := strings.Split(inputString, ",")
	numberMap := map[int]int{}
	lastNumber := 0
	lastDifference := 0
	for index, numberString := range inputList {
		number, _ := strconv.Atoi(numberString)

		if (index + 1) == len(inputList) {
			if numberIndex, exists := numberMap[number]; exists {
				lastDifference = (index + 1) - numberIndex;
			} else {
				lastDifference = 0;
			}
		}
		numberMap[number] = (index + 1)
		lastNumber = number
	}
	for nextIndex := len(inputList) + 1; nextIndex <= rounds; nextIndex++ {
		number := lastDifference
		if numberIndex, exists := numberMap[number]; exists {
				lastDifference = nextIndex - numberIndex
		} else {
				lastDifference = 0
		}
		numberMap[number] = nextIndex
		lastNumber = number
	}
	return lastNumber
}

func main() {
	var inputString string
	fmt.Scanf("%s", &inputString)
	fmt.Println("input:", inputString)

	fmt.Println("Question 1: What will be the 2020th number spoken?")
	fmt.Print("Anwser: ")
	fmt.Println(calculate(inputString, 2020))
	fmt.Println("Question 2: What will be the 30000000th number spoken?")
	fmt.Print("Anwser: ")
	fmt.Println(calculate(inputString, 30000000))

}