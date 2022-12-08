package main

import (
	"bufio"
	"fmt"
	"os"
	"unicode"
)

func main() {
	scanner := bufio.NewScanner(os.Stdin)
	total := 0
	numGroups := 3
	lines := make([]string, numGroups)
	lineNum := 0
	for scanner.Scan() {
		line := scanner.Text()
		lines[lineNum] = line
		lineNum++
		if lineNum != numGroups {
			continue
		}
		lineNum = 0
		charCounts := make([]map[rune]int, numGroups)
		for lineNum, line := range lines {
			charCounts[lineNum] = make(map[rune]int)
			for _, char := range line {
				charCounts[lineNum][char] = 1
			}
		}
		firstCounts := charCounts[0]
		for k := range firstCounts {
			match := true
			for i := 1; i < numGroups; i++ {
				_, ok := charCounts[i][k]
				match = match && ok
			}
			if match {
				total += priority(k)
			}
		}
	}
	fmt.Println(total)
}

func priority(in rune) int {
	const lowerOffset = 'a' - 1
	const upperOffset = 'A' - 27
	var diff rune
	if unicode.IsLower(in) {
		diff = in - lowerOffset
	} else {
		diff = in - upperOffset
	}
	return int(diff)
}
