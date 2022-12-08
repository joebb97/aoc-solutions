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
	lowerOffset := 'a' - 1
	upperOffset := 'A' - 27
	for scanner.Scan() {
		line := scanner.Text()
		halfWay := len(line) / 2
		firstHalf := line[:halfWay]
		secondHalf := line[halfWay:]
		firstHalfItems := map[rune]struct{}{}
		for _, char := range firstHalf {
			firstHalfItems[char] = struct{}{}
		}
		var sharedItem rune
		for _, char := range secondHalf {
			if _, ok := firstHalfItems[char]; ok {
				sharedItem = char
				break
			}
		}
		var diff rune
		if unicode.IsLower(sharedItem) {
			diff = sharedItem - lowerOffset
		} else {
			diff = sharedItem - upperOffset
		}
		total += int(diff)
	}
	fmt.Println(total)
}
