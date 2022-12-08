package main

import (
	"fmt"
	"io/ioutil"
	"os"
	"strings"
)

const offset = 'X' - 'A'

type HandShape struct {
	beats byte
	score int
}

var gameMappings = map[byte]HandShape{
	// Rock beats scissors
	'A': {'C', 1},
	// Paper beats rock
	'B': {'A', 2},
	// Scisscors beats paper
	'C': {'B', 3},
}

func main() {
	allBytes, err := ioutil.ReadAll(os.Stdin)
	must(err)

	all := string(allBytes)
	// strip trailing newline
	all = all[:len(all)-1]

	lines := strings.Split(all, "\n")
	// fmt.Println(lines, len(lines))
	total := 0
	for _, line := range lines {
		opponent := line[0]
		us := line[2] - offset
		// fmt.Printf("%q %q\n", opponent, us)
		
		losingMove, _ := gameMappings[us]
		score := losingMove.score
		if opponent == losingMove.beats {
			score += 6	
		} else if opponent == us {
			score += 3
		}
		total += score
	}
	fmt.Println(total)
}

func must(err error) {
	if err != nil {
		panic(err)
	}
}
