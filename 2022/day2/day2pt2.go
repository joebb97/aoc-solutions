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

const winPoints = 6
const drawPoints = 3
const losePoints = 0

var outcomes = map[byte]int {
	'X': losePoints,
	'Y': drawPoints,
	'Z': winPoints,
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
		outcome := outcomes[line[2]]
		opponentInfo := gameMappings[opponent]
		losingMove := gameMappings[opponentInfo.beats]
		winningMove :=  gameMappings[losingMove.beats]
		score := outcome
		if outcome == losePoints {
			score += losingMove.score
		} else if outcome == drawPoints {
			score += opponentInfo.score
		} else {
			score += winningMove.score
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
