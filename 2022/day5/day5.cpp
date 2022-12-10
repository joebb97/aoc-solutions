#include <array>
#include <deque>
#include <iostream>
#include <sstream>
#include <vector>
using namespace std;

void print_stacks(const vector<deque<char>> &stacks) {
  for (auto &stack : stacks) {
    for (auto &item : stack) {
      cout << item << " ";
    }
    cout << "\n";
  }
  cout << "\n";
}

int main() {
  string line;
  getline(cin, line);
  auto item_width = 4;
  auto line_size = line.size();
  auto num_stacks = (line_size + 1) / 4;
  vector<deque<char>> stacks(num_stacks);
  while (line.size() == line_size) {
    for (auto i = 0, stack_idx = 0; i < line_size;
         i += item_width, ++stack_idx) {
      auto to_add = line[i + 1];
      if (to_add == '1') {
        break;
      }
      if (to_add != ' ') {
        stacks[stack_idx].push_front(to_add);
      }
    }
    getline(cin, line);
  }

  getline(cin, line);
  vector<array<int, 3>> moves;
  while (line.size() != 0) {
    array<int, 3> move;
    string skip;
    istringstream line_in(line);
    line_in >> skip >> move[0] >> skip >> move[1] >> skip >> move[2];
    moves.push_back(move);
    getline(cin, line);
  }

  for (const auto &move : moves) {
    auto amt = move[0];
    auto &from = stacks[move[1] - 1];
    auto &to = stacks[move[2] - 1];
    for (int i = 0; i < amt; ++i) {
        auto top = from.back();
        from.pop_back();
        to.push_back(top);
    }
  }

  for (const auto &stack: stacks) {
      cout << stack.back();
  }
}
