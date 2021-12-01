#include <iostream>
#include <fstream>
#include <climits>
#include <cerrno>

int main(int argc, char* argv[]) {

  if (argc != 2) {
    std::cerr << "Usage: " << argv[0] << " <input>" << std::endl;
    return 1;
  }

  std::ifstream input;
  input.exceptions(std::ifstream::failbit | std::ifstream::badbit);

  try {
    input.open(argv[1]);
  } catch (std::ifstream::failure e) {
    std::cerr << "Could not open file: \"" << argv[1] << "\". " << e.code().message() << std::endl;
    return 1;
  }

  int history[3], current;

  int sum = 0, previous_sum, count = 0;
  for (int i = 0; i < 3; i++) {
    input >> history[i];
    sum += history[i];
  }
  int history_index = 0;
  previous_sum = sum;

  while (input >> current) {
    sum += current - history[history_index];
    history[history_index] = current;
    history_index = (history_index + 1) % 3;
    if (sum > previous_sum) count++;
    previous_sum = sum;
  }

  std::cout << count << std::endl;
  return 0;
}