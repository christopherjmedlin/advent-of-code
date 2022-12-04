#include <fstream>
#include <iostream>
#include <string>
#include <vector>

#include <stdlib.h>

#include "days.h"

static const std::string RED = "\e[31m";
static const std::string GREEN = "\e[32m";
static const std::string WHITE = "\e[97m";
static const std::string BOLD = "\u001b[1m";
static const std::string UNBOLD = "\033[0m";

std::vector<std::string> load_input(int day) {
	std::string input_path = "inputs/day" + std::to_string(day) + ".txt";
	std::ifstream file;

	std::string line;
	std::vector<std::string> lines;
	file.open(input_path, std::ifstream::in);

	if (!file.is_open()) { throw "Input file does not exist."; }

	while (!file.eof()) {
		getline(file, line);
		lines.push_back(line);
	}
	lines.pop_back();

	file.close();
	return lines;
}

void run_day(int day) {
	Answer result;
	std::vector<std::string> input = load_input(day);
	
	std::cout << GREEN << "Day " << day << std::endl;

	std::cout << "---------" << WHITE << std::endl;

	switch (day) {
		case 1: result = day_one(input);
			break;
		case 2: result = day_two(input);
			break;
		case 3: result = day_three(input);
			break;
		case 4: result = day_four(input);
			break;
		case 5: result = day_five(input);
			break;
		default: result = Answer {part_one: 0, part_two: 0};
	}
	
	if (result.part_one) {
		std::cout << "Part 1: " << result.part_one << std::endl;
	} else {
		std::cout << "Part 1: " << BOLD + RED << "unsolved" << UNBOLD + WHITE << std::endl;
	}

	if (result.part_two) {
		std::cout << "Part 2: " << result.part_two << std::endl;
	} else {
		std::cout << "Part 2: " << BOLD + RED << "unsolved" << UNBOLD + WHITE << std::endl;
	}
}

void run_all_days() {
	for (int i = 1; i <= 25; i++) {
		try {
			run_day(i); 
			std::cout << std::endl;
		} catch (const char* msg) {}
	}
}

int main(int argc, char* argv[]) {
	if (argc < 2) {
		std::cout << "Error: Day number required\n";
		return 1;
	}

	if (std::string(argv[1]) == "--all") {
		std::cout << std::endl;
		run_all_days();
	} else {
		int day = atoi(argv[1]);
		
		try {
			run_day(day);
		} catch (const char* msg) {
			std::cerr << BOLD + RED << msg << UNBOLD + WHITE << '\n' << std::endl;
		}
	}
	return 0;
}
