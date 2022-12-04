#include "days.h"

int fuel(int mass, int total) {
	if (mass <= 5) {
		return total;
	}
	
	mass = mass/3 - 2;
	total += mass;
	return fuel(mass, total);
}

Answer day_one(std::vector<std::string> input) {
	std::vector<std::string>::iterator it;
	
	int part_one, part_two = 0;
	for (it = input.begin(); it != input.end(); it++) {
		int mass = std::stoi(*it, nullptr, 10);
		part_one += mass/3 - 2;
		part_two += fuel(mass, 0);
	};

	return Answer {
		part_one: part_one,
		part_two: part_two
	};
}
