#include "days.h"
#include "util/intcode.h"

#include <iostream>

Answer day_five(std::vector<std::string> input) {
	IntcodeInterpreter* ici = new IntcodeInterpreter(input[0]);
	ici->run();
	ici->input(1);
	int p1 = ici->output();

	ici->reset();
	ici->run();
	ici->input(5);
	int p2 = ici->output();


	return Answer {
		part_one: p1,
		part_two: p2
	};
}
