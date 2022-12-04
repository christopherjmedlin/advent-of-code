#include "days.h"
#include "util/intcode.h"

Answer day_two(std::vector<std::string> input) {
	IntcodeInterpreter* ici = new IntcodeInterpreter(input[0]);
	ici->set_mem(1, 12);
	ici->set_mem(2, 2);
	ici->run();
	int part_one = ici->get_mem(0);
	ici->reset();
	
	int output = 0, noun, verb;
	bool found = false;
	for (noun = 0; noun <= 99; noun++) {
		for (verb = 0; verb <= 99; verb++) {
			ici->set_mem(1, noun);
			ici->set_mem(2, verb);
			ici->run();
			if (ici->get_mem(0) == 19690720) { found=true; break; }
			ici->reset();
		}
		if (found) { break; }
	}

	delete ici;

	return Answer {
		part_one: part_one,
		part_two: 100 * noun + verb
	};
}
