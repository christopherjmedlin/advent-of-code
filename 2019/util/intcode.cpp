#include "intcode.h"

#include <sstream>
#include <iomanip>
#include <iostream>

IntcodeInterpreter::IntcodeInterpreter() { pc = 0; input_addr = 0; out = 0; }
IntcodeInterpreter::IntcodeInterpreter(std::string program) {
	pc = 0; input_addr = 0; out = 0;
	
	load(program);
}

void IntcodeInterpreter::set_mem(int index, int val) { mem[index] = val; }
int IntcodeInterpreter::get_mem(int index) { return mem[index]; }

void IntcodeInterpreter::run() {
	while (mem[pc] != 99 && !input_addr) {
		Opcode oc = parse_opcode(std::to_string(mem[pc]));
		if (oc.pmode_three) { throw "Write with immediate paramater"; }
		run_opcode(oc);
		if (pc >= 1000) { throw "Unhalted program."; }
	}
}

void IntcodeInterpreter::reset() { mem = rom; pc = 0; }

void IntcodeInterpreter::load(std::string program) {
	rom.fill(0);
	std::string num;
	std::istringstream iss { program };
	int count = 0;
	while (getline(iss, num, ',')) {
		rom[count] = std::stoi(num, nullptr, 10);
		count++;
	}
	mem = rom;
}

int IntcodeInterpreter::output() { return out; }
void IntcodeInterpreter::input(int in) { 
	mem[input_addr] = in; 
	input_addr = 0; 
	run();
}

Opcode IntcodeInterpreter::parse_opcode(std::string oc_str) {
	Opcode oc;
	
	if (oc_str.length() > 5) { throw "Opcode too long."; }
	// fill in leading zeros
	while (oc_str.length() < 5) {
		oc_str = "0" + oc_str;
	}

	oc.operation = oc_str.at(4) - 48;
	oc.pmode_one = oc_str.at(2) - 48;
	oc.pmode_two = oc_str.at(1) - 48;
	oc.pmode_three = oc_str.at(0) - 48;
	return oc;
}

void IntcodeInterpreter::run_opcode(Opcode oc) {
	switch (oc.operation) {
		case 1:
			mem[mem[pc + 3]] = parameter(oc.pmode_one, mem[pc + 1]) + 
							   parameter(oc.pmode_two, mem[pc + 2]);
			pc += 4;
			break; 
		case 2:
			mem[mem[pc + 3]] = parameter(oc.pmode_one, mem[pc + 1]) *
							   parameter(oc.pmode_two, mem[pc + 2]);
			pc += 4;
			break;
		case 3:
			input_addr = mem[pc + 1];
			pc += 2;
			break;
		case 4:
			out = parameter(oc.pmode_one, mem[pc + 1]);
			pc += 2;
			break;
		case 5:
			jump_if_true(oc);
			break;
		case 6:
			jump_if_false(oc);
			break;
		case 7:
			less_than(oc);
			break;
		case 8:
			equals(oc);
			break;
	}
}

void IntcodeInterpreter::jump_if_true(Opcode oc) {
	if (parameter(oc.pmode_one, mem[pc + 1])) {
		pc = parameter(oc.pmode_two, mem[pc + 2]);
	} else {
		pc += 3;
	}
}

void IntcodeInterpreter::jump_if_false(Opcode oc) {
	if (!parameter(oc.pmode_one, mem[pc + 1])) {
		pc = parameter(oc.pmode_two, mem[pc + 2]);
	} else {
		pc += 3;
	}
}

void IntcodeInterpreter::less_than(Opcode oc) {
	int p1 = parameter(oc.pmode_one, mem[pc + 1]);
	int p2 = parameter(oc.pmode_two, mem[pc + 2]);
	if (p1 < p2) {
		mem[mem[pc+3]] = 1;
	} else {
		mem[mem[pc+3]] = 0;
	}
	pc += 4;
}

void IntcodeInterpreter::equals(Opcode oc) {
	int p1 = parameter(oc.pmode_one, mem[pc + 1]);
	int p2 = parameter(oc.pmode_two, mem[pc + 2]);
	if (p1 == p2) {
		mem[mem[pc+3]] = 1;
	} else {
		mem[mem[pc+3]] = 0;
	}
	pc += 4;
}

int IntcodeInterpreter::parameter(int pmode, int val) {
	if (pmode) {
		return val;
	} else {
		return mem[val];
	}
}
