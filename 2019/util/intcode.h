#ifndef UTIL_INTCODE
#define UTIL_INTCODE

#include <array>
#include <string>

typedef struct {
	int operation;
	int pmode_one;
	int pmode_two;
	int pmode_three;
} Opcode;

class IntcodeInterpreter {
	std::array<int, 1000> mem;
	std::array<int, 1000> rom;
	int pc;

	int out;
	int input_addr;

public:
	IntcodeInterpreter();
	IntcodeInterpreter(std::string program);

	void set_mem(int index, int val);
	int get_mem(int index);

	void run();
	void reset();
	void load(std::string program);

	int output();
	void input(int in);

private:
	Opcode parse_opcode(std::string oc_str);
	void run_opcode(Opcode oc);
	int parameter(int pmode, int val);

	void jump_if_true(Opcode oc);
	void jump_if_false(Opcode oc);
	void less_than(Opcode oc);
	void equals(Opcode oc);
};

#endif
