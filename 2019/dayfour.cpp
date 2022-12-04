#include "days.h"
#include <unordered_map>

int repetition_length(std::string str, int index) {
	std::string::iterator iter = str.begin() + index;
	char digit = *iter;
	int length;
	for (iter; *iter == digit && iter < str.end(); iter++) {
		length += 1;
	}
	return length;
}

bool is_valid_password(std::string passwd, bool only_doubles) {
	std::string::iterator iter;
	std::unordered_map<int, int> digit_map;
	bool valid;
	
	for (iter = passwd.begin(); iter < passwd.end(); iter++) {
		if (iter != passwd.begin() && *iter < *(iter-1)) {
		 	return false;
		}
		int repetition = repetition_length(passwd, iter-passwd.begin());
		iter += repetition-1;
		if ((!only_doubles && repetition >= 2) || repetition == 2) {
			valid = true;
		}
	}
	return valid;
}

Answer day_four(std::vector<std::string> input) {
	int part_one = 0;
	int part_two = 0;
	int lower = stoi(input[0], nullptr, 10);
	int upper = stoi(input[1], nullptr, 10);
	for (int i = lower; i <= upper; i++) {
		if (is_valid_password(std::to_string(i), false)) { part_one++; }
		if (is_valid_password(std::to_string(i), true)) { part_two++; }
	}

	return Answer {
		part_one: part_one,
		part_two: part_two
	};
}
