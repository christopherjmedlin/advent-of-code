#include "days.h"
#include "util/lines.h"

#include <algorithm>
#include <sstream>
#include <utility>
#include <cstdlib>

class Wire {
	std::vector<Line> vertical_lines;
	std::vector<Line> horizontal_lines;
	
public:
	using iterator = std::vector<Line>::iterator;
	Wire(std::string input);

	iterator vertical_begin() { return vertical_lines.begin(); }
	iterator vertical_end() { return vertical_lines.end(); }
	iterator horizontal_begin() { return horizontal_lines.begin(); }
	iterator horizontal_end() { return horizontal_lines.end(); }

	std::vector< std::pair<int, int> > get_intersections(Wire w);
	int steps(std::pair<int, int> point);
};

Wire::Wire(std::string dirs) {
	int curr_x, curr_y = 0;
	
	std::string instruction;
	std::istringstream iss { dirs };
	while (getline(iss, instruction, ',')) {
		std::string dist_str = instruction.substr(1, instruction.size() - 1);
		int dist = stoi(dist_str, nullptr, 10);

		switch (instruction.at(0)) {
			case 'R':
				horizontal_lines.push_back(
					create_line(curr_x, curr_y, curr_x+dist, curr_y)
				);
				curr_x+=dist;
				break;
			case 'L':
				horizontal_lines.push_back(
					create_line(curr_x, curr_y, curr_x-dist, curr_y)
				);
				curr_x-=dist;
				break;
			case 'U':
				vertical_lines.push_back(
					create_line(curr_x, curr_y, curr_x, curr_y+dist)
				);
				curr_y+=dist;
				break;
			case 'D':
				vertical_lines.push_back(
					create_line(curr_x, curr_y, curr_x, curr_y-dist)
				);
				curr_y-=dist;
				break;
		}
	}
}

std::vector< std::pair<int, int> > Wire::get_intersections(Wire w) {
	std::vector< std::pair<int, int> > result;

	// compare vertical to horizontal
	std::vector<Line> lines(vertical_lines);
	lines.insert(lines.end(), w.horizontal_begin(), w.horizontal_end());
	result = /* util/lines.h */intersections(lines);
	
	std::vector< std::pair<int, int> > more_results;

	// vise versa
	lines = horizontal_lines;
	lines.insert(lines.end(), w.vertical_begin(), w.vertical_end());
	more_results = /* util/lines.h */intersections(lines);

	result.insert(result.end(), more_results.begin(), more_results.end());
	return result;
}

int closest_dist(std::vector< std::pair<int, int> > points) {
	int closest = 10000;
	std::vector< std::pair<int, int> >::iterator iter;
	for (iter = points.begin(); iter < points.end(); iter++) {
		int dist = abs(iter->first) + abs(iter->second);
		if (dist < closest) {
			closest = dist;
		}		
	}
	return closest;
}

Answer day_three(std::vector<std::string> input) {
	Wire w1(input[0]);
	Wire w2(input[1]);
	std::vector< std::pair<int, int> > result = w1.get_intersections(w2);
	std::remove(result.begin(), result.end(), std::make_pair(0, 0));

	return Answer {
		part_one: closest_dist(result),
		part_two: 0
	};
}
