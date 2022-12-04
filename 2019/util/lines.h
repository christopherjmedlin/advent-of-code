#ifndef UTIL_LINES
#define UTIL_LINES

#include <vector>
#include <utility>

enum Orientation { vertical, horizontal };

struct Line {
	int x1;
	int y1;
	int x2;
	int y2;
	Orientation axis;
};

Line create_line(int x1, int y1, int x2, int y2);
// finds intersections in a vector of vertical and horizontal lines
std::vector< std::pair<int, int> > intersections(std::vector<Line> lines);
bool point_on_line(Line line, std::pair<int, int> point);
int line_length(Line line);

#endif
