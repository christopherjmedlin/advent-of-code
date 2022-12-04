#include "lines.h"
#include "bst.h"

#include <algorithm>
#include <iostream>
#include <cstdlib>

Line create_line(int x1, int y1, int x2, int y2) {
	Orientation axis;
	if (x1 == x2) { axis = vertical; }
	else if (y1 == y2) { axis = horizontal; }

	return Line {
		x1: x1,
		y1: y1,
		x2: x2,
		y2: y2,

		axis: axis
	};
}

struct Point {
	int x;
	int y;
	std::string type; 
};

bool sort_points(Point a, Point b) {
	if (a.x == b.x) {
		return a.y < b.y;
	}
	return a.x < b.x;
}

// vertical line sweep - https://www.youtube.com/watch?v=dePDHVovJlE
std::vector< std::pair<int, int> > intersections(std::vector<Line> lines) {
	std::vector< std::pair<int, int> > intersects;
	std::vector<struct Point> points;
	
	std::vector<Line>::iterator iter;
	for (iter = lines.begin(); iter < lines.end(); iter++) {
		// if vertical, only one coord is needed
		if (iter->axis == vertical) {
			points.push_back(Point {x: iter->x1, y: iter->y1, type: "vertical"});
			points.push_back(Point {x: iter->x1, y: iter->y2, type: "vertical"});
		} else if (iter->axis == horizontal) {
			points.push_back(Point {x: std::min(iter->x1, iter->x2), y: iter->y1, type: "horizontal-first"});
			points.push_back(Point {x: std::max(iter->x1, iter->x2), y: iter->y1, type: "horizontal-second"});
		}
	}
	// sort based on x coord
	std::sort(points.begin(), points.end(), sort_points);
	
	std::vector<struct Point>::iterator piter;
	Node* bst = nullptr;
	for (piter = points.begin(); piter < points.end(); piter++) {
		// left point of horizontal line
		if (piter->type.compare("horizontal-first") == 0) {
			bst = bst_insert(piter->y, bst);
		}
		// right point of horizontal line
		else if (piter->type.compare("horizontal-second") == 0) {
			bst = bst_remove(piter->y, bst);
		}
		else {
			// vertical line
			std::vector<int> y_intersection_vals;
			bst_range_search(&y_intersection_vals, 
							 piter->y,
							 (piter+1)->y,
							 bst);
			piter++;
			std::vector<int>::iterator i;
			for (i = y_intersection_vals.begin(); i < y_intersection_vals.end(); i++) {
				intersects.push_back(std::make_pair(piter->x, *i));		
			}
		}
	}

	return intersects;
}

bool point_on_line(Line line, std::pair<int, int> point) {
	if (line.axis == vertical) {
		int upper_y = std::max(line.y2, line.y1);
		int lower_y = std::min(line.y2, line.y1);
		return (point.second <= upper_y && point.second >= lower_y && point.first == line.x1);
	}
	int upper_x = std::max(line.x2, line.x1);
	int lower_x = std::min(line.x2, line.x1);
	return (point.first <= upper_x && point.first >= lower_x && point.second == line.y1);
}

int line_length(Line line) {
	if (line.axis == vertical) {
		return abs(line.y1 - line.y2);
	} else {
		return abs(line.x1 - line.x2);
	}
}
