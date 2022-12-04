#include "catch.hpp" 
#include "../util/lines.h"
#include <iostream>

TEST_CASE( "lines" ) {
	
	SECTION( "line creation" ) {
		Line line = create_line(50, 60, 25, 60);
		REQUIRE(line.axis == horizontal);
		line = create_line(25, 60, 50, 60);
		REQUIRE(line.axis == horizontal);
		line = create_line(25, 60, 25, 40);
		REQUIRE(line.axis == vertical);
	};

	SECTION( "intersections" ) {
		std::vector<Line> lines;
		lines.push_back(create_line(-4, 0, -4, 4));
		lines.push_back(create_line(-3, 2, -2, 2));
		lines.push_back(create_line(0, 2, 5, 2));
		lines.push_back(create_line(1, 1, 3, 1));
		lines.push_back(create_line(2, -2, 2, 3));
		lines.push_back(create_line(4, 2, 4, 6));
		lines.push_back(create_line(-6, -1, -2, -1));
		lines.push_back(create_line(-5, -5, -5, 7));
		lines.push_back(create_line(-8, 3, -3, 3));

		std::vector< std::pair<int, int> > points = intersections(lines);
		REQUIRE(points.size() == 6);
		REQUIRE(points[0] == std::make_pair(-5, -1));
		REQUIRE(points[1] == std::make_pair(-5, 3));
		REQUIRE(points[2] == std::make_pair(-4, 3));
		REQUIRE(points[3] == std::make_pair(2, 1));
		REQUIRE(points[4] == std::make_pair(2, 2));
		REQUIRE(points[5] == std::make_pair(4, 2));
	};

	SECTION( "point on line" ) {
		Line line = create_line(1, 3, 10, 3);
		REQUIRE(point_on_line(line, std::make_pair(2, 3)));
		REQUIRE(point_on_line(line, std::make_pair(10, 3)));
		REQUIRE(!point_on_line(line, std::make_pair(11, 3)));
		REQUIRE(!point_on_line(line, std::make_pair(2, 4)));
	};

	SECTION( "line length" ) {
		Line line = create_line(1, 3, 20, 3);
		REQUIRE(line_length(line) == 19);
		line = create_line(1, 3, 1, 20);
		REQUIRE(line_length(line) == 17);
	};
};
