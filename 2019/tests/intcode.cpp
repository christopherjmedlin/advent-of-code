#include "catch.hpp"
#include "../util/intcode.h"

TEST_CASE( "intcode" ) {
	IntcodeInterpreter* ici = new IntcodeInterpreter;

	SECTION( "load program" ) {
		ici->load("1,2,3,4,5,6,7");
		REQUIRE(ici->get_mem(3) == 4);
		REQUIRE(ici->get_mem(6) == 7);
		ici->load("1002,4,3,2,1");
		ici->reset();
		REQUIRE(ici->get_mem(0) == 1002);
		REQUIRE(ici->get_mem(2) == 3);
	}
	
	SECTION( "small programs" ) {
		ici->load("1,0,0,0,99");
		ici->run();
		REQUIRE(ici->get_mem(0) == 2);
		ici->load("2,3,0,3,99");
		ici->reset();
		ici->run();
		REQUIRE(ici->get_mem(3) == 6);
		ici->load("2,4,4,5,99,0");
		ici->reset();
		ici->run();
		REQUIRE(ici->get_mem(5) == 9801);
	}

	SECTION( "parameter modes" ) {
		ici->load("1002,4,3,4,33");
		ici->run();
		REQUIRE(ici->get_mem(4) == 99);
		ici->load("1101,100,-1,4,0");
		ici->reset();
		ici->run();
		REQUIRE(ici->get_mem(4) == 99);
		// can't have write operations with immediate PM
		ici->load("11002,4,3,4,33");
		ici->reset();
		REQUIRE_THROWS(ici->run());
	}

	SECTION ( "io" ) {
		ici->load("3,5,99,0,0,0");
		ici->run();
		ici->input(100);
		REQUIRE(ici->get_mem(5) == 100);
		ici->load("4,5,99,0,0,50");
		ici->reset();
		ici->run();
		REQUIRE(ici->output() == 50);
	}

	SECTION( "jump" ) {
		ici->load("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9");
		ici->run();
		ici->input(100);
		ici->reset();
		ici->run();
		ici->input(0);
		REQUIRE(!ici->output());
	}

	SECTION( "conditionals" ) {
		ici->load("3,9,8,9,10,9,4,9,99,-1,8");
		ici->run();
		ici->input(8);
		REQUIRE(ici->output() == 1);
		ici->reset();
		ici->run();
		ici->input(7);
		REQUIRE(!ici->output());

		ici->load("3,3,1107,-1,8,3,4,3,99");
		ici->reset();
		ici->run();
		ici->input(7);
		REQUIRE(ici->output() == 1);
	}
}
