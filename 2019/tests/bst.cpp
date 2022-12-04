#include "catch.hpp"

#include "../util/bst.h"

#include <algorithm>

TEST_CASE( "binary search trees" ) {
	
	Node* node = bst_insert(3, nullptr);
	node = bst_insert(5, node);
	node = bst_insert(1, node);
	node = bst_insert(2, node);
	node = bst_insert(90, node);
	node = bst_insert(4, node);

	SECTION( "insert should always return head" ) {
		REQUIRE( node->val == 3 );
	}
	
	SECTION( "duplicates not allowed" ) {
		REQUIRE_THROWS(node = bst_insert(3, node));
	}

	SECTION( "insert" ) {
		REQUIRE(node->right->val == 5);
		REQUIRE(node->right->left->val == 4);
		REQUIRE(node->right->right->val == 90);
	}

	SECTION( "search" ) {
		REQUIRE(bst_search(90, node));
		REQUIRE(!bst_search(50, node));
	}

	SECTION( "minimum" ) {
		REQUIRE(bst_min(node) == 1);
		REQUIRE(bst_min(node->right) == 4);
	}

	SECTION( "removal" ) {
		bst_remove(90, node);
		REQUIRE(!node->right->right);
		bst_remove(1, node);
		REQUIRE(node->left->val == 2);

		bst_insert(30, node);
		bst_insert(20, node);
		bst_insert(40, node);
		bst_insert(35, node);	
		bst_remove(30, node);
		REQUIRE(node->right->right->val == 35);
		// should have deleted the minimum node of the right tree
		REQUIRE(!node->right->right->right->left);
	}

	SECTION( "range search" ) {
		std::vector<int> result;
		bst_range_search(&result, 4, 90, node);
		
		REQUIRE(result.size() == 3);
		REQUIRE(find(result.begin(), result.end(), 4) != result.end());
		REQUIRE(find(result.begin(), result.end(), 5) != result.end());
		REQUIRE(find(result.begin(), result.end(), 90) != result.end());
		REQUIRE(find(result.begin(), result.end(), 3) == result.end());
	}

	delete node;
}
