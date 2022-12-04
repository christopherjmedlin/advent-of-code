#ifndef UTIL_BST
#define UTIL_BST

#include <vector>

class Node {
public:
	int val;
	Node* left;
	Node* right;

	Node(int value);
	~Node();
};

Node* bst_insert(int val, Node* head);
int bst_min(Node* head);
Node* bst_remove(int val, Node* head);
bool bst_search(int val, Node* head);
void bst_range_search(std::vector<int> *result, int lower, int upper, Node* head);

#endif
