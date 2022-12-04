#include "bst.h"

#include <stdexcept>

Node::Node(int value) {
	val = value;
	left = nullptr;
	right = nullptr;
}

Node::~Node() {
	delete left;
	delete right;
}

Node* bst_insert(int val, Node* head) {
	if (head == nullptr) {
		return new Node(val);
	}
	
	if (val < head->val) {
		if (head->left == nullptr) {
			head->left = new Node(val);
		} else {
			bst_insert(val, head->left);
		}
	} else if (val > head->val) {
		if (head->right == nullptr) {
			head->right = new Node(val);
		} else {
			bst_insert(val, head->right);
		}
	} else {
		throw std::invalid_argument{ "Duplicates not allowed" };
	}
	
	return head;
}

bool bst_search(int val, Node* head) {
	if (head == nullptr) { return false; }

	if (val == head->val) {
		return true;
	} else if (val < head->val) {
		return bst_search(val, head->left);
	} else if (val > head->val) {
		return bst_search(val, head->right);
	}

	return false;
}

int bst_min(Node* head) {
	if (head == nullptr) { return 0; }
	
	if (!head->left) {
		return head->val;
	} else {
		return bst_min(head->left);
	}
}

Node* bst_remove(int val, Node* head) {
	if (head == nullptr) { return nullptr; }
	
	if (val < head->val) {
		head->left = bst_remove(val, head->left);
	} else if (val > head->val) {
		head->right = bst_remove(val, head->right);
	}
	
	else {
		Node* node;
		if (!head->right) { node = head->left; }
		else if (!head->left) { node = head->right; }
		else {
			// find minimum of right tree
			int new_val = bst_min(head->right);
			// remove that minimum node
			head->right = bst_remove(new_val, head->right);
			node = new Node(new_val);
			node->right = head->right;
			node->left = head->left;
		}
		free(head);	
		return node;
	}

	return head;
}

void bst_range_search(std::vector<int> *result, int lower, int upper, Node* head) {
	if (!head) { return; }
	if (lower < head->val) {
		bst_range_search(result, lower, upper, head->left);
	}

	if (lower <= head->val && upper >= head->val) {
		result->push_back(head->val);
	}

	if (upper > head->val) {
		bst_range_search(result, lower, upper, head->right);
	}
}
