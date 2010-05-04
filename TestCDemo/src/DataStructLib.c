#include <stdio.h>
#include "DataStructLib.h"

LinkedList *CreateLinkedList() {
	LinkedList *pLinkedList = malloc(sizeof(LinkedList));
	pLinkedList->head = NULL;
	return pLinkedList;
}

void AddFirst(LinkedList *pLinkedList, Node *pNewNode) {
	pNewNode->next = pLinkedList->head;
	pLinkedList->head = pNewNode;
}

void AddNodeByIndex(LinkedList *pLinkedList, int index, Node *pNewNode) {
	
	if (index < 0) return;
	
	if (index == 0) {
		AddFirst(pLinkedList, pNewNode);
		return;
	}
	
	Node *node = pLinkedList->head;
	if (node == NULL) return; /* if index > 0 && head == NULL, this is a error. In this case, index should be 0 anyway while head == NULL*/
	
	/* Below head != NULL and index >= 1 */
	while (--index) {
		node = node->next;
	    if (node == NULL) return; /* index > size of linked list*/
	}
	pNewNode->next = node->next;
	node->next = pNewNode;
}

void AddLast(LinkedList *pLinkedList, Node *pNewNode) {
	Node *node = pLinkedList->head;
	if (node == NULL) {
		AddFirst(pLinkedList, pNewNode);
		return;
	}
	while (node->next != NULL) {
		node = node->next;
	}
	node->next = pNewNode;
}

void AddNode(LinkedList *pLinkedList, Node *pNewNode) {
	AddLast(pLinkedList, pNewNode);
}

/**
 * Return 1 means success to remove while 0 means fail to remove.
 */
int RemoveNode(LinkedList *pLinkedList, Node *pDeleteNode) {
	Node *node = pLinkedList->head;
	if (node == NULL) return 0;
	
	// Remove first.
	if (node == pDeleteNode) {
	    RemoveFirst(pLinkedList);
	    return 1;
	}
	
	while (node->next != NULL) {
		if (node->next == pDeleteNode) {
			if (node->next->next == NULL) {
				node->next = NULL;
			} else {
				node->next = node->next->next;
			}
			return 1;
		}
		node = node->next;
	}
	
	return 0;
}

Node *RemoveNodeByIndex(LinkedList *pLinkedList, int index) {
	if (index < 0) return NULL;
	if (index == 0) {
		return RemoveFirst(pLinkedList);
	}
	/* Below is the index > 0*/
	Node *previousNode = pLinkedList->head;
	if (previousNode == NULL) return NULL; /* NULL linked list, nothing can be removed. */
	Node *node = previousNode->next; 
	if (node == NULL) return NULL; /* One node list, index should be 0, can not be great than 0. */
	
	// if no return at this time, at least there are two nodes in the list now, remember it.
	while (--index) {
		previousNode = node;
		node = node->next;
		if (node == NULL) return NULL; /* index > size of linked list*/
	}
	
	if (node->next == NULL) {
		previousNode->next = NULL;
	} else {
		previousNode->next = node->next;
	}
	return node;
	
}

Node *RemoveFirst(LinkedList *pLinkedList) {
	Node *node = pLinkedList->head;
	if (node == NULL) return NULL;
	pLinkedList->head = node->next;
	return node;
}

Node *RemoveLast(LinkedList *pLinkedList) {
	Node *node = pLinkedList->head;
	if (node == NULL) return NULL;
	/* If there is only one node in the list.*/
	if (node->next == NULL) {
	    pLinkedList->head = NULL;
		return node;
	}
	/* More than one node in the list.*/
	Node* previousNode = node;
	node = node->next;
	while (node->next != NULL) {
		previousNode = node;
		node = node->next;
	}
	previousNode->next = NULL;
	return node;
}

int GetLinkedListSize(LinkedList *pLinkedList) {
	int i = 0;
	Node *node = pLinkedList->head;
	while (node != NULL) {
		node = node->next;
		i++;
	}
	return i;
}

/* return -1 means no found node in current list. or return index of list.*/

int IndexOf(LinkedList *pLinkedList, Node *pSearchedNode) {
	Node *node = pLinkedList->head;
	int i = 0;
	while (node != NULL) {
		if (node == pSearchedNode) return i;
		node = node->next;
		i++;
	} 
	return -1;
}

Node *GetNode(LinkedList *pLinkedList, int index) {
	if (index < 0) return NULL;
	Node *node = pLinkedList->head;
	if (node == NULL) return NULL;
	
	/* index >= 0 && node != NULL*/
	while (index--) {
		node = node->next;
		if (node == NULL) return NULL; /* index > size of linked list. */
	}
	return node;
}

Node *GetFirstNode(LinkedList *pLinkedList) {
	return pLinkedList->head;
}

Node *GetLastNode(LinkedList *pLinkedList) {
	Node *node = pLinkedList->head;
	if (node == NULL) return NULL;
	
	while (node->next != NULL) {
		node = node->next;
	}
	return node;
}

void DeleteLinkedList(LinkedList *pLinkedList) {
	free(pLinkedList);
}
