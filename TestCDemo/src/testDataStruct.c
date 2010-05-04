#include <stdio.h>
#include "DataStructLib.h"

void printNode(Node *pNode);

void printLinkedList(LinkedList *pLinkedList);

typedef struct _Data {
		int i;
} Data;

int main(int argc, char *argv[]) {
	LinkedList *pLinkedList = CreateLinkedList();
	
	
	Node node;
	Data data;
	data.i = 1;
	node.pData = &data;
	node.next = NULL;
	AddFirst(pLinkedList, &node);
	
	Node node2;
	Data data2;
	data2.i = 2;
	node2.pData = &data2;
	node2.next = NULL;
	AddLast(pLinkedList, &node2);
	
	Node node3;
	Data data3;
	data3.i = 3;
	node3.pData = &data3;
	node3.next = NULL;
	AddNodeByIndex(pLinkedList, 2, &node3);
	
	// test get size & get node
	printLinkedList(pLinkedList);
	
	// test remove code here.
	Node *pNode = RemoveFirst(pLinkedList);
	printNode(pNode);
	
	// print again after remove first node.
	printLinkedList(pLinkedList);
}

void printNode(Node *pNode) {
	printf("node of pData is: %d\n", ((Data *)pNode->pData)->i);
	printf("node of next is: %d\n", pNode->next);
}

void printLinkedList(LinkedList *pLinkedList) {
	Node *pNode;
	int i = GetLinkedListSize(pLinkedList);
	int j = 0;
	for (j = 0; j < i; j++) {
		pNode = GetNode(pLinkedList, j);
	    printf("%d node of pData is: %d\n", j + 1, ((Data *)pNode->pData)->i);
	    printf("%d node of next is: %d\n", j + 1, pNode->next);
	}
}
