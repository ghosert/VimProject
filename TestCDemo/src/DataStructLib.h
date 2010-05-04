typedef struct _Node {
	struct _Node *next;
	void *pData;
} Node, *PNode;

typedef struct _LinkedList {
	Node *head;
} LinkedList, *PLinkedList;

LinkedList *CreateLinkedList();

void AddFirst(LinkedList *pLinkedList, Node *pNewNode);

void AddNodeByIndex(LinkedList *pLinkedList, int index, Node *pNewNode);

void AddLast(LinkedList *pLinkedList, Node *pNewNode);

void AddNode(LinkedList *pLinkedList, Node *pNewNode);

/* Return 1 means success to remove while 0 means fail to remove. */
int RemoveNode(LinkedList *pLinkedList, Node *pDeleteNode);

Node *RemoveNodeByIndex(LinkedList *pLinkedList, int index);

Node *RemoveFirst(LinkedList *pLinkedList);

Node *RemoveLast(LinkedList *pLinkedList);

int GetLinkedListSize(LinkedList *pLinkedList);

/* return -1 means no found node in current list. or return index of list. */
int IndexOf(LinkedList *pLinkedList, Node *pSearchedNode);

Node *GetNode(LinkedList *pLinkedList, int index);

Node *GetFirstNode(LinkedList *pLinkedList);

Node *GetLastNode(LinkedList *pLinkedList);

void DeleteLinkedList(LinkedList *pLinkedList);
