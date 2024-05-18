#include  <stdio.h>
#include  "util.h"

// Heap sort.
void max_heapify(int *ap, int i, int heap_size);
void build_max_heap(int *ap, int n);
void heapsort(int *ap, int n);

// Page 122: max priority queue operations:
// Can be used in job shcedule like applications.
int heap_maximum(int *ap);
int heap_extract_max(int *ap, int *heap_size);
void heap_increase_key(int *ap, int i, int key);
void max_heap_insert(int *ap, int key, int *heap_size);

#define MIN_NUM -1000;

int main(int argc, char *argv[])
{
    int A[] = {4, 1, 3, 2, 16, 9, 10, 14, 8, 7};
    int n = sizeof(A) / sizeof(int);

    // Heap sort
    printf("\nBefore heap sorting:\n");
    printArray(A, n);
    heapsort(A, n);
    printf("After heap sorting:\n");
    printArray(A, n);

    // rebuild max heap for max priority queue below
    int B[] = {4, 1, 3, 2, 16, 9, 10, 14, 8, 7};
    n = sizeof(B) / sizeof(int);
    build_max_heap(B, n);
    printf("\nmax heap:\n");
    printArray(B, n);

    // max priority queue: for insert/remove element from B, it's better to use linkedlist instead of array in real codes.
    // c array can't change the size arbitrary.
    int *heap_size = &n;
    printf("heap maximum: %d\n", heap_maximum(B));
    printf("Before extract heap maximum: \n");
    printHeap(B, *heap_size);
    printf("extract heap maximum: %d\n", heap_extract_max(B, heap_size));
    printf("After extract heap maximum: \n");
    printHeap(B, *heap_size);
    heap_increase_key(B, 4, 11);
    printf("After increase heap by change the value of the 5th element from 7 to 11: \n");
    printHeap(B, *heap_size);
    max_heap_insert(B, 12, heap_size);
    printf("After insert a new element with the value 12 to max heap: \n");
    printHeap(B, *heap_size);
}

// Page 116: The running time is O(lgN) which is equal to on a node of height h as O(h).
void max_heapify(int *ap, int i, int heap_size)
{
    // We calculate (i + 1) * 2 & (i + 1) * 2 + 1 to get heap left & right index which starts from 1 first.
    // Then, we minus 1 to get array index which starts from 0.
    int left = (i + 1) * 2 - 1;
    int right = (i + 1) * 2 + 1 - 1;
    int largest;
    if (left < heap_size && ap[left] > ap[i]) {
        largest = left;
    } else {
        largest = i;
    }
    if (right < heap_size && ap[right] > ap[largest]) {
        largest = right;
    }
    if (largest != i) {
        // exchange ap[i] and ap[largest]
        int temp = ap[i];
        ap[i] = ap[largest];
        ap[largest] = temp;
        max_heapify(ap, largest, heap_size);
    }
}

// Page 117: only 1 to n / 2 node is node, n / 2 + 1 to n is leaf.
// So that we just max_heapify the nodes of the tree from bottom to up.
// The overall running time is O(n).
void build_max_heap(int *ap, int n)
{
    int i;
    for (i = n / 2 - 1; i >= 0; i--) {
        max_heapify(ap, i, n);
    }
}

// Page 120: Exchange the maximum ap[0] to the last position of the array, and then max heapify the ap[0] once again.
// Until all the n - 1 elements are handled, so that we got a heap sorted array.
// Running time: O(NlgN), since each of the n - 1 calls on max_heapify takes time O(lgN), while build_max_heap takes O(N)
void heapsort(int *ap, int n)
{
    build_max_heap(ap, n);
    int i;
    for (i = n - 1; i >= 1; i--) {
        int temp = ap[0];
        ap[0] = ap[i];
        ap[i] = temp;
        max_heapify(ap, 0, i);
    }
}

// Returns the element of A with the largest key.
// Running time: theta(1);
int heap_maximum(int *ap)
{
    return ap[0];
}

// Remove and return the element with largest key.
// Running time: O(lgN)
int heap_extract_max(int *ap, int *heap_size)
{
    if ((*heap_size) < 0) {
        printf("heap underflow");
    }
    int max = ap[0];
    ap[0] = ap[*heap_size - 1];
    *heap_size = *heap_size - 1;
    max_heapify(ap, 0, *heap_size);
    return max;
}

// increases the value of element i with new value key, which is assumed to be at least as large as i's current key value.
// Running time: O(lgN)
void heap_increase_key(int *ap, int i, int key)
{
    if (key < ap[i]) {
        printf("new key is smalller than current key");
        return;
    }
    ap[i] = key;
    int parent = (i + 1) / 2 - 1;
    while (parent > 0 && ap[parent] < ap[i]) {
        int temp = ap[parent];
        ap[parent] = ap[i];
        ap[i] = temp;
        i = parent;
        parent = (i + 1) / 2 - 1;
    }
}

// Insert the new element with the value key to set.
// Running time: O(lgN)
void max_heap_insert(int *ap, int key, int *heap_size)
{
    *heap_size = *heap_size + 1;
    ap[*heap_size - 1] = MIN_NUM;
    heap_increase_key(ap, *heap_size - 1, key);
}

