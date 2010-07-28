#include  <stdio.h>
#include  <stdlib.h>

void max_heapify(int *ap, int i, int heap_size);

void build_max_heap(int *ap, int n);

void heapsort(int *ap, int n);

// max priority queue operations:
void insert(int **ap, x), maximum(A), extract_max(A), increase_key(A, x, key)

void printArray(int *ap, int i);

int main(int argc, char *argv[]) {
    int A[] = {4, 1, 3, 2, 16, 9, 10, 14, 8, 7};
    int n = 10;
    int *ap = (int *) malloc(sizeof(int) * n);

    // Heap sort
    printf("Before heap sorting:\n");
    printArray(A, n);
    heapsort(A, n);
    printf("After heap sorting:\n");
    printArray(A, n);

    // max priority queue
    // Page 122: insert(A, x), maximum(A), extract_max(A), increase_key(A, x, key)
    insert(S)

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
void heapsort(int *ap, int n) {
    build_max_heap(ap, n);
    int i;
    for (i = n - 1; i >= 1; i--) {
        int temp = ap[0];
        ap[0] = ap[i];
        ap[i] = temp;
        max_heapify(ap, 0, i);
    }
}

void printArray(int *ap, int i)
{
    int j;
    for (j = 0; j < i; j++) {
        printf("%d ", ap[j]);
    }
    printf("\n");
}
