#include  <stdio.h>

void insertion_sort(int *, int);
int binary_search(int *, int, int, int);
void bubble_sort(int *, int);
void printArray(int *, int);

void merge(int *, int, int, int);
void merge_sort(int *, int, int);

int main(int argc, char *argv[])
{
    printf("\n");

    // insertion sort
    int A0[] = {2, 1, 4, 9, 7, 5, 6, 10};
    int n = sizeof(A0) / sizeof(int);
    printf("Before INSERTION sorting A:\n");
    printArray(A0, n);
    insertion_sort(A0, n);
    printf("After INSERTION sorting A:\n");
    printArray(A0, n);

    printf("\n");

    // merge sort
    int A3[] = {2, 1, 4, 9, 7, 5, 6, 10};
    n = sizeof(A3) / sizeof(int);
    printf("Before MERGE sorting A:\n");
    printArray(A3, n);
    merge_sort(A3, 0, n - 1);
    printf("After MERGE sorting A:\n");
    printArray(A3, n);

    printf("\n");

    // Given a sorted list for binary search
    int A1[] = {1, 2, 2, 3, 4, 5, 6, 7};
    n = sizeof(A1) / sizeof(int);
    printf("BINARY search 1 from A:\n");
    printArray(A1, n);
    int r = binary_search(A1, 1, 0, n - 1);
    printf("The index of matched result is: %d\n", r);

    printf("\n");

    // bubble sort
    int A2[] = {2, 1, 4, 9, 7, 5, 6, 10};
    n = sizeof(A2) / sizeof(int);
    printf("Before BUBBLE sorting A:\n");
    printArray(A2, n);
    bubble_sort(A2, n);
    printf("After BUBBLE sorting A:\n");
    printArray(A2, n);

    printf("\n");

    return 0;
}

#define MAX_NUM 100000; 

// running time: cN(lgN + 1) = NlgN
// Merge sort: page 32
// Recursive call
void merge_sort(int *ap, int p, int r)
{
    if (p < r) {
        int q = (p + r) / 2;
        merge_sort(ap, p, q);
        merge_sort(ap, q + 1, r);
        merge(ap, p, q, r);
    }
}

// Merge: page 30
// Merge two sorted arrays, like ap = [2, 3, 4, 7, 8, 9], p = 0, q = 2, r = 5
void merge(int *ap, int p, int q, int r)
{
    int n1 = q - p + 1;
    int n2 = r - q;

    // create arrays L[1   n1 + 1] and R[1   n2 + 1]
    int L[n1 + 1];
    int R[n2 + 1];
    int i, j;
    for (i = 0; i < n1; i++) {
        L[i] = ap[p + i];
    }
    for (j = 0; j < n2; j++) {
        R[j] = ap[q + 1 + j];
    }

    L[n1] = MAX_NUM;
    R[n2] = MAX_NUM;

    i = 0;
    j = 0;
    int k;

    for (k = p; k < r + 1; k++) {
        if (L[i] > R[j]) {
            ap[k] = R[j];
            j++;
        } else {
            ap[k] = L[i];
            i++;
        }
    }
}

// Insertion sort: page 19 - 20
// running time: aN**2 + bN + c = N**2
void insertion_sort(int *ap, int n)
{
    int j;
    for (j = 1; j < n; j++) {
        int key = ap[j];
        // Insert ap[j] into the sorted sequence ap[0   j - 1].
        int i = j - 1;
        while (i >= 0 && ap[i] > key) {
            ap[i + 1] = ap[i];
            i = i - 1;
        }
        ap[i + 1] = key;
    }
}

// running time: lgN
// Binary search: Page 36
int binary_search(int *ap, int v, int low, int high)
{
    while (low <= high) {
        int mid = (low + high) / 2;
        if (v == ap[mid]) {
            return mid;
        } else if (v > ap[mid]) {
            low = mid + 1;
        } else if (v < ap[mid]) {
            high = mid - 1;
        }
    }
    return -1;
}

// Bubble sort: Page 37
void bubble_sort(int *ap, int n)
{
    int i, j;
    for (i = 0; i < n - 1; i++) {
        for (j = n - 1; j > i; j--) {
            if (ap[j] < ap[j - 1]) {
                int temp = ap[j - 1];
                ap[j - 1] = ap[j];
                ap[j] = temp;
            }
        }
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

