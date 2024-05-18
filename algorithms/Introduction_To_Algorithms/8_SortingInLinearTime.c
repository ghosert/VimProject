#include  <stdio.h>
#include  <math.h>
#include  "util.h"

typedef struct entity {
    int key;
    int value;
} Entity;

void counting_sort(int *ap, int *bp, int n, int k);

void radix_sort(int *ap, int n, int digit);
void counting_sort_radix(Entity *e, int *bp, int n, int k);

int main(int argc, char *argv[])
{
    // counting sort:
    int A[] = {2, 5, 3, 0, 2, 3, 0, 3};
    int n = sizeof(A) / sizeof(int);
    printf("\nBefore counting sort:\n");
    printArray(A, n);

    int B[n];
    counting_sort(A, B, n, 5); // k = 5, since 5 is the largest one in A array.

    printf("\nAfter counting sort:\n");
    printArray(B, n);


    // radix sort:
    int A1[] = {329, 457, 657, 839, 436, 720, 355};
    n = sizeof(A1) / sizeof(int);
    printf("\nBefore radix sort:\n");
    printArray(A1, n);

    radix_sort(A1, n, 3); // digit = 3, since the digit of the number is 3.

    printf("\nAfter radix sort:\n");
    printArray(A1, n);
}

// page 148.
// The running time is: theta(n + k), we simplify it as theta(n)
// It make sense to use this sort when the maximum value k <= n
void counting_sort(int *ap, int *bp, int n, int k)
{
    k = k + 1; // we should have at least 0-k position.
    int C[k];
    int i;
    for (i = 0; i < k; i++) {
        C[i] = 0;
    }
    for (i = 0; i < n; i++) {
        C[ap[i]] = C[ap[i]] + 1;
    }
    for (i = 0; i < k - 1; i++) {
        C[i + 1] = C[i] + C[i + 1];
    }
    for (i = n - 1; i >= 0; i--) {
        bp[C[ap[i]] - 1] = ap[i];
        C[ap[i]] = C[ap[i]] - 1;
    }
}

// page 150.
// The running time is: theta(d * (n + k)), (n + k) here is got form reusing counting sort(see above), we simplify overall running time as theta(n).
// counting sort, and the version of radix sort use counting sort does not sort in place,
// means it will create new memeory for sorting, so not in place will take more memoery but may be faster.
void radix_sort(int *ap, int n, int digit)
{
    Entity e[n];
    int i;
    for (i = 0; i < digit; i++) {
        int k = 0;
        int j;
        for (j = 0; j < n; j++) {
            e[j].key = ap[j] / (int) pow(10, i) % 10;
            e[j].value = ap[j];
            if (e[j].key > k) {
                k = e[j].key; // get maximum k.
            }
        }
        counting_sort_radix(e, ap, n, k);
    }
}

// counting_sort for radix.
void counting_sort_radix(Entity *e, int *bp, int n, int k)
{
    k = k + 1; // we should have at least 0-k position.
    int C[k];
    int i;
    for (i = 0; i < k; i++) {
        C[i] = 0;
    }
    for (i = 0; i < n; i++) {
        C[e[i].key] = C[e[i].key] + 1;
    }
    for (i = 0; i < k - 1; i++) {
        C[i + 1] = C[i] + C[i + 1];
    }
    for (i = n - 1; i >= 0; i--) {
        bp[C[e[i].key] - 1] = e[i].value;
        C[e[i].key] = C[e[i].key] - 1;
    }
}

