#include  <stdio.h>

// Page 128-129

// worst-case running time when input array is unbalanced: theta(n**2), like insertion sort.
// best-case running time when input array is balanced: theta(nlgn), like merge sort.
// average-case running time for expecting/arverage case/randomized array: theta(nlgn) which is much closer to the best case above but with a larger constant hidden by the O-notation.
// sorting in place.
// quicksort is often the best practical choice for sorting.
void quick_sort(int *ap, int p, int r);
// running time of partition is theta(n);
int partition(int *ap, int p, int r);

// To reduce the running time, randomize the input first, then quick sort. This is actually an average case after randomizing.
// Many people regard the resulting randomized version of quick sort as the sorting algorithm of choice for large enough inputs.
int randomized_partition(int *ap, int p, int r);
void randomized_quick_sort(int *ap, int p, int r);
int random(int p, int r);

void printArray(int *ap, int i);


int main(int argc, char *argv[])
{
    int A[] = {2, 8, 7, 1, 3, 5, 6, 4};
    int n = sizeof(A) / sizeof(int);
    printf("\nBefore quick sorting:\n");
    printArray(A, n);

    quick_sort(A, 0, n - 1);

    printf("\nAfter quick sorting:\n");
    printArray(A, n);

    int B[] = {2, 8, 7, 1, 3, 5, 6, 4};
    int m = sizeof(B) / sizeof(int);
    printf("\nBefore randomized quick sorting:\n");
    printArray(B, m);

    randomized_quick_sort(B, 0, m - 1);

    printf("\nAfter randomized quick sorting:\n");
    printArray(B, m);

    return 0;
}

int partition(int *ap, int p, int r)
{
    int x = ap[r];
    int i = p - 1;
    int j;
    for (j = p; j < r; j++) {
        if (ap[j] <= x) {
            i = i + 1;
            int tmp = ap[i];
            ap[i] = ap[j];
            ap[j] = tmp;
        }
    }
    int tmp = ap[r];
    ap[r] = ap[i + 1];
    ap[i + 1] = tmp;
    return i + 1;
}

void quick_sort(int *ap, int p, int r)
{
    if (p < r) {
        int q = partition(ap, p, r);
        quick_sort(ap, p, q - 1);
        quick_sort(ap, q + 1, r);
    }
}

int randomized_partition(int *ap, int p, int r)
{
    int i = random(p, r);
    int tmp = ap[r];
    ap[r] = ap[i];
    ap[i] = tmp;
    return partition(ap, p, r);
}

void randomized_quick_sort(int *ap, int p, int r)
{
    if (p < r) {
        int q = randomized_partition(ap, p, r);
        randomized_quick_sort(ap, p, q - 1);
        randomized_quick_sort(ap, q + 1, r);
    }
}

int random(int p, int r)
{
    srand(time(NULL));
    return (rand() % (r - p + 1)) + p; // random(p, r)
}

void printArray(int *ap, int i)
{
    int j;
    for (j = 0; j < i; j++) {
        printf("%d ", ap[j]);
    }
    printf("\n");
}

