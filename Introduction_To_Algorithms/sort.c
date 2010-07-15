#include  <stdio.h>

void printArray(int *, int);
int binary_search(int *, int, int, int);

int main(int argc, char *argv[])
{

    // Given a sorted list for binary search
    int A[] = {1, 2, 2, 3, 4, 5, 6, 7};
    int r = binary_search(A, 1, 0, 7);

    printArray(A, sizeof(A) / sizeof(int));

    printf("The index of matched result is: %d\n", r);

    return 0;
}

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

void printArray(int *ap, int i)
{
    int j;
    for (j = 0; j < i; j++) {
        printf("%d ", ap[j]);
    }
    printf("\n");
}

