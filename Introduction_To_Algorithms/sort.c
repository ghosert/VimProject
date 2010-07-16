#include  <stdio.h>

void printArray(int *, int);
int binary_search(int *, int, int, int);
void bubble_sort(int *, int);

int main(int argc, char *argv[])
{

    // Given a sorted list for binary search
    int A[] = {1, 2, 2, 3, 4, 5, 6, 7};
    int r = binary_search(A, 1, 0, 7);
    printArray(A, sizeof(A) / sizeof(int));
    printf("The index of matched result is: %d\n", r);

    // bubble sortA:
    int B[] = {2, 1, 4, 9, 7, 5, 6, 10};
    int n = sizeof(B) / sizeof(int);
    bubble_sort(B, n);
    printf("sorted B:\n");
    printArray(B, n);

    return 0;
}

// lgN running time.
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

void bubble_sort(int *ap, int n)
{
    int i, j;
    for (i = 0; i <= n - 2; i++) {
        for (j = n - 1; j > i; j--) {
            if (ap[j] < ap[j - 1]) {
                int temp = ap[j - 1];
                ap[j - 1] = ap[j];
                ap[j] = temp;
            }
        }
    }

}

