#include  <stdio.h>
#include  "util.h"

void minimum(int *ap, int n, int *min);
void min_max(int *ap, int n, int *min, int *max);

int partition(int *ap, int p, int r);
int randomized_partition(int *ap, int p, int r);
// Get the ith smallest element, p, r is 0 based, while i is 1 based.
// Page 163, the average case running time is theta(n), the worst case is: theta(n**2)
// But we have a selection in worst-case linear time algorithm in page 166.
int randomized_select(int *ap, int p, int r, int i);

int main(int argc, char *argv[])
{
    int A[] = {5, 3, 8, 9, 2, 4, 7, 6, 0};
    int n = sizeof(A) / sizeof(int);
    printf("\nCurrent Array A:\n");
    printArray(A, n);
    int min;
    minimum(A, n, &min);
    printf("Minimum a from array: %d\n", min);
    int max;
    min_max(A, n, &min, &max);
    printf("Minimum a & Maximum b from array: %d %d\n", min, max);
    int ith = randomized_select(A, 0, n - 1, n / 2);
    printf("Get %dth smallest element from array: %d\n", n / 2, ith);
}

// n - 1 comparisions totally.
void minimum(int *ap, int n, int *min)
{
    *min = ap[0];
    int i;
    for (i = 1; i < n; i++) {
        if (*min > ap[i]) {
            *min = ap[i];
        }
    }
}

// 3[n/2] comparisions totally.
void min_max(int *ap, int n, int *min, int *max)
{
    int i;
    if (n % 2 == 0) {
        if (ap[1] > ap[0]) {
            *min = ap[0];
            *max = ap[1];
        } else {
            *min = ap[1];
            *max = ap[0];
        }
        i = 2;
    } else {
        *min = ap[0];
        *max = ap[0];
        i = 1;
    }
    if (n == 1 || n == 2) return;
    for (; i < n; i++) {
        int j = ap[i];
        int k = ap[++i];
        if (j > k) {
            int tmp = k;
            k = j;
            j = tmp;
        }
        if (*min > j) {
            *min = j;
        }
        if (*max < k) {
            *max = k;
        }
    }
}

int randomized_select(int *ap, int p, int r, int i)
{
    if (p == r) {
        return ap[p];
    }
    int q = randomized_partition(ap, p, r);
    int k = q - p + 1;
    if (i == k) {
        return ap[q];
    }
    if (i < k) {
        randomized_select(ap, p, q - 1, i);
    } else {
        randomized_select(ap, q + 1, r, i - k);
    }
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

int randomized_partition(int *ap, int p, int r)
{
    int i = random(p, r);
    int tmp = ap[r];
    ap[r] = ap[i];
    ap[i] = tmp;
    return partition(ap, p, r);
}

