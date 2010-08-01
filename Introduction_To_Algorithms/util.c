#include  <stdio.h>
#include  <math.h>
#include  "util.h"

void printArray(int *ap, int i)
{
    int j;
    for (j = 0; j < i; j++) {
        printf("%d ", ap[j]);
    }
    printf("\n");
}

void printHeap(int *ap, int heap_size)
{
    int i, j;
    int height = log2(heap_size) + 1;
    for (i = 0; i < height; i++) {
        for (j = 0; j < pow(2, i); j++) {
            int index = (int) pow(2, i) + j - 1;
            if (index == heap_size) break;
            printf("%d ", ap[index]);
        }
        printf("\n");
    }
}

