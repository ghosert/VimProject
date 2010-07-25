#include  <stdio.h>
#include  <math.h>

void hire_assistant(int *, int);
void randomized_hire_assistant(int *, int);
void permute_by_sorting(int *, int);
void randomize_in_place(int *, int);
void printArray(int *ap, int i);

int main(int argc, char *argv[])
{
    int A[] = {2, 8, 5, 9, 7, 10};
    int n = sizeof(A) / sizeof(int);

    // Probabilistic Analysis
    printf("\nhire assistant\n");
    hire_assistant(A, n);

    printf("\n");
    
    // Randomized hire assistant
    printf("randomized assistant\n");
    randomized_hire_assistant(A, n);

    printf("\n");
}

// E[X] = 1/1 + 1/2 + 1/3 + 1/4 ... + 1/N = lnN + O(1)
// means, assume the candidates are coming with a random order, for N candidate.
// Even though we interview N people, we only actually hire approximately lnN of them,
// on average.
void hire_assistant(int *ap, int n)
{
    int best = 0;
    int i;
    for (i = 0; i < n; i++) {
        if (ap[i] > best) {
            best = ap[i];
            printf("Hiring index: %d, the value is: %d\n", i, best);
        }
    }
}

// Randomize the list before picking up the candidates.
// The expected hiring cost of the procedure is same to hire_assistant, which is O(chlnN)
void randomized_hire_assistant(int *ap, int n)
{
    // Randomly permute the list of candidates.

    // Option 1: permute by sorting.
    printf("Before permute by sorting:\n");
    printArray(ap, n);
    permute_by_sorting(ap, n);
    printf("After permute by sorting:\n");
    printArray(ap, n);

    // Option 2: randomize in place.
    printf("Before randomize in place:\n");
    printArray(ap, n);
    randomize_in_place(ap, n);
    printf("After randomize in place:\n");
    printArray(ap, n);

    int best = 0;
    int i;
    for (i = 0; i < n; i++) {
        if (ap[i] > best) {
            best = ap[i];
            printf("Hiring index: %d, the value is: %d\n", i, best);
        }
    }
}

typedef struct entity {
    int key;
    int value;
} Entity;

void insertion_sort(Entity *, int);

// for comparison sort, the running time is omiga(nlgn) for average case.
void permute_by_sorting(int *ap, int n)
{
    Entity e[n];
    srand(time(NULL));
    int p[n];
    int i;
    printf("Randomly generate P as sort keys to sort A:");
    for (i = 0; i < n; i++) {
        p[i] = rand() % (int) pow(n, 3);
        printf(" %d", p[i]);
        e[i].key = p[i];
        e[i].value = ap[i];
    }
    printf("\n");
    insertion_sort(e, n);
    for (i = 0; i < n; i++) {
        ap[i] = e[i].value;
    }
}

void insertion_sort(Entity *ep, int n)
{
    int j;
    for (j = 1; j < n; j++) {
        Entity key = ep[j];
        int i = j - 1;
        while (i >= 0 && ep[i].key > key.key) {
            ep[i + 1] = ep[i];
            i = i - 1;
        }
        ep[i + 1] = key;
    }
}

// for randomize_in_place, the running time is O(n).
void randomize_in_place(int *ap, int n)
{
    srand(time(NULL));
    int i;
    printf("Randomly swap A[i] value in place:");
    for (i = 0; i < n; i++) {
        int j = (rand() % (n - i)) + i; // random(i, n)
        printf(" %d", j);
        // swap A[i] <--> A[RANDOM(i, n)]
        int temp = ap[j];
        ap[j] = ap[i];
        ap[i] = temp;
    }
    printf("\n");
}

void printArray(int *ap, int i)
{
    int j;
    for (j = 0; j < i; j++) {
        printf("%d ", ap[j]);
    }
    printf("\n");
}
