#!/usr/bin/python

from decimal import Decimal
infinity = Decimal('Infinity')

# Insertion sort: page 19 - 20
def insertion_sort(A):
    for j, key in enumerate(A):

        # Skip the first item, since it's already sorted for a single item.
        if j == 0:
            continue

        # Insert A[j] into the sorted sequence A[0 j-1]
        i = j - 1
        while i >= 0 and A[i] > key:
            A[i + 1] = A[i]
            i = i - 1
        A[i + 1] = key

# Merge: page 30
def merge(A, p, q, r):

    n1 = q - p + 1
    n2 = r - q
    # create arrays L[1   n1 + 1] and R[1   n2 + 1]
    L = [A[p + i] for i in range(n1)]
    R = [A[q + 1 + j] for j in range(n2)]

    # [2, 4, 5, 7, infinity]
    L.append(infinity)
    # [1, 2, 3, 6, infinity]
    R.append(infinity)

    i = 0
    j = 0
    n = r - p + 1

    for ni in range(n):
        ai = ni + p
        if L[i] <= R[j]:
            A[ai] = L[i]
            i = i + 1
        else:
            A[ai] = R[j]
            j = j + 1

# Merge sort: page 32
# Recursive call
def merge_sort(A, p, r):
    if p < r:
        q = (p + r) / 2
        merge_sort(A, p, q)
        merge_sort(A, q + 1, r)
        merge(A, p, q, r)

# Binary search: Page 36
def binary_search(A, a, p, r):
    while p <= r:
        p, r = binary(A, a, p, r)
    return  r if A[r] == a else None

def binary(A, a, p, r):
    z = (p + r) / 2
    if A[z] == a:
        return z + 1, z
    elif A[z] < a:
        return z + 1, r
    elif A[z] > a:
        return p, z - 1

# Bubble sort: Page 37
def bubble_sort(A):
    for i in range(len(A)):
        j = len(A) - 1 - i
        for k in range(j):
            l = len(A) - 1 - k
            if A[l] < A[l - 1]:
                temp = A[l]
                A[l] = A[l - 1]
                A[l - 1] = temp


if __name__ == '__main__':

    A = [5, 2, 4, 6, 1, 3]
    insertion_sort(A)
    print A

    A = [2, 4, 5, 7, 1, 2, 3, 6]
    # means li[0:4] is a sorted sub array while li[4:8] is another sorted sub array
    # so we set 0, 3, 7 here.
    # we need to merge sort two sorted sub arrays into one sorted array.
    merge(A, 0, 3, 7)
    print A

    A = [5, 2, 4, 7, 1, 3, 2, 6]
    merge_sort(A, 0, len(A) - 1)
    print A
    
    # Given a sorted list for binary search
    A = [1, 2, 2, 3, 4, 5, 6, 7]
    # print binary_search(A, 5, 0, len(A) - 1)
    print binary_search(A, 1, 0, 7)

    A = [5, 2, 4, 7, 1, 3, 2, 6]
    bubble_sort(A)
    print A


