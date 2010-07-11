#!/usr/bin/python

# Insertion sort:

A = [5, 2, 4, 6, 1, 3]

for j, key in enumerate(A[1:]):
    j = j + 1
    # Insert A[j] into the sorted sequence A[0 j-1]
    i = j - 1
    while i >= 0 and A[i] > key:
        A[i + 1] = A[i]
        i = i - 1
    A[i + 1] = key

print A

