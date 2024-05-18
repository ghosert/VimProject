package basic;

import java.util.Arrays;

class BubbleSort extends ISort {

    public void sort(int[] numbers) {
        int n = numbers.length;
        for (int i = 0; i < n - 1; i++) {
            for (int j = n - 1; j > i; j--) {
                if (numbers[j] < numbers[j - 1]) { // swap if the latter is less than the former.
                    int a = numbers[j];
                    numbers[j] = numbers[j - 1];
                    numbers[j - 1] = a;
                }
            }
        }
    }
}

class SelectionSort extends ISort {

    public void sort(int[] numbers) {
        int n = numbers.length;
        for (int j = 0; j < n - 1; j++) {
            int minIndex = j;
            for (int i = j + 1; i < n; i++) {
                if (numbers[i] < numbers[minIndex]) {
                    minIndex = i;
                }
            }
            int a = numbers[j];
            numbers[j] = numbers[minIndex];
            numbers[minIndex] = a;
        }
    }
}

class InsertionSort extends ISort {

    public void sort(int[] numbers) {
        int n = numbers.length;
        for (int j = 1; j < n; j++) {
            for (int i = j; i > 0; i--) {
                if (numbers[i] < numbers[i -1]) {
                    int a = numbers[i];
                    numbers[i] = numbers[i - 1];
                    numbers[i - 1] = a;
                }
            }
        }
    }
}

class MergeSort extends ISort {

    public void sort(int[] numbers) {
        int[] sortedArray = this.splitArray(numbers);
        for (int i = 0; i < numbers.length; i++) {
            numbers[i] = sortedArray[i];
        }
    }

    private int[] splitArray(int[] numbers) {
        if (numbers.length != 1) {
            int n = numbers.length / 2;
            int[] firstArray = Arrays.copyOf(numbers, n);
            int[] secondArray = Arrays.copyOfRange(numbers, n, numbers.length);
            int[] sortedFirstArray = this.splitArray(firstArray);
            int[] sortedSecondArray = this.splitArray(secondArray);
            int[] sortedArray = new int[sortedFirstArray.length + sortedSecondArray.length];
            for (int i = 0, j = 0, k = 0; k < sortedArray.length; k++) {
                if (sortedFirstArray[i] < sortedSecondArray[j]) {
                    sortedArray[k] = sortedFirstArray[i];
                    if (i < sortedFirstArray.length - 1) {
                        i++;
                    } else {
                        sortedFirstArray[i] = Integer.MAX_VALUE;
                    }
                } else {
                    sortedArray[k] = sortedSecondArray[j];
                    if (j < sortedSecondArray.length - 1) {
                        j++;
                    } else {
                        sortedSecondArray[j] = Integer.MAX_VALUE;
                    }
                }
            }
            return sortedArray;
        }
        return numbers;
    }
}

class HeapSort extends ISort {

    public void sort(int[] numbers) {
        this.buildHeap(numbers);
        for (int i = numbers.length - 1; i >= 0; i --) {
            this.swap(numbers, 0, i);
            heapify(numbers, 0, i);
        }
    }

    private void buildHeap(int[] numbers) {
        int index = numbers.length / 2 -  1;
        for (int i = index; i >= 0; i--) {
            this.heapify(numbers, i, numbers.length);
        }
    }

    private void heapify(int[] numbers, int i, int n) {
        int l = 2 * (i + 1)  - 1;
        int r = 2 * (i + 1);
        int max = i;
        if (l < n && numbers[l] > numbers[i]) {
            max = l;
        }
        if (r < n && numbers[r] > numbers[max]) {
            max = r;
        }
        if (max != i) {
            this.swap(numbers, max, i);
            heapify(numbers, max, n);
        }
    }

    private void swap(int[] numbers, int i, int j) {
        int a = numbers[i];
        numbers[i] = numbers[j];
        numbers[j] = a;
    }
}

abstract class ISort {

    abstract void sort(int[] numbers);

    void compareResult(int[] originalNumbers, int[] resultNumbers) {
        this.printNumbers("Original sequence:", originalNumbers);
        Arrays.sort(originalNumbers);
        this.printNumbers("Sorted result:", resultNumbers);
        if (Arrays.equals(originalNumbers, resultNumbers)) {
            System.out.println(this.getClass().getSimpleName() + " passed!");
        } else {
            System.out.println(this.getClass().getSimpleName() + "failed!");
        }
    }

    void printNumbers(String statement, int[] numbers) {
        System.out.println(statement);
        for (int n : numbers) {
            System.out.printf("%d ", n);
        }
        System.out.println();
    }
}

public class Sort {
    public static void main(String[] args) {
        int[] numbers = new int[] {4, 8, 2, 3, 7, 4, 5, 9, 1};
        runSort(new BubbleSort(), numbers);
        runSort(new SelectionSort(), numbers);
        runSort(new InsertionSort(), numbers);
        runSort(new MergeSort(), numbers);
        runSort(new HeapSort(), numbers);
    }

    private static void runSort(ISort sort, int[] numbers) {
        int[] resultNumbers = numbers.clone();
        sort.sort(resultNumbers);
        sort.compareResult(numbers.clone(), resultNumbers);
        System.out.println();
    }
}

