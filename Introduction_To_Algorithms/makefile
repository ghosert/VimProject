main:
	# if you need to use malloc() & free(), don't forget to #include <stdlib.h>, otherwise, a warning will be thrown by gcc compiler.
	gcc -c -o bin/sort.o sort.c
	gcc -o bin/sort bin/sort.o
	gcc -c -o bin/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o 5_ProbabilisticAnalysisAndRandomizedAlgorithms.c
	# -lm here is for pow() function, since pow can not accept variable without -lm.
	# 'man pow' to see link with -lm
	gcc -o bin/5_ProbabilisticAnalysisAndRandomizedAlgorithms bin/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o -lm
	gcc -c -o bin/6_Heapsort.o 6_Heapsort.c
	gcc -o bin/6_Heapsort bin/6_Heapsort.o -lm
	gcc -c -o bin/7_Quicksort.o 7_Quicksort.c
	gcc -o bin/7_Quicksort bin/7_Quicksort.o
clean:
	rm bin/* -r
