main:
	gcc -c -o bin/sort.o sort.c
	gcc -o bin/sort bin/sort.o
	gcc -c -o bin/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o 5_ProbabilisticAnalysisAndRandomizedAlgorithms.c
	# -lm here is for pow() function, since pow can not accept variable without -lm.
	gcc -o bin/5_ProbabilisticAnalysisAndRandomizedAlgorithms bin/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o -lm
clean:
	rm bin/* -r
