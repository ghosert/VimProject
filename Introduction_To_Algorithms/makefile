GCC_TMP = ~/gcc_test/tmp
GCC_LIB = ~/gcc_test/lib
GCC_BIN = ~/gcc_test/bin
GCC_INCLUDE = ~/gcc_test/include
LD_PATH = /home/jiawzhang/gcc_test/lib

# if you need to use malloc() & free(), don't forget to #include <stdlib.h>, otherwise, a warning will be thrown by gcc compiler.
main:
	# -lm here is for pow() function, since pow can not accept variable without -lm.
	# 'man pow' to see link with -lm
	gcc -o $(GCC_TMP)/util.o -c util.c
	gcc -o $(GCC_LIB)/libutil.so $(GCC_TMP)/util.o -shared -lm
	gcc -c -o $(GCC_TMP)/sort.o sort.c
	gcc -o $(GCC_BIN)/sort $(GCC_TMP)/sort.o -L$(LD_PATH) -lutil
	gcc -c -o $(GCC_TMP)/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o 5_ProbabilisticAnalysisAndRandomizedAlgorithms.c
	gcc -o $(GCC_BIN)/5_ProbabilisticAnalysisAndRandomizedAlgorithms $(GCC_TMP)/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o -L$(LD_PATH) -lutil -lm
	gcc -c -o $(GCC_TMP)/6_Heapsort.o 6_Heapsort.c
	gcc -o $(GCC_BIN)/6_Heapsort $(GCC_TMP)/6_Heapsort.o -L$(LD_PATH) -lutil
	gcc -c -o $(GCC_TMP)/7_Quicksort.o 7_Quicksort.c
	gcc -o $(GCC_BIN)/7_Quicksort $(GCC_TMP)/7_Quicksort.o -L$(LD_PATH) -lutil
	cp *.h $(GCC_INCLUDE)/
clean:
	rm $(GCC_BIN)/* -rf
	rm $(GCC_LIB)/* -rf
	rm $(GCC_TMP)/* -rf
	rm $(GCC_INCLUDE)/* -rf
