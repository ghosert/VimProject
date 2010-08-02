GCC_TMP = ~/gcc_test/tmp
GCC_LIB = ~/gcc_test/lib
GCC_BIN = ~/gcc_test/bin
GCC_INCLUDE = ~/gcc_test/include
LD_PATH = /home/jiawzhang/gcc_test/lib
# -fPIC is for compiling dynamic so on 64bit machine, otherwise, error will happen.
COMPILEOPTION = -c -fPIC

# if you need to use malloc() & free(), don't forget to #include <stdlib.h>, otherwise, a warning will be thrown by gcc compiler.
main:
	# -lm here is for pow() function, since pow can not accept variable without -lm.
	# 'man pow' to see link with -lm
	gcc -o $(GCC_TMP)/util.o $(COMPILEOPTION) util.c
	gcc -o $(GCC_LIB)/libutil.so $(GCC_TMP)/util.o -shared -lm
	gcc -o $(GCC_TMP)/2_GettingStarted.o $(COMPILEOPTION) 2_GettingStarted.c
	gcc -o $(GCC_BIN)/2_GettingStarted $(GCC_TMP)/2_GettingStarted.o -L$(LD_PATH) -lutil
	gcc -o $(GCC_TMP)/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o $(COMPILEOPTION) 5_ProbabilisticAnalysisAndRandomizedAlgorithms.c
	gcc -o $(GCC_BIN)/5_ProbabilisticAnalysisAndRandomizedAlgorithms $(GCC_TMP)/5_ProbabilisticAnalysisAndRandomizedAlgorithms.o -L$(LD_PATH) -lutil -lm
	gcc -o $(GCC_TMP)/6_Heapsort.o $(COMPILEOPTION) 6_Heapsort.c
	gcc -o $(GCC_BIN)/6_Heapsort $(GCC_TMP)/6_Heapsort.o -L$(LD_PATH) -lutil
	gcc -o $(GCC_TMP)/7_Quicksort.o $(COMPILEOPTION) 7_Quicksort.c
	gcc -o $(GCC_BIN)/7_Quicksort $(GCC_TMP)/7_Quicksort.o -L$(LD_PATH) -lutil
	cp *.h $(GCC_INCLUDE)/
clean:
	rm $(GCC_BIN)/* -rf
	rm $(GCC_LIB)/* -rf
	rm $(GCC_TMP)/* -rf
	rm $(GCC_INCLUDE)/* -rf
