#include <stdio.h>
#include <time.h> 
#include <sys/time.h> 
#include <unistd.h>
#include <locale.h>

int main(int argc, char *argv) {
	struct timeval start;
	struct timeval end;
	gettimeofday(&start, NULL);
	
	int i;
	for (i = 0; i < 1000000000; i++) {
	}
	
	gettimeofday(&end, NULL);
	long l = (end.tv_sec - start.tv_sec) * 1000000 + (end.tv_usec - start.tv_usec);
	printf("%ld\n", l);
	
	exit(0);
}
