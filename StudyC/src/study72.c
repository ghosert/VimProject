// stdio.h is stored in /usr/include/stdio.h
// effect on printf, getchar, putchar, getc, putc, fprintf, fscan, ...
#include <stdio.h> 

// Test this by using command:
// ./study72
// ./study72 jiaweijiawei
// ./study72 testdata
// ./study72 testdata testdata
int main(int argc, char *argv[]) {

	FILE *fp;
	void filecopy(FILE *, FILE *);
	char *prog = argv[0]; /* program name for errors */
	if (argc == 1) /* no args; copy standard input */
		filecopy(stdin, stdout); // stdin, stdout, stderr
	else
		while (--argc > 0)
			if ((fp = fopen(*++argv, "r")) == NULL) { // "r" "w" "a" mode, fopen return NULL if error happen.
				// we use the sentence below instead of printf(), because this allow us to redirect the 
				// error information to another channel.
				// eg. if we type command: progam 1>a 2>b, the data in the stderr will be saved to b.
				fprintf(stderr, "%s: can't open %s\n", prog, *argv);
				exit(1);
			} else {
				filecopy(fp, stdout);
				fclose(fp);
			}
	if (ferror(stdout)) { // check if the stdout error happens, eg. the disk is full.
		                  // and also we use feof(FILE *) to see whether a file reach the end of a file.
		fprintf(stderr, "%s: error writing stdout\n", prog);
		exit(2);
	}
	// write something to the file writedata
	// and then reload it to a variable and print it out.
	fp = fopen("writedata", "w");
	fprintf(fp, "Youare%s\n", "pig.");
	fclose(fp);

	fp = fopen("writedata", "r");
	char string[100];
	fscanf(fp, "%s", string);
	fclose(fp);
	printf("%s\n", string);

	printf("Begin to read line by using fgets()\n");
	fp = fopen("testdata", "r");
	while (fgets(string, 100, fp) != NULL) { // read a line with the MAX charaters. return NULL if error or eof
		                                     // using fputs to write a line to FILE
	    printf("%s", string);
	}
	fclose(fp);
	
	printf("Begin to read line by using gets()\n");
	gets(string); // similar to fgets but work for the stdin, and without '\n' input.
	puts(string); // similar to fputs but work for the stdout, and with '\n' output.

	exit(0);
}

/* filecopy: copy file ifp to file ofp */
void filecopy(FILE *ifp, FILE *ofp) {
	int c;
	while ((c = getc(ifp)) != EOF) // getc return EOF if error happens or reach at the end of file.
		putc(c, ofp); // putc return EOF if error happens.
}

