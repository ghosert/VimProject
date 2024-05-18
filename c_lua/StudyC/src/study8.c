// work for function va_list etc.
#include <stdarg.h>
// #include "syscalls.h"
#include <stdio.h>
// work for const O_RDONLY O_WRONLY O_RDWR
#include <fcntl.h>

// below two .h file work for lseek function, but it seems it is not neccessary in linux env and MingW
// just for cygwin, without them the lseek function always retrun -1.
#include <sys/types.h>
#include <unistd.h>

#undef getchar

#define PERMS 0666 /* RW for owner, group, others */

// we are invoking the system function in this part. it just like win32 api.

// Test this by using command:
// ./study8
// ./study8 testdata targetdata
int main(int argc, char *argv[]) /* copy input to output */
{
	char buf[BUFSIZ];
	int n;
	printf("Use read and write.\n");
	// There are three default file type: 0 means stdin, 1 means stdout, 2 means stderr
	// below 0 means read data from stdin.
	while ((n = read(0, buf, BUFSIZ)) > 0)
		// read return -1 means error, return 0 means eof.
		// below 1 means write data to stdout.
		write(1, buf, n); // if the return number does not equals to n, means error happened.
	printf("Use getchar()\n");
	int getchar();
	char c = getchar();
	putchar(c);
	printf("Use getchar2()\n");
	int getchar2();
	c = getchar2();
	putchar(c);
	c = getchar2(); // the data has been stored in buf[]
	putchar(c);

	/* cp: copy f1 to f2 */
	void error(char *, ...);
	int f1, f2, m;
	char buff[BUFSIZ];
	if (argc != 3)
		error("Usage: cp from to");
	
	// O_RDONLY 以只读方式打开文件
	// O_WRONLY 以只写方式打开文件
	// O_RDWR 以读写方式打开文件
	if ((f1 = open(argv[1], O_RDONLY, 0)) == -1) // return -1 if error happened.
		                                         // it will not create a file if it doesn't exist.
		                                         // the third parameter will always be 0.
		                                         
		error("cp: can't open %s", argv[1]);
	if ((f2 = creat(argv[2], PERMS)) == -1)  // return -1 if error, create a new file if it doesn't exist.
		                                     // abandon an old file if it exists and then create a new one.
		                                     // the second parameter means permission, is a octal number.
		                                     // like 0777 0755
		error("cp: can't create %s, mode %03o", argv[2], PERMS);
	while ((m = read(f1, buff, BUFSIZ)) > 0)
		if (write(f2, buff, m) != m) // if the size output does not equals to input, that means error happened
			error("cp: write error on file %s", argv[2]);
	
	// add words at the top of the f2.
	char *addString = "I'm adding something at the top of the file.\n";
	int length = strlen(addString);
	printf("string length to be added: %d\n", length);
	if (lseek(f2, 0L, 2) != -1) { /* return new position or -1 if error. */
		                          // the second parameter means the relative postion to the third parameter
		                          // the third parameters could be 0, 1, 2 means
		                          // 0: head position of file 1: current position of file
		                          // 2: tail position of file
//lseek allow the user to move file position quickly, don't have to move file position by reading file.		
//      在UNIX shell 程序中使用重定向符>>或在系统调用fopen 中使用参数
//		“a”），则在写操作之前必须使用下列系统调用找到文件的末尾：
//		lseek(fd, 0L, 2);
//		若要返回文件的开始处（即反绕），则可以使用下列调用：
//		lseek(fd, 0L, 0);
// fseek do the same thing but return a non zero when error happens.
		length = write(f2, addString, length);
		printf("string length has been added: %d\n", length);
	} else {
		printf("lseek error value: %d\n", -1);
	}
	
	printf("\nDo you want to delete the file copied just now(y/n): ");
	char cc;
	scanf("%c", &cc);
	if (cc == 'y') {
		unlink(argv[2]); // just linke command remove in unix.
	}
	
	printf("the file %s length is %d", argv[2], lseek(f2, 0L, 2));
	// if f1 f2 want to be reuse below, it should be invoked by close(int fd); for one program can only 
	// open 20 files. so close the existing fd for reuse purpose is adviced.
	
	return 0;
}

/* getchar: unbuffered single character input */
int getchar(void) {
	char c;
	return (read(0, &c, 1) == 1) ? (unsigned char) c : EOF;
}

/* getchar: simple buffered version */
int getchar2(void) {
	static char buf[BUFSIZ];
	static char *bufp = buf;
	static int n = 0;
	if (n == 0) { /* buffer is empty */
		n = read(0, buf, sizeof buf);
		bufp = buf;
	}
	return (--n >= 0) ? (unsigned char) *bufp++ : EOF;
}

/* error: print an error message and die */
void error(char *fmt, ...) {
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "error: ");
	vfprintf(stderr, fmt, args); // use vfprintf to print the variable here.
	fprintf(stderr, "\n");
	va_end(args);
	exit(1);
}
