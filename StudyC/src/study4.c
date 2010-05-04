#include <stdio.h>
#include <ctype.h>

main() {
	fourPointOne();
	fourPointTwo();
	fourPointSix();
	fourPointSix(); // To test the static variable.
	fourPointEight();
	fourPointTen();
	fourPointEleven();
}

#define MAXLINE 1000 /* maximum input line length */
int getline(char line[], int max);
int strindex(char source[], char searchfor[]);
char pattern[] = "ould"; /* pattern to search for */

/* find all lines matching pattern, just like grep command in linux. */
int fourPointOne() {
	char line[MAXLINE];
	int found = 0;
	while (getline(line, MAXLINE) > 0)
		if (strindex(line, pattern) >= 0) {
			printf("%s", line);
			found++;
		}
	return found;
}

/* getline: get line into s, return length */
int getline(char s[], int lim) {
	int c, i;
	i = 0;
	while (--lim > 0 && (c=getchar()) != EOF && c != '\n')
		s[i++] = c;
	if (c == '\n')
		s[i++] = c;
	s[i] = '\0';
	return i;
}

/* strindex: return index of t in s, -1 if none */
int strindex(char s[], char t[]) {
	int i, j, k;
	for (i = 0; s[i] != '\0'; i++) {
		for (j=i, k=0; t[k]!='\0' && s[j]==t[k]; j++, k++)
			;
		if (k > 0 && t[k] == '\0')
			return i;
	}
	return -1;
}

fourPointTwo() {
	double sum, atof(char []); // Let program know that atof will return a double value, or you can't use atof.
	                           // This can be written on the top of the code also.
	char line[MAXLINE];
	int getline(char line[], int max); // let program know that getline will return a int value.
	sum = 0;
	while (getline(line, MAXLINE) > 0)
		printf("\t%g\n", sum += atof(line)); // does %g means double value?
	return 0;
}

/* atof: convert string s to double */
double atof(char s[]) {
	double val, power;
	int i, sign;
	for (i = 0; isspace(s[i]); i++)
		/* skip white space */
		;
	sign = (s[i] == '-') ? -1 : 1;
	if (s[i] == '+' || s[i] == '-')
		i++;
	for (val = 0.0; isdigit(s[i]); i++)
		val = 10.0 * val + (s[i] - '0');
	if (s[i] == '.')
		i++;
	for (power = 1.0; isdigit(s[i]); i++) {
		val = 10.0 * val + (s[i] - '0');
		power *= 10;
	}
	return sign * val / power;
}


fourPointSix() {
	static int jtest = 0; /* static variable means the value will remain always. */
	jtest = jtest + 1;
	printf("The %d time(s) value: %d\n", jtest);
    double ifunction(); /* Because the ifunction is static function, here is a warning.*/
	ifunction();
	extern int itest; /* Here will be a warning. because of itest is a static variable.*/
}

static int itest = 0; /* static variable means, this variable can only
                       * used from the current line to the rest part of the current file.
                       * It can not be used in the other parts, even by add extern keywords.
                       */

static double ifunction() { /* static function is the same as static variable outside of the function.
                             * It means it can not be used out of it's effective area. Even if there is 
                             * a function declare. */
	printf("%d\n", 19801114);
}

fourPointEight() {
	/* The code below for variable i is not allowed in java. And this should be avoided to use 
	 * which will confuse the programmer.*/
	int i = 1;
	while (1) {
		int i = 2;
		printf("The second i: %d\n", i);
		break;
	}
	printf("The first i: %d\n", i);
}

fourPointTen() {
	printd(654321);
}

printd(int n) {
	if (n < 0) {
		printf("%c", '-');
		n = -n;
	}
	if (n / 10) {
		printd(n / 10);
	}
	printf("%c", n % 10 + '0');
}

#define max(A, B) ((A) > (B) ? (A) : (B))

#define square(x) x * x /* WRONG */

#undef putchar /* testing undef*/

#define forever for(;;)

#define dprint(expr) printf(#expr " = %g\n", expr)

#define paste(front, back) front ## back

fourPointEleven() {
	int a, b, c, d;
	a = b = 1;
	c = d = 2;
	int x = max(a + b, c + d); // equals 4.
	int y = square(2 + 1); // means 2 + 1 * 2 + 1 = 5 ; so the former define should be
	                       // #define square(x) (x) * (x) means (2 + 1) * (2 + 1) = 9 not 5. 5 is wrong result
	printf("\nmax(a + b, c + d) = %d\n", x);
	printf("square(x) x * x = %d\n", y);
	printf("after undefining putchar: %d\n", putchar(2008)); /* after undefining, the program
	                                                          * use self define putchar function */
	forever {
		printf("I am in forever.\n");
		break;
	}
	
    dprint(1.0*x/y); // means  printf("1.0*x/y" " = %g\n", 1.0*x/y);
                     // equals printf("1.0*x/y = %g\n", 1.0*x/y);
    printf("test paste macro: %d\n", paste(1, 2));
}

int putchar(int i) {
	return i;
}

//例如，为了保证hdr.h文件的内容只被包含一次，可以将该文件的内容包含在下列形式
//的条件语句中：
//#if !defined(HDR)     // equals #ifndef HDR, the opposite sentence is #ifdef HDR
//#define HDR
///* hdr.h文件的内容放在这里 */
//#endif
//第一次包含头文件

//下面的这段预处理代码首先测试系统变量SYSTEM，然后根据该变量的值确定包含哪个版本的头文件：
//#if SYSTEM == SYSV
//#define HDR "sysv.h"
//#elif SYSTEM == BSD
//#define HDR "bsd.h"
//#elif SYSTEM == MSDOS
//#define HDR "msdos.h"
//#else
//#define HDR "default.h"
//#endif
//#include HDR