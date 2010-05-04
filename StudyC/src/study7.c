// stdio.h is stored in /usr/include/stdio.h
// effect on printf, getchar, putchar, getc, putc, fprintf, fscan, ...
#include <stdio.h> 
// effect on tolower
#include <ctype.h>
// effect on va_list, va_start, va_arg, va_end
#include <stdarg.h>

// Test this by using command:
// ./study7 < testdata
// cat testdata | ./study7
// ./study7 < testdata > resultdata
int main(int argc, char *argv[]) {

	printf("The number of parameters: %d\n", argc);
	if (argc > 0)
		printf("They are:");
	while (argc--) {
		printf("%s ", *argv++);
	}
	printf("\n");

	sevenPointOne();
	sevenPointTwo();
	sevenPointThree();
	sevenPointFour();
}

sevenPointOne() {
	char c;
	while ((c = getchar()) != EOF) {
		putchar(tolower(c));
	}
}

sevenPointTwo() {
	// These sentence is the same. Notice how to use the signal *
	printf("%.9g\n", 1.0 / 3);
	printf("%.*g\n", 9, 1.0 / 3);
	// These sentence is the same. Notice how to use the signal *
	printf("%.*s\n", 6, "jiaweinumberone");
	printf("%.6s\n", "jiaweinumberone");

	printf("%s\n", "hello, world");
	printf("%10s\n", "hello, world");
	printf("%.10s\n", "hello, world");
	printf("%-10s\n", "hello, world");
	printf("%.15s\n", "hello, world");
	printf("%-15s\n", "hello, world");
	printf("%15.10s\n", "hello, world");
	printf("%-15.10s\n", "hello, world");

	// sprintf is similar to printf, but it will not output the result to
	// screen but store them in the first char arrays.
	// so the size of char arrays should be large enough.
	char string[100];
	sprintf(string, "%s who are you\n", "hello, world");
	printf("%s", string);
}

sevenPointThree() {
	void minprintf(char *fmt, ...);
	minprintf("%s\n", "jiaweinumberone");
}

/* minprintf: minimal printf with variable argument list */
void minprintf(char *fmt, ...) {
	va_list ap; /* points to each unnamed arg in turn */
	char *p, *sval;
	int ival;
	double dval;
	va_start(ap, fmt);
	/* make ap point to 1st unnamed arg */
	for (p = fmt; *p; p++) {
		if (*p != '%') {
			putchar(*p);
			continue;
		}
		switch (*++p) {
		case 'd':
			ival = va_arg(ap, int);
			printf("%d", ival);
			break;
		case 'f':
			dval = va_arg(ap, double);
			printf("%f", dval);
			break;
		case 's':
			for (sval = va_arg(ap, char *); *sval; sval++)
				putchar(*sval);
			break;
		default:
			putchar(*p);
			break;
		}
	}
	va_end(ap);
	/* clean up when done */
}

sevenPointFour() {
	// the code below will cause a runtime error.
	//  int *p;
	//	scanf("%d", p);

	// the code below is right to invoke scanf();
	int i;
	scanf("%d", &i);
	printf("%d\n", i);

	double sum, v;
	sum = 0;

	printf("input double data continuously, ctrl-d to stop:\n");
	//lf means &v is a double not float. f means &v is a float.
	// because of double v here. so lf do make sense.
	while (scanf("%lf", &v) == 1)
		printf("\t%.2f\n", sum += v);

	// 25 Dec 1988 相应的scanf语句可以这样编写：
	int day, year;
	char monthname[20];
	printf("please input 25 Dec 1988:\n");
	scanf("%d %s %d", &day, monthname, &year);
	printf("you have inputted %d %s %d\n", day, monthname, year);

	int day2, month2, year2;
	printf("please input 12/25/1988:\n");
	scanf("%d/%d/%d", &month2, &day2, &year2);
	printf("you have inputted %d %d %d\n", month2, day2, year2);

	/* scanf函数忽略格式串中的空格和制表符。此外，在读取输入值时，它将跳过空白符（空
	 * 格、制表符、换行符等等）。如果要读取格式不固定的输入，最好每次读入一行，然后再用
	 * sscanf将合适的格式分离出来读入。
	 */

	//	while (getline(line, sizeof(line)) > 0) {
	//		if (sscanf(line, "%d %s %d", &day, monthname, &year) == 3)
	//			printf("valid: %s\n", line); /* 25 Dec 1988 form */
	//		else if (sscanf(line, "%d/%d/%d", &month, &day, &year) == 3)
	//			printf("valid: %s\n", line); /* mm/dd/yy form */
	//		else
	//			printf("invalid: %s\n", line); /* invalid form */
	//	}

}
