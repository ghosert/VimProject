#include <stdio.h>
#include <string.h>
#include <math.h>
// effect on function rand() srand()
#include <stdlib.h>

int main(void) {
	sevenPointEight();
}

sevenPointEight() {
	// <string.h>
//	strcat(s, t) 将t指向的字符串连接到s指向的字符串的末尾
//	strncat(s, t, n) 将t指向的字符串中前n个字符连接到s指向的字符串的末尾
//	strcmp(s, t) 根据s 指向的字符串小于（s<t）、等于（s==t）或大于（s>t）t
//	指向的字符串的不同情况，分别返回负整数、0或正整数
//	strncmp(s, t, n) 同strcmp相同，但只在前n个字符中比较
//	strcpy(s, t) 将t指向的字符串复制到s指向的位置
//	strncpy(s, t, n) 将t指向的字符串中前n个字符复制到s指向的位置
//	strlen(s) 返回s指向的字符串的长度
//	strchr(s, c) 在s指向的字符串中查找c，若找到，则返回指向它第一次出现的位
//	置的指针，否则返回NULL
//	strrchr(s, c) 在s指向的字符串中查找c，若找到，则返回指向它最后一次出现的
//	位置的指针，否则返回NULL
	char s[100] = "abc";
	char t[100] = "def";
	printf("s, t: %s %s\n", s, t);
	strcat(s, t);
	printf("strcat(s, t): %s\n", s);
	strncat(s, t, 2);
	printf("strncat(s, t, 2): %s\n", s);
	printf("strcmp(s, t): %d\n", strcmp(s, t));
	printf("strncmp(s, t, 2): %d\n", strncmp(s, t, 2));
	strcpy(s, t);
	printf("strcpy(s, t): %s\n", s);
	strncpy(s, "ab", 2);
	printf("strncpy(s, \"ab\", 2): %s\n", s);
	printf("strlen(s): %d\n", strlen(s));
	printf("strchr(s, \"ab\"): %s\n", strchr(s, 'a'));   // return NULL if failed.
	printf("strrchr(s, \"ab\"): %s\n", strrchr(s, 'b')); // return NULL if failed.
	
	// <ctype.h>
//	isalpha(c) 若c是字母，则返回一个非0 值，否则返回0
//	isupper(c) 若c是大写字母，则返回一个非0值，否则返回0
//	islower(c) 若c是小写字母，则返回一个非0值，否则返回0
//	isdigit(c) 若c是数字，则返回一个非0 值，否则返回0
//	isalnum(c) 若isalpha(c)或isdigit(c)，则返回一个非0值，否则返回0
//	isspace(c) 若c是空格、横向制表符、换行符、回车符，换页符或纵向制表符，
//	则返回一个非0 值
//	toupper(c) 返回c的大写形式
//	tolower(c) 返回c的小写形式
	char c = 'a';
	printf("%c\n", c);
	printf("isalpha(c): %d\n", isalpha(c));
	printf("isupper(c): %d\n", isupper(c));
	printf("islower(c): %d\n", islower(c));
	printf("isdigit(c): %d\n", isdigit(c));
	printf("isalnum(c): %d\n", isalnum(c));
	printf("isspace(c): %d\n", isspace(c));
	printf("toupper(c): %c\n", toupper(c));
	printf("tolower(c): %c\n", tolower(c));
	
	int getch(void);
	void ungetch(int);
	
	printf("print a character for getch(): \n");
	char d = getch();
	printf("ungetch() %c.\n", d);
	ungetch(d);
	printf("getch() again get %c\n", getch());
	
	// ungetc(); just like ungetch here, but just work for FILE *
	// ungetc() is matched with getc(); they both work for FILE *
	// and getchar() putchar() work for stdin & stdout
	// gets, puts work for stdin & stdout
	// fgets, fputs work for FILE *
	// getc, putc work for FILE *
	
	printf("system(\"date\"): %d\n", system("date")); // return $? value;
	
	int *p;
	p = (int *)malloc(sizeof(int)); // allocated a int size and with no initilization. return NULL if failed.
    printf("%d \n", *p);
	free(p);
	int n = 3;
	p = (int *)calloc(n, sizeof(int)); // n int space has been allocated, and initialized with 0; NULL if fail
	while (n--) {
	    printf("%d, %d \n", n, *p++);
	}
    // printf("%d, %d \n", n, *p); this will cause a runtime error.
	// And i don't know why if p does not back pace to original space, it will casue a runtime error.
	p--;
	p--;
	p--;
    free(p);
    
//    sin(x) x 的正弦函数，其中x 用弧度表示
//    cos(x) x 的余弦函数，其中x 用弧度表示
//    atan2(y, x) y/x 的反正切函数，其中，x 和y用弧度表示
//    exp(x) 指数函数ex
//    log(x) x 的自然对数（以e为底），其中，x>0
//    log10(x) x 的常用对数（以10为底），其中，x>0
//    pow(x, y) 计算xy的值
//    sqrt(x) x 的平方根（x≥0）
//    fabs(x) x 的绝对值
    printf("sin(3.14)=%f\n", sin(3.14));
    
    // srand() to set a seed number.
    //rand() return a rand number between 0 - RAND_MAX [exclude RAND_MAX]
    printf("produce five rand number between 0 - 1 [include 0, exclude 1]\n");
    double r =  ((double) rand() / (RAND_MAX+1.0));
    printf("%f ", r);
    r =  ((double) rand() / (RAND_MAX+1.0));
    printf("%f ", r);
    r =  ((double) rand() / (RAND_MAX+1.0));
    printf("%f ", r);
    r =  ((double) rand() / (RAND_MAX+1.0));
    printf("%f ", r);
    r =  ((double) rand() / (RAND_MAX+1.0));
    printf("%f ", r);
    
    exit(0);
}

#define BUFSIZE 100
char buf[BUFSIZE]; /* buffer for ungetch */
int bufp = 0; /* next free position in buf */
int getch(void) /* get a (possibly pushed-back) character */
{
	return (bufp > 0) ? buf[--bufp] : getchar();
}
void ungetch(int c) /* push character back on input */
{
	if (bufp >= BUFSIZE)
		printf("ungetch: too many characters\n");
	else
		buf[bufp++] = c;
}