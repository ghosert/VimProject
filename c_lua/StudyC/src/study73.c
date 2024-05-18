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
//	strcat(s, t) ��tָ����ַ������ӵ�sָ����ַ�����ĩβ
//	strncat(s, t, n) ��tָ����ַ�����ǰn���ַ����ӵ�sָ����ַ�����ĩβ
//	strcmp(s, t) ����s ָ����ַ���С�ڣ�s<t�������ڣ�s==t������ڣ�s>t��t
//	ָ����ַ����Ĳ�ͬ������ֱ𷵻ظ�������0��������
//	strncmp(s, t, n) ͬstrcmp��ͬ����ֻ��ǰn���ַ��бȽ�
//	strcpy(s, t) ��tָ����ַ������Ƶ�sָ���λ��
//	strncpy(s, t, n) ��tָ����ַ�����ǰn���ַ����Ƶ�sָ���λ��
//	strlen(s) ����sָ����ַ����ĳ���
//	strchr(s, c) ��sָ����ַ����в���c�����ҵ����򷵻�ָ������һ�γ��ֵ�λ
//	�õ�ָ�룬���򷵻�NULL
//	strrchr(s, c) ��sָ����ַ����в���c�����ҵ����򷵻�ָ�������һ�γ��ֵ�
//	λ�õ�ָ�룬���򷵻�NULL
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
//	isalpha(c) ��c����ĸ���򷵻�һ����0 ֵ�����򷵻�0
//	isupper(c) ��c�Ǵ�д��ĸ���򷵻�һ����0ֵ�����򷵻�0
//	islower(c) ��c��Сд��ĸ���򷵻�һ����0ֵ�����򷵻�0
//	isdigit(c) ��c�����֣��򷵻�һ����0 ֵ�����򷵻�0
//	isalnum(c) ��isalpha(c)��isdigit(c)���򷵻�һ����0ֵ�����򷵻�0
//	isspace(c) ��c�ǿո񡢺����Ʊ�������з����س�������ҳ���������Ʊ����
//	�򷵻�һ����0 ֵ
//	toupper(c) ����c�Ĵ�д��ʽ
//	tolower(c) ����c��Сд��ʽ
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
    
//    sin(x) x �����Һ���������x �û��ȱ�ʾ
//    cos(x) x �����Һ���������x �û��ȱ�ʾ
//    atan2(y, x) y/x �ķ����к��������У�x ��y�û��ȱ�ʾ
//    exp(x) ָ������ex
//    log(x) x ����Ȼ��������eΪ�ף������У�x>0
//    log10(x) x �ĳ��ö�������10Ϊ�ף������У�x>0
//    pow(x, y) ����xy��ֵ
//    sqrt(x) x ��ƽ������x��0��
//    fabs(x) x �ľ���ֵ
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