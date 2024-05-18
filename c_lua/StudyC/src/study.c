/*
 ============================================================================
 Name        : study.c
 Author      : JiaweiZhang
 Version     :
 Copyright   : Your copyright notice
 Description : Hello World in C, Ansi-style
 ============================================================================
 */

#include <stdio.h>

/*
 * Ctrl c: both windows, linux means close the program.
 * Ctrl d in linux equals Ctrl z in windows means input a EOF.
 * Ctrl z in linux means switch the current task to background.
 */

/* if there is no type defined for the return value, the default type is int
 * Here, man() equals to int main(), not void main() and unlike java, in java,
 * there must be a return type defined or the compiler will throw the error.*/
main() {
	onePointOne();
	onePointTwo();
	onePointThree();
	onePointFour();
	onePointFive();
	onePointSix();
	onePointSeven();
	onePointEight();
	onePointNine();
	onePointTen();
}

onePointOne() {
	printf("hello, world\n");
	printf("hello, ");
	printf("world");
	printf("\n");
	printf("hello, world\t");
	printf("hello, world\b");
	printf("hello, world\\");
	printf("hello, world\c");
	printf("\n");
}

onePointTwo() {
	/* ��fahr = 0, 20, ..., 300
	 ��ӡ�����¶��������¶ȶ��ձ�*/
	int fahr, celsius;
	int lower, upper, step;
	lower = 0; /* �¶ȱ������*/
	upper = 300; /* �¶ȱ������*/
	step = 20; /* ����*/
	fahr = lower;
	while (fahr <= upper) {
		celsius = 5 * (fahr - 32) / 9;
		printf("%3d\t%6d\n", fahr, celsius);
		fahr = fahr + step;
	}

	printf("\nBelow is the float version.\n\n");

	float fahrFloat, celsiusFloat;
	lower = 0; /* �¶ȱ������*/
	upper = 300; /* �¶ȱ������*/
	step = 20; /* ����*/
	fahrFloat = lower;
	while (fahrFloat <= upper) {
		celsiusFloat = 5.0 / 9.0 * (fahrFloat - 32);
		printf("%3.0f\t%6.1f\n", fahrFloat, celsiusFloat);
		fahrFloat = fahrFloat + step;
	}

	printf(
			"\n 1)Eight format:%o 2)Sixteen format:%x 3)char:%c 4)string:%s 5)%%itself:%%\n\n",
			010, 0x10, 'c', "string");

}

onePointThree() {
	int fahr;
	for (fahr = 0; fahr <= 300; fahr = fahr + 20)
		printf("%3d %6.1f\n", fahr, (5.0 / 9.0) * (fahr - 32 ));
}

#define LOWER 0
#define UPPER 300
#define STEP 20

onePointFour() {
	int fahr;
	for (fahr = LOWER; fahr <= UPPER; fahr = fahr + STEP)
		printf("%3d %6.1f\n", fahr, (5.0 / 9.0) * (fahr - 32));
}

#define IN 1 /* �ڵ�����*/
#define OUT 0 /* �ڵ�����*/

/*  This part should be tested in the real console mode. */
onePointFive() {
	/* input characters and then output them. */
	/* account the number of the characters and lines inputed. Press the ctrl+Z to stop the program. */
	int c;
	long lnc = 0; // number of characters with long type
	double dnc = 0; // number of characters with int type
	int inl = 0; // number of lines
	int nw = 0; // number of words
	int state= OUT;
	while ((c = getchar()) != EOF) {
		putchar(c);
		++lnc;
		++dnc;
		if (c == '\n')
			++inl;
		if (c == ' ' ||c == '\n' || c == '\t')
			state = OUT;
		else if (state == OUT) {
			state = IN;
			++nw;
		}
	}
	printf(
			"The total number of inputted characters is: %ld %.0f The number of line is: %d. The number of words is: %d\n",
			lnc, dnc, inl, nw);
}

onePointSix() {
	/* ͳ�Ƹ������֡��հ��ַ��������ַ��ֱ���ֵĴ��� */
	int c, i, nwhite, nother;
	// int ndigit[3]; Right, but ndigit should be initialized later before it is used.
	// int ndigit[3] = {0, 0, 0}; Right.
	// int ndigit[] = {0, 0, 0}; Right.
	// int ndigit[]; Wrong.
	// int ndigit[3] = {1}; Right. ndigit[0] = 1; ndigit[1] = 0; ndigit[2] = 0;
	// char string[] = "i love you"; equals char string[] = {'i', .. 'u', '\0'};Right.
	// char string[100] = "i love you"; Right. string[9] = u, string[10-99] = ''.
	int ndigit[10];
	nwhite = nother = 0;
	for (i =0; i < 10; ++i)
		ndigit[i] = 0;
	while ( (c = getchar() ) != EOF)
		if (c >= '0' && c <= '9')
			++ndigit[c - '0'];
		else if (c == ' ' ||c == '\n' || c == '\t')
			++nwhite;
		else
			++nother;
	printf("digits =");
	for (i =0; i < 10; ++i)
		printf(" %d", ndigit[i]);
	printf(", white space = %d, other = %d\n", nwhite, nother);
}

int power(int m, int n);

onePointSeven() {
	int i;
	for (i = 0; i < 10; i++) {
		printf("%d, %d, %d \n", i, power(2, i), power(-3, i));
	}
}

int power(int base, int n) {
	int i;
	int value = 1;
	for (i = 0; i < n; i++) {
		value = value * base;
	}
	return value;
}

onePointEight() {
	int i;
	for (i = 0; i < 10; i++) {
		printf("%d, %d, %d \n", i, power2(2, i), power2(-3, i));
	}
}

int power2(int base, int n) {
	int value = 1;
	for (; n > 0; n--) {
		value = value * base;
	}
	return value;
}

#define MAXLINE 1000 /* ��������еĴ�С*/
int getline(char line[ ], int maxline);
void copy(char to[ ], char from []);

onePointNine() {
	int len; /* ��ǰ�г���*/
	int max; /* ��ĿǰΪֹ�����ֵ���еĳ���*/
	char line[MAXLINE]; /* ��ǰ�������*/
	char longest[MAXLINE]; /* ���ڱ��������*/
	max = 0;
	while ( (len = getline(line, MAXLINE) ) > 0)
		if (len > max) {
			max = len;
			copy(longest, line);
		}
	if (max > 0) /* ��һ��*/
		printf("%s", longest) ;
	return 0;
}

/* getline����һ�ж���s�в������䳤��*/
int getline(char s [], int lim) {
	int c, i;
	for (i = 0; i < lim -1 && (c = getchar()) != EOF && c != '\n'; ++i)
		s[i] = c;
	if (c == '\n') {
		s[i] = c;
		++i;
	}
	s[i] = '\0';
	return i;
}
/* copy����from������to; �ٶ�to�㹻��*/
void copy(char to [ ], char from [ ]) {
	int i;
	i = 0;
	while ( (to[ i ] = from [ i ]) != '\0')
		++i;
}

#define MAXLINE2 1000 /* ��������еĴ�С*/
int max2; /* ��ĿǰΪֹ�����ֵ���еĳ���*/
char line2[MAXLINE]; /* ��ǰ�������*/
char longest2[MAXLINE]; /* ���ڱ��������*/
int getline2(void);
void copy2(void);

/* ����ⲿ���������ڵ��ú���֮ǰ�����ú��е�extern�ؼ��־Ϳ���ʡ��, ���º����ڵ�extern������ʡ��
 * ��������������Դ�ļ���ĳ��������f i l e 1�ļ��ж��塢��f i l e 2��f i l e 3�ļ���ʹ�ã���ô��
 * f i l e 2��f i l e 3�ļ��о���Ҫʹ��e x t e r n˵�������Ӹñ����ĳ��֡�����ͨ���ѱ�����e x t e r n˵����
 * ��������һ���������ļ��У���ʷ�Ͻ���ͷ�ļ�������ÿһ��Դ�ļ���ǰ����# i n c l u d e������
 * Ҫ�õ�ͷ�ļ�������������׺.h ��Լ��Ϊͷ�ļ�������չ�� */

onePointTen() {

	/* ��ӡ��������У� �ر�汾*/
	int len;
	extern int max2;
	extern char longest2[ ];
	max2 = 0;
	while ( (len = getline2() ) > 0)
		if (len > max2) {
			max2 = len;
			copy2();
		}
	if (max2 > 0) /* ��һ��*/
		printf("%s", longest2) ;
	return 0;
}

/* getline���ر�汾*/
int getline2(void) {
	int c, i;
	extern char line2[ ];
	for (i = 0; i < MAXLINE2 -1 && (c = getchar() ) != EOF && c != '\n'; ++i)
		line2[i] = c;
	if (c == '\n') {
		line2[i] = c;
		++i;
	}
	line2[i] = '\0';
	return i;
}
/* copy���ر�汾*/
void copy2(void) {
	int i;
	extern char line2[ ], longest2[ ];
	i = 0;
	while ( (longest2[ i ] = line2[ i ]) != '\0')
		++i;
}