#include <stdio.h>
#include <string.h>

int main() {
	threePointThree();
	threePointFour();
	threePointFive();
	threePointSix();
	threePointSeven();
	threePointEight();
}

threePointThree() {
	int x = 3;
	int v[] = { 1, 2, 3, 4, 5 };
	int n = sizeof(v) / sizeof(int);
	printf("binSearch: %d\n", binSearch(x, v, n));
}

/* binary search. */
int binSearch(int x, int v[], int n) {
	int low = 0;
	int high = n - 1;
	while (low <= high) {
		int mid = (low + high) / 2;
		if (x < v[mid]) {
			high = mid + 1;
		} else if (x > v[mid]) {
			low = mid + 1;
		} else {
			return mid;
		}
	}
	return -1; /* no match. */
}

threePointFour() {
	int c, i, nwhite, nother, ndigit[10];
	nwhite = nother = 0;
	for (i = 0; i < 10; i++)
		ndigit[i] = 0;
	while ((c = getchar()) != EOF) {
		switch (c) {
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			ndigit[c-'0']++;
			break;
		case ' ':
		case '\n':
		case '\t':
			nwhite++;
			break;
		default:
			nother++;
			break;
		}
	}
	printf("digits =");
	for (i = 0; i < 10; i++)
		printf(" %d", ndigit[i]);
	printf(", white space = %d, other = %d\n", nwhite, nother);
}

threePointFive() {
	char s[] = ".uoy evol I";
	reverse(s);
	printf("%s\n", s);
}

reverse(char s[]) {
	int i;
	int j;
	char temp;
	for (i = 0, j = strlen(s) - 1; i < j; i++, j--) {
		temp = s[j];
		s[j] = s[i];
		s[i] = temp;
	}
}

threePointSix() {
	char s[100];
	int n = 12345;
	itoa(n, s);
	printf("%s\n", s);
}

/* itoa: convert n to characters in s */
itoa(int n, char s[]) {
	int i, sign;
	if ((sign = n) < 0) /* record sign */
		n = -n; /* make n positive */
	i = 0;
	do { /* generate digits in reverse order */
		s[i++] = n % 10 + '0'; /* get next digit */
	} while ((n /= 10) > 0); /* delete it */
	if (sign < 0)
		s[i++] = '-';
	s[i] = '\0';
	reverse(s);
}

threePointSeven() {
	char s[] = "abcde   ";
	trim(s);
	printf("%s\n", s);
}

/* trim: remove trailing blanks, tabs, newlines */
int trim(char s[]) {
	int n;
	for (n = strlen(s)-1; n >= 0; n--)
		if (s[n] != ' ' && s[n] != '\t' && s[n] != '\n')
			break;
	s[n+1] = '\0';
	return n;
}

threePointEight() {
	/* goto keyword should be used only to break several circles,
	 * just like break: in java.
	 * Also goto keyword can be insteaded of other sentences.
	 * Here we give two ways to implement the same function.*/
	
	// one way.
	int a[] = { 1, 3, 5, 7, 9 };
	int b[] = { 2, 4, 3, 8, 10 };
	int i, j, m, n;
	m = n = 5;
	for (i = 0; i < n; i++)
		for (j = 0; j < m; j++)
			if (a[i] == b[j])
				goto found;
	printf("Didn't find any common element\n");
	found: printf("got one a[%d] == b[%d]\n", i, j);

	// the other way.
	int found = 0;
	for (i = 0; i < n && !found; i++)
		for (j = 0; j < m && !found; j++)
			if (a[i] == b[j])
				found = 1;
	if (found)
		printf("got one a[%d] == b[%d]\n", i - 1, j - 1);
	else
		printf("Didn't find any common element\n");
}
