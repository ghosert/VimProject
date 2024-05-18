#include <stdio.h>

int main() {
	twoPointThree();
	twoPointFour();
	twoPointSeven();
	twoPointEight();
	twoPointNine();
	twoPointEleven();
}

twoPointThree() {
	printf("The length of the string \"You are pig.\": %d\n",
			strlen("You are pig."));
	enum month {SUN, MON, TUE, WED, THU, FRI, SAT};
	printf("%d, %d, %d, %d, %d, %d, %d\n", SUN, MON, TUE, WED, THU, FRI, SAT);
}

int strlen(char string[]) {
	int i = 0;
	while (string[i] != '\0') {
		i++;
	}
	return i;
}

twoPointFour() {
	// We can't modify a const variable, or a compile error will be happened.
	const int i = 100;
	printf("%d\n", i);
}

twoPointSeven() {
	printf("convert \"123456\" to %d\n", atoi("123456"));
	char string[] = "ABCDEFG";
	lower(string);
	printf("convert \"ABCDEFG\" to %s\n", string);
	printf("The size of char is: %dbit, in java, it is 16bit\n", sizeof(char) * 8);
	printf("The value of char can be from -128 to 127(signed char), or from 0 to 255(unsigned char), in java it is from 0 to 66635.\n");
}

atoi(char s[]) {
	int i, n;
	n = 0;
	for (i = 0; s[i] >= '0' && s[i] <= '9'; ++i)
		n = 10 * n + (s[i] - '0');
	return n;
}

lower(char s[]) {
	int i;
	char c;
	for (i = 0; (c = s[i]) != '\0'; i++) {
		if (c >= 'A' && c <= 'Z') {
		    s[i] = c - 'A' + 'a';
		}
	}
}

twoPointEight() {
	char s[10] = "111";
	strcat(s, "222");
	printf("%s\n", s);
}

/* strcat: concatenate t to end of s; s must be big enough */
strcat(char s[], char t[]) {
	int i = 0;
	int j = 0;
	while (s[i] != '\0') {
		i++;
	}
	while ((s[i++] = t[j++]) != '\0') {
	}
}

twoPointNine() {
	printf("4 & 2 = %d\n", 4 & 2);
	printf("4 | 2 = %d\n", 4 | 2);
	printf("4 >> 2 = %d\n", 4 >> 2);
	printf("4 << 2 = %d\n", 4 << 2);
	printf("-4 >> 2 = %d\n", -4 >> 2);
	printf("-4 << 2 = %d\n", -4 << 2);
}

twoPointEleven() {
	2 > 1 ? printf("yes\n") : printf("no\n"); /* This is not allowed in java, but c is ok*/
	int a = 2 > 1 ? 2 : 1; /* This is the only way in java to use condition expression */
	printf("%d %d\n", a, 2 > 1 ? 2 : 1);
	
	int n = 2;
	printf("You have %d item%s.\n", n, n==1 ? "" : "s");
}