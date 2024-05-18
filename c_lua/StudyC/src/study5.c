#include <stdio.h>

int main(int argc, char *argv[]) {
	fivePointOne();
	fivePointTwo();
	fivePointThree();
	fivePointFour();
	fivePointFive();
	fivePointSix();
	fivePointSeven();
	fivePointEleven();
	
	printf("The number of arg is: %d\n", argc);
	printf("And they are: ");
	int i = 0;
	for (; i < argc; i++) {
		printf("%s ", argv[i]);
	}
	printf("\n");
	printf("And they are: ");
	while (argc--) {
		printf("%s ", *argv++); // caculate *argv first then argv++
	}
	printf("\n");
	argv--; // back pace to the last char *argv
	char c;
	while (c = *argv[0]++) { // caculate argv[0], then *argv[0], then argv[0]++  the same as the definition *(*argv)++
		                     // *++argv[0] is also right definition. caculate argv[0], then ++argv[0], then *..
		                     // base on the case above, we should know [] has a higher priority to caculate.
		printf("%c ", c); // print all the char in last char *argv.
	}
	*argv[0]--; // back pace to '\0'
	*argv[0]--; // back pace to the last char in char *argv
	printf("%c", (*argv++)[0]); // make sure the difference from *argv[0]++.
	                            // *argv[0]++ will begin to scan the chars in argv[0]
	                            // but (*argv++)[0] means get the current char and point to next pointer array.
	                            // (*++argv)[0] means point to next pointer array and then get the first char.
	argv--;
	printf("%c", *argv++[0]); // make sure the difference from above and *argv[0]++
	                          // caculate argv++, then argv++[], then *argv++[]
	                          // as usual, the [] has a higher priority to caculate, compared with signal *.
}

fivePointOne() {
	int x = 1, y = 2, z[10];
	int *ip; /* ip is a pointer to int */
	ip = &x; /* ip now points to x */
	y = *ip; /* y is now 1 */
	*ip = 0; /* x is now 0 */
	*ip = *ip + 1;
	*ip += 1;
	++*ip;
	(*ip)++; /* It could not be *ip++ */
	int *iq;
	iq = ip;
	ip = &z[0]; /* ip now points to z[0] */
}

fivePointTwo() {
	int i = 0;
	int j = 1;
	printf("Before exchange: i = %d, j = %d\n", i, j);
	swap(&i, &j);
	printf("After exchange: i = %d, j = %d\n", i, j);
	// pass the address is the only way to change the value of variable from invoker.
	changeIt(&i);
	printf("After exchange i to 2008: i = %d\n", i);
}

swap(int *i, int *j) {
	int temp;
	temp = *i;
	*i = *j;
	*j = temp;
}

changeIt(int *p) {
	*p = 2008;
}

fivePointThree() {
	int a[] = { 1, 2, 3, 4, 5, 6, 7 };
	int *p;
	p = &a[0]; // this equals p = a; a also means a pointer. a = &a[0];
	int x = *p;
	printf("the first number of array: %d\n", x); // p means the pointer point to the first number of array.
	printf("the second number of array: %d\n", *(p + 1)); // this means *(p + 1) equals a[1]
	printf("the third number of array: %d\n", *(a + 2)); // a means a pointer just like p. a + i = &a[i];
	// conclusion.
	// int *p = &a[0] = a;
	// *(p + i) = a[i] = *(a + i);
	// p + i = &a[i] = a + i;
	printf("the length of \"jiawei\" is %d\n", strlen("jiawei")); // pass the first address of array to strlen
}

strlen(char *s) { // equals to strlen(char s[]) {
	int n = 0;
	while (*(s++) != '\0')
		n++;
	return n;
}

fivePointFour() {
	printf("the length of \"jiawei\" is %d\n", strlen2("jiawei"));
}

strlen2(char *s) { // two pointer allow to minus.
	char *p = s;
	while (*(s++) != '\0')
		;
	return s - p - 1;
}

fivePointFive() {
	/* s is an array while p is a point, the case below show the difference. 
	 * The same thing for them is both them can visit value by adding steps.*/

	char s[] = "i love you"; /* s can not be refered to another address, but string content can be modified.*/
	// s = "iii"; //this will cause a compile error, for s is not a pointer.
	*(s + 1) = 'i'; // this is well, because for an array, the value can be modified.
	printf("%s %c\n", s, *s);

	char *p = "i love you"; /* p can be refered to another address, but string content can not be modified. */
	p = "you love me"; // this is well, because p is a point, not a name of array.
	// *(p + 1) = 'u'; // this will cause a runtime error, for p can not modify the value.
	printf("%s %c\n", p, *p);

	// char *p = "you love me"; p will be pointed to an anonymous string, so it can't modify the value.

	// s = p; s++; //this will cause a compile error, for s is not a pointer.

	p = s; /* p now refered to an array and can be used just like s to modify the string content.*/
	*(p + 1) = ' ';
	printf("%s\n", p);

	// char *ss = "123"; This will cause a runtime error, for ss should be an array which its value is allowed to be modified.
	char ss[10] = "123";
	char *tt = "456"; // Here, use pointer is ok, for we just visit the value, no modification.
	printf("%s compared with %s: %d\n", ss, tt, strcmp(ss, tt));
	strcpy(ss, tt);
	strcpy1(ss, tt);
	strcpy2(ss, tt);
	strcpy3(ss, tt);
	strcpy4(ss, tt);
	printf("copy tt to ss get %s\n", ss);
	printf("%s compared with %s: %d\n", ss, tt, strcmp2(ss, tt));
}

/* strcpy: copy t to s; array subscript version */
strcpy(char *s, char *t) {
	int i;
	i = 0;
	while ((s[i] = t[i]) != '\0')
		i++;
}

/* strcpy: copy t to s; pointer version */
strcpy1(char *s, char *t) {
	int i;
	i = 0;
	while ((*s = *t) != '\0') {
		s++;
		t++;
	}
}

/* strcpy: copy t to s; pointer version 2 */
strcpy2(char *s, char *t) {
	while ((*s++ = *t++) != '\0')
		;
}

/* strcpy: copy t to s; pointer version 3 */
strcpy3(char *s, char *t) {
	while (*s++ = *t++)
		// because '\0' equals 0;
		;
}

/* strcpy: copy t to s; pointer version 4 */
strcpy4(char s[], char t[]) { // It seems this is not ok, but it works actually. becasue char s[] here is 
	// pointer not array, so it can ++ himself.(notice: array can not ++ himself.)
	// And whatever s t is array or pointer in the invoker function, it now become pointer in this function.
	// If the pointer is pointed to another array or variable before,
	// the value in this function can be modified, otherwise, can not.
	while (*s++ = *t++)
		// because '\0' equals 0;
		;
	// also pointer s here point to a array named ss[10] from its invoker, so 
	// this pointer can also modify the value.
	*--s = '+';
}

/* strcmp: return <0 if s<t, 0 if s==t, >0 if s>t */
int strcmp(char *s, char *t) {
	int i;
	for (i = 0; s[i] == t[i]; i++)
		if (s[i] == '\0')
			return 0;
	return s[i] - t[i];
}

/* strcmp: return <0 if s<t, 0 if s==t, >0 if s>t */
int strcmp2(char *s, char *t) {
	for (; *s == *t; s++, t++)
		if (*s == '\0')
			return 0;
	return *s - *t;
}

fivePointSix() {
	char *plines[] = { "123", "456", "789" }; // define a pointer array.
	int i = 3;
	writeLines(plines, i);
	/* The code below is wrong, for plines here is array, not pointer, it can not ++ itself.
	 * writeLines(char *plines[], int i) is ok, plines here is pointer, it can ++ itself.
	 while (i-- > 0)
	 printf("%s\n", *(plines++));
	 */
}

writeLines(char *plines[], int i) { // char **plines is also a correct definition.
	printf("%c ", plines[0][0]); // This does not mean we are using a two-dimension array
	printf("%c ", **plines);
	while (i-- > 0)
		printf("%s\n", *(plines++)); // print all the strings.
}

fivePointSeven() {
	int days = day_of_year(1980, 11, 14);
	printf("The days passed from 1980/01/01 to 1980/11/14: %d\n", days);
	int month;
	int day;
	month_day(1980, days, &month, &day);
	printf("The date is 1980/%d/%d after %d days passed since 1980/01/01\n", month, day, days);
}

// it also can be defined as static char daytab[][13] to ignore the line number.
// but static char daytab[][] is not allowed.
static char daytab[2][13] = { { 0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30,
		31 }, { 0, 31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 } };

/* day_of_year: set day of year from month & day */
int day_of_year(int year, int month, int day) {
	int i, leap;
	leap = year%4 == 0 && year%100 != 0 || year%400 == 0;
	for (i = 1; i < month; i++)
		day += daytab[leap][i];
	return day;
}

/* month_day: set month, day from day of year */
month_day(int year, int yearday, int *pmonth, int *pday) {
	int i, leap;
	leap = year%4 == 0 && year%100 != 0 || year%400 == 0;
	for (i = 1; yearday > daytab[leap][i]; i++)
		yearday -= daytab[leap][i];
	*pmonth = i;
	*pday = yearday;
}

int callback(char *, char *);
void invoker(int (*)(char *, char *), char *, char *);
// void invoker(int (*fun)(char *, char *), char *, char *); // this is also ok.

fivePointEleven() {
	// this is ok.
	// invoker((int (*)(char *, char *))callback, "you are pig.", "I am not a pig.");
	
	// this is also ok and simpler for the invoker to invoke the function.
	invoker(callback, "you are pig.", "I am not a pig.");
}

// Just like callback or template pattern in java, 
// this callback part will be offered to the third-part to implement.
int callback(char *s, char *t) {
	printf("\n%s\n", s);
	printf("%s\n\n", t);
	return 1;
}

// this invoker is most likely hidden in the api providers.
// for the third-part programmer, just implement callback function and know when to invoke is enough.
void invoker(int (*fun)(char *, char *), char *s, char *t) {
	// invoke callback(char *, char *)
	(*fun)(s, t);
}
