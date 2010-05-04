#include <stdio.h>

int main(void) {
	sixPointOne();
	sixPointTwo();
	sixPointThree();
	sixPointFive();
	sixPointSeven();
	sixPointEight();
}

struct point {
	int x;
	int y;
}; // equals to int, define a new date type, but not use it to declare a variable;

sixPointOne() {
	struct point {
		int x;
		int y;
	} point1, point2, point3; // equals to point point1, point2, point3
	// define a new data type, and then use it to declare point1, point2, point3

	struct {
		int x;
		int y;
	} point4; // the point data type can be ignored and declare a variable directly.

	struct point pt = { 300, 400 }; // initialize.

	printf("%d, %d, ", pt.x, pt.y);

	double dist, sqrt(double);
	dist = sqrt((double)pt.x * pt.x + (double)pt.y * pt.y);
	printf("%g\n", dist);

	struct rect {
		struct point pt1;
		struct point pt2;
	};

	struct rect screen = { { 100, 200 }, { 300, 400 } };

	printf("%d, %d, ", screen.pt1.x, screen.pt1.y);
	printf("%d, %d\n", screen.pt2.x, screen.pt2.y);
}

sixPointTwo() {
	struct point makepoint(int x, int y);
	struct point pt = makepoint(1, 2);
	printf("%d, %d\n", pt.x, pt.y);
	struct point pt2 = makepoint(2, 1);
	struct point addpoint(struct point, struct point);
	struct point pt3 = addpoint(pt, pt2);
	printf("%d, %d\n", pt.x, pt.y); // this proved that struct type is value passed.
	printf("%d, %d\n", pt3.x, pt3.y);

	struct point origin, *pp;
	pp = &origin;
	origin.x = 1980;
	origin.y = 1114;
	printf("origin is %d, %d\n", (*pp).x, (*pp).y); // this sentence is the same to the one below.
	// because . is priority to * so () is necessary here.
	printf("origin is %d, %d\n", pp->x, pp->y); // use -> on pointer pp while use . on non-pointer origin 

	// 在所有运算符中，下面4 个运算符的优先级最高：结构运算符“.”和“->”、用于函数调用的“()”以及用于下标的“[]”
	// 因此，它们同操作数之间的结合也最紧密
	struct {
		int len;
		char *str;
	}*p, str;

	p = &str; // This sentence can not be taken out, or an error will be thrown when running.
	p->len = 0;
	++p->len; // 将增加len的值，而不是增加p 的值，
	printf("%d\n", p->len);

	p->str = "jiawei";
	*p->str++; // caculate p->str then *p->str then p->str++
	printf("%s\n", p->str); // this will print "iawei"
	printf("%c\n", (*p->str) + 1); // equals to *(p->str). will print 'i' + 1 = 'j' equals to str[0] + 1;

	printf("%d\n", p);
	printf("%c\n", *p++->str); // this will print 'i', and p will be added 1;
	printf("%d\n", p);
}

struct point makepoint(int x, int y) {
	struct point pt;
	pt.x = x;
	pt.y = y;
	return pt;
}

// this proved that struct type is value passed.
struct point addpoint(struct point pt1, struct point pt2) {
	pt1.x += pt2.x;
	pt1.y += pt2.y;
	return pt1;
}

sixPointThree() {

	struct key1 {
		char *word;
		int count;
	}
			keytab1[] = { { "auto", 0 }, { "break", 0 }, { "case", 0 }, {
					"char", 0 }, { "const", 0 }, { "continue", 0 }, {
					"default", 0 }, { "unsigned", 0 }, { "void", 0 }, {
					"volatile", 0 }, { "while", 0 } };

	// or the definition below is also right.

	struct key {
		char *word;
		int count;
	} keytab[] = { "auto", 0, "break", 0, "case", 0, "char", 0, "const", 0,
			"continue", 0, "default", 0, "unsigned", 0, "void", 0, "volatile",
			0, "while", 0 }, *p;

	int numbers = sizeof(keytab) / sizeof(struct key);
	printf("%d\n", numbers);
	numbers = sizeof(keytab) / sizeof(keytab[0]);
	printf("%d\n", numbers);

	printf("%s, %d\n", (*keytab).word, (*keytab).count);
	printf("%s, %d\n", keytab->word, keytab->count);

	p = keytab;
	printf("%s, %d\n", p->word, p->count);
}

#define MAXWORD 100

struct tnode { /* the tree node: */
	char *word; /* points to the text */
	int count; /* number of occurrence */
	struct tnode *left; /* left child */
	struct tnode *right; /* right child */
};

struct tnode *addtree(struct tnode *, char *);
void treeprint(struct tnode *);
int getword(char *, int);

sixPointFive() {

	struct t {
		struct s *p; /* p points to an s */
	};

	struct s {
		struct t *q; /* q points to a t */
	};

	struct tnode *root;
	char word[MAXWORD];
	root = NULL;
	while (getword(word, MAXWORD) != EOF)
		if (isalpha(word[0]))
			root = addtree(root, word);
	treeprint(root);
	return 0;
}

struct tnode *talloc(void);
char *strdup(char *);

/* addtree: add a node with w, at or below p */
struct tnode *addtree(struct tnode *p, char *w) {
	int cond;
	if (p == NULL) { /* a new word has arrived */
		p = talloc(); /* make a new node */
		p->word = strdup(w);
		p->count = 1;
		p->left = p->right = NULL;
	} else if ((cond = strcmp(w, p->word)) == 0)
		p->count++; /* repeated word */
	else if (cond < 0) /* less than into left subtree */
		p->left = addtree(p->left, w);
	else
		/* greater than into right subtree */
		p->right = addtree(p->right, w);
	return p;
}

/* talloc: make a tnode */
struct tnode *talloc(void) {
	return (struct tnode *) malloc(sizeof(struct tnode));
}

char *strdup(char *s) /* make a duplicate of s */
{
	char *p;
	p = (char *) malloc(strlen(s)+1); /* +1 for '\0' */
	if (p != NULL)
		strcpy(p, s);
	return p;
}

/* getword: get next word or character from input */
int getword(char *word, int lim) {
	int c, getch(void);
	void ungetch(int);
	char *w = word;
	while (isspace(c = getch()))
		;
	if (c != EOF)
		*w++ = c;
	if (!isalpha(c)) {
		*w = '\0';
		return c;
	}
	for (; --lim > 0; w++)
		if (!isalnum(*w = getch())) {
			ungetch(*w);
			break;
		}
	*w = '\0';
	return word[0];
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

/* treeprint: in-order print of tree p */
void treeprint(struct tnode *p) {
	if (p != NULL) {
		treeprint(p->left);
		printf("%4d %s\n", p->count, p->word);
		treeprint(p->right);
	}
}

sixPointSeven() {
	typedef int Integer;
	Integer i = 1;
	Integer is[] = { 1, 2, 3, 4 };
	printf("%d, %d\n", i, *(is + 1));

	typedef char *String;
	String s = "jiaweinumberone";
	printf("%s\n", s);

	// declare a Treeptr pointer and a tnode struct.
	typedef struct tnode *Treeptr;
	typedef struct tnode { /* the tree node: */
		char *word; /* points to the text */
		int count; /* number of occurrences */
		struct tnode *left; /* left child */
		struct tnode *right; /* right child */
	} Treenode;

	Treenode node; // equals to struct tnode node;
	node.count = 0;
	printf("node.count: %d ", node.count);

	Treeptr ptr = &node; // equals to struct tnode *ptr = &node;
	ptr->count = 1;
	printf("node.count: %d ", node.count);
	printf("ptr->count: %d\n", ptr->count);

}

sixPointEight() {
	union u_tag {
		int ival;
		float fval;
		char *sval;
	} u;

	// I have no idea about how to run the code below rightly.
	//	if (utype == INT)
	//		printf("%d\n", u.ival);
	//	if (utype == FLOAT)
	//		printf("%f\n", u.fval);
	//	if (utype == STRING)
	//		printf("%s\n", u.sval);
	//	else
	//		printf("bad type %d in utype\n", utype);
	struct {
		char *name;
		int flags;
		int utype;
		union {
			int ival;
			float fval;
			char *sval;
		} u;
	} symtab[100];
	// 可以通过下列语句引用其成员ival：
	// symtab[i].u.ival;
	// 也可以通过下列语句之一引用字符串sval的第一个字符：
	// *symtab[i].u.sval;
	// symtab[i].u.sval[0];
}
