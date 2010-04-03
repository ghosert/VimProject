#include <stdio.h>

#define NPERLINE	10

main()
{
    short line[2][NPERLINE];
    register int n, i, j;
    register int curline = 0;
    int currently_printing = 1;

    for(i = 0;
       (n = fread(line[curline], sizeof(short), NPERLINE, stdin)) > 0;
       i += n, curline = 1-curline) {
        if(i != 0 && n == NPERLINE && sameline(line[0], line[1])) {
            if(currently_printing) {
                printf("*\n");
                currently_printing = 0;
            }
            continue;
        }
        currently_printing = 1;
        printf("%7d ", i);
        for(j = 0; j < n; j++)
            printf("%6d ", line[curline][j]);
        printf("\n");
    }
    printf("%4d\n", i);
}

sameline(l1, l2)
register short *l1, *l2;
{
    register n = NPERLINE;
    while(--n >= 0)
        if(*l1++ != *l2++)
            return 0;
    return 1;
}
