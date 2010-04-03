/* sndmac.h -- system-specific definitions */

typedef double FASTFLOAT;
typedef float MEMFLOAT;

/* avoid conflicts if already defined: */

#ifndef max

#define min(a, b) ((a) < (b) ? (a) : (b))
#define max(a, b) ((a) > (b) ? (a) : (b))

#define ntohl(x) (x)
#define ntohs(x) (x)
#define htonl(x) (x)
#define htons(x) (x)

#endif
