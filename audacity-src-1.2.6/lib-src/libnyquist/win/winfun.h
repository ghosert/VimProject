#ifdef __cplusplus 
extern "C" {
#endif

char *getfilename(char *deflt, char *extension, char *mode, char *prompt);
FILE *fileopen(char *deflt, char *extension, char *mode, char *prompt);

#ifdef __cplusplus 
}
#endif
