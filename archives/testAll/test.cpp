#include <stdio.h>
#include <windows.h>

typedef struct _TestStruct
{
    int i;
    char *string;
} TestStruct;

int main(int argc, char *argv[])
{
    OSVERSIONINFO osversioninfo;
	osversioninfo.dwOSVersionInfoSize = sizeof(OSVERSIONINFO);
	BOOL isSuccess = GetVersionEx(&osversioninfo);
	if (isSuccess) {
		printf("Major Version:%d\n", osversioninfo.dwMajorVersion);
		printf("Minor Version:%d\n", osversioninfo.dwMinorVersion);
	} else {
		printf("Invoking failed.");
	}
	getchar();
	return isSuccess == TRUE ? 0 : 1;
}

