#include <stdio.h>
#include <locale.h>

int main(void) {
	
	char a = 'a';
	printf("char a sizeof %d\n", sizeof(a));
	
	wchar_t wa = 'a'; // equals to wchar_t wa = L'a';
	printf("wchar_t wa sizeof %d\n", sizeof(wa));
	
	// make sure that the console will show the chinese character normally.
	setlocale(LC_CTYPE, ".936");
	
	// if the file is not utf8 format, use gcc ... -finput-charset=gb2312 to compile
	// or here will be a error when begin to compile because of L
	// L means the following character will occupy two bytes here.
	wchar_t p = L'Äã'; 
	                   
	wprintf(L"%c\n", p);
	printf("wchar_t p %d\n", sizeof(p));
	
	// if the file is not utf8 format, use gcc ... -finput-charset=gb2312 to compile
	// or here will be a error when begin to compile becasue of L
	// L means each the following character will occupy two bytes here.
	wchar_t pp[] = L"ÄãºÃ";
	wprintf(L"%s\n", pp);
	printf("length of wchar_t pp %d\n", wcslen(pp)); // you can't use strlen here to the wchar_t type
	printf("size of wchar_t pp %d\n", sizeof(pp));	
	
	printf("strlen(\"Hello\") %d\n", strlen("Hello"));
	printf("strlen(L\"Hello\")%d\n", strlen(L"Hello"));
	printf("wcslen(L\"Hello\")%d\n", wcslen(L"Hello"));
	
	exit(0);
}
