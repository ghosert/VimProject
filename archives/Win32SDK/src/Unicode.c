#include <windows.h>

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iShowCmd) {
	
	TCHAR a = 'a';
// depended on _UNICODE means add UNICODE option in the gcc cmd.
	
// wchar_t equals to WCHAR, char equals to CHAR, TCHAR could be wchar_t or char, depended on _UNICODE signal.
// see the descriptions below:
	
//#define wchar_t WCHAR
//#typedef char CHAR
//#ifdef _UNICODE
//#typedef WCHAR TCHAR
//#else
//#typedef CHAR TCHAR
//#endif
	
	
//TEXT means the string will be with L or without L depended on _UNICODE signal.
//#ifdef _UNICODE
//#define TEXT(quote) L##quote
//#else
//#define TEXT(quote) quote
//#endif
	
// 8 bit data type in windows definition.
//typedef CHAR * PCHAR, * LPCH, * PCH, * NPSTR, * LPSTR, * PSTR;
//typedef CONST CHAR * LPCCH, * PCCH, * LPCSTR, * PCSTR;
	
// 16 bit data type in windows definition.
//typedef WCHAR * PWCHAR, * LPWCH, * PWCH, * NPWSTR, * LPWSTR, * PWSTR;
//typedef CONST CHAR * LPCWCH, * PCWCH, * LPCWSTR, * PCWSTR;
	
// depended on _UNICODE signal windows definition. could be 8 or 16
//TCHAR * PTCHAR, * LPTCH, * PTCH, * NPTSTR, * LPTSTR, * PTSTR;
//CONST TCHAR * LPCTCH, * PCTCH, * LPCTSTR, * PCTSTR;
	
// concultion:
// TCHAR PTCHAR and other T data type will make sure the code can be ASCII or UNICODE in future(decided by _UNICODE).
// TEXT function will make sure the string with L or without L in future(decided by _UNICODE).
// And other macro like MessageBox, wsprintf will make sure the code can be ASCII or UNICODE in future(decided by _UNICODE).

	MessageBox(NULL, TEXT("我是机器人"), TEXT("我很强壮"), 0);
	
//#ifdef UNICODE
//#define MessageBox MessageBoxW
//#else
//#define MessageBox MessageBoxA
//#endif
	
	// lstren is a windows c macro could be stren or wcslen decided by _UNICODE
	// other winows c functions including lstrcpy, lstrcat, etc...
	TCHAR s[100];
	// wsprintf is a windows c macro could be wsprintfA or wsprintfW decided by _UNICODE
	wsprintf(s, TEXT("%d"), lstrlen(TEXT("you are pig.")));
	MessageBox(NULL, s, TEXT("a"), 0);
	
	return 0;
}
