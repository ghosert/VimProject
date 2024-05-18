#include <windows.h>
/* hInstance is the windows hinstance
 * hPrevInstance is designed for Win16, in Win32, it has been given up, defined NULL always.
 * PSTR is a char * and szCmdLine, sz means the variable is a string end with '\0'
 * iShowCmd means the variable is a int type.
 */
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, PSTR szCmdLine, int iCmdShow) {
	/* the first variable is a windows instance.
	 * TEXT function is not necessary, but it is useful of the UNICODE env.
	 * the second and third parameters means windows text and title to be shown.
	 * the last parameter means the way to show. NORMALLY, MAXIMUM, MINIMUM
	 */
	// the constant begin with MB_ is designed for the last parameter
	// such as MB_OK; MB_OKCANCEL; to show the type of the button
	// such as  MB_ICONQUESTION; MB_ICONERROR to show the icon of the message box.
	// you can use MB_YESNO | MB_ICONQUESTION as the fourth parameter here.
	// 0 here means MB_OK
	MessageBox(NULL, TEXT("Hello Windows XP !"), TEXT("HelloMsg"), 0);
	// MessageBox return ID*** such as IDOK
	return 0;
}
