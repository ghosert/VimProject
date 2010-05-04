typedef int (*BTCALLBACK)(void);

typedef struct _CUSTOMBUTTONPARAM {
	HWND pHwnd;
	LPCSTR lpszNormalBitmapResource;
	LPCSTR lpszMouseMoveBitmapResource;
	LPCSTR lpszPressedBitmapResource;
	int leftPosition;
	int topPosition;
	HMENU ID_CHILDWINDOW;
	BTCALLBACK lpfnCallBack;
} CUSTOMBUTTONPARAM, *PCUSTOMBUTTONPARAM;

HRGN CreateRegionFromBitMap(HDC, BITMAP, COLORREF);

void SetWindowRgnFromBitMap(HWND, HDC, BITMAP, COLORREF, BOOL);

void CreateCustomButton(PCUSTOMBUTTONPARAM);

BOOL IsPtInWindowsRegion(HWND hwnd, int x, int y);

// flags for dwFlags parameter below.
#define AW_HOR_POSITIVE 0x1    // 从左到右打开窗口
#define AW_HOR_NEGATIVE 0x2    // 从右到左打开窗口
#define AW_VER_POSITIVE 0x4    // 从上到下打开窗口
#define AW_VER_NEGATIVE 0x8    // 从下到上打开窗口
#define AW_CENTER 0x10   // 看不出任何效果
#define AW_HIDE 0x10000 // 在窗体卸载时若想使用本函数就得加上此常量 
#define AW_ACTIVATE 0x20000 // 在窗体通过本函数打开后，默认情况下会失去焦点，除非加上本常量 
#define AW_SLIDE 0x40000 // 看不出任何效果
#define AW_BLEND 0x80000 // 淡入淡出效果
// dwTime means milliseconds.
void CreateAnimateWindows(HWND hwnd, DWORD dwTime, DWORD dwFlags, BOOL isCreateOrClose);

// flags for dwFlags parameter below.
#define   LWA_COLORKEY 0x00000001   
#define   LWA_ALPHA    0x00000002   
void CreateTransParentWindows(HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags);
