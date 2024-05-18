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
#define AW_HOR_POSITIVE 0x1    // �����Ҵ򿪴���
#define AW_HOR_NEGATIVE 0x2    // ���ҵ���򿪴���
#define AW_VER_POSITIVE 0x4    // ���ϵ��´򿪴���
#define AW_VER_NEGATIVE 0x8    // ���µ��ϴ򿪴���
#define AW_CENTER 0x10   // �������κ�Ч��
#define AW_HIDE 0x10000 // �ڴ���ж��ʱ����ʹ�ñ������͵ü��ϴ˳��� 
#define AW_ACTIVATE 0x20000 // �ڴ���ͨ���������򿪺�Ĭ������»�ʧȥ���㣬���Ǽ��ϱ����� 
#define AW_SLIDE 0x40000 // �������κ�Ч��
#define AW_BLEND 0x80000 // ���뵭��Ч��
// dwTime means milliseconds.
void CreateAnimateWindows(HWND hwnd, DWORD dwTime, DWORD dwFlags, BOOL isCreateOrClose);

// flags for dwFlags parameter below.
#define   LWA_COLORKEY 0x00000001   
#define   LWA_ALPHA    0x00000002   
void CreateTransParentWindows(HWND hwnd, COLORREF crKey, BYTE bAlpha, DWORD dwFlags);
