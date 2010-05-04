/*----------------------------------------------
   COLORS3.C -- Version using Common Dialog Box
                (c) Charles Petzold, 1998
  ----------------------------------------------*/

#include <windows.h>
#include <commdlg.h>

int WINAPI WinMain (HINSTANCE hInstance, HINSTANCE hPrevInstance,
                    PSTR szCmdLine, int iCmdShow)
{
     static CHOOSECOLOR cc ;
     static COLORREF    crCustColors[16] ;

     cc.lStructSize    = sizeof (CHOOSECOLOR) ;
     cc.hwndOwner      = NULL ;
     cc.hInstance      = NULL ;
     cc.rgbResult      = RGB (0x80, 0x80, 0x80) ;
     cc.lpCustColors   = crCustColors ;
     cc.Flags          = CC_RGBINIT | CC_FULLOPEN ;
     cc.lCustData      = 0 ;
     cc.lpfnHook       = NULL ;
     cc.lpTemplateName = NULL ;

     return ChooseColor (&cc) ;
}
