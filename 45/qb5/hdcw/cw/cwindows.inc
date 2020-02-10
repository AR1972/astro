;*
;*	CW : Character Windows
;*
;*	cwindows.inc : public include file for CW routines/structures
; Created Mon Aug 29 22:45:00 1988 */ 

pwndMsg		equ	[word ptr 00000h]
messageMsg	equ	[word ptr 00002h]
wParamMsg	equ	[word ptr 00004h]
lParamMsg	equ	[dword ptr 00006h]
LO_lParamMsg	equ	[word ptr 00006h]
HI_lParamMsg	equ	[word ptr 00008h]
timeMsg		equ	[dword ptr 0000Ah]
LO_timeMsg		equ	[word ptr 0000Ah]
HI_timeMsg		equ	[word ptr 0000Ch]
cbMsgMin	equ	0000Eh
WM_NULL		equ	00000h
WM_CREATE	equ	00001h
WM_WANTFOCUS	equ	00005h
WM_MAKEACTIVE	equ	00006h
WM_SETFOCUS	equ	00007h
WM_KILLFOCUS	equ	00008h
WM_PAINT	equ	0000Fh
WM_QUIT		equ	00012h
WM_KEYFIRST	equ	00100h
WM_KEYLAST	equ	00102h
WM_KEYDOWN	equ	00100h
WM_KEYUP	equ	00101h
WM_CHAR		equ	00102h
WM_CUT		equ	00103h
WM_PASTE	equ	00104h
WM_COPY		equ	00105h
WM_INSERT	equ	00106h
WM_MENUIDLE	equ	00110h
WM_COMMAND	equ	00111h
WM_MENUSELECT	equ	00112h
WM_MENUSTART	equ	00113h
WM_HSCROLL	equ	00114h
WM_VSCROLL	equ	00115h
WM_INITMENUPOPUP	equ	00116h
WM_ALARM	equ	00117h
WM_MOUSEFIRST	equ	00200h
WM_LMOUSELAST	equ	00203h
WM_MOUSELAST	equ	00206h
WM_MOUSEMOVE	equ	00200h
WM_LBUTTONDOWN	equ	00201h
WM_LBUTTONUP	equ	00202h
WM_LBUTTONDBLCLK	equ	00203h
WM_RBUTTONDOWN	equ	00204h
WM_RBUTTONUP	equ	00205h
WM_RBUTTONDBLCLK	equ	00206h
EM_SETSEL	equ	00300h
EM_GETSEL	equ	00301h
WM_ACTIVATE	equ	00320h
WM_ZOOM		equ	00321h
WM_CLOSE	equ	00322h
WM_MOVE		equ	00323h
WM_SIZE		equ	00324h
WM_DIALOG	equ	00380h
WM_LISTBOX_COMMAND	equ	WM_DIALOG
LB_RESETCONTENT	equ	00340h
LB_ADDSTRING	equ	00341h
LB_DELETESTRING	equ	00342h
LB_SETCURSEL	equ	00343h
LB_GETCURSEL	equ	00344h
LB_GETTEXT	equ	00346h
LB_GETCOUNT	equ	00347h
LB_REPLACESTRING	equ	00348h
LB_INSERTSTRING	equ	00349h
WM_USER		equ	00400h
LBN_SELCHANGE	equ	0
LBN_DBLCLK	equ	1
LBN_SELECT_DONE	equ	2
lbrCause	equ	0000Fh
lbrNone		equ	0
lbrMouse	equ	1
lbrScroll	equ	2
lbrKeys		equ	3
lbrSpace	equ	4
lbrOther	equ	5
flbrReselect	equ	00010h
hemMenu		equ	1
hemMenuItem	equ	2
hemMbox		equ	3
hemDialog	equ	4
hemUserMin	equ	00010h
isaBackground	equ	0
isaHilite	equ	1
isaGreyed	equ	2
isaEnabled	equ	3
isaDisabled	equ	4
isaAlert	equ	5
isaDialogBox	equ	6
isaStatic	equ	isaDialogBox
isaButton	equ	isaDialogBox
isaPushButton	equ	7
isaButtonDown	equ	8
isaListBox	equ	9
isaEdit		equ	isaDialogBox
isaScrollbar	equ	10
isaElevator	equ	11
isaMenuBox	equ	12
isaMenu		equ	13
isaMenuSelected	equ	14
isaMenuHilite	equ	15
isaMenuHiliteSel	equ	16
isaItemHiliteSel	equ	17
isaDialogAccel	equ	18
isaShadow	equ	19
isaUserMin	equ	20
isaUserMax	equ	(isaUserMin + 16)
isaMax		equ	isaUserMax
SBS_HORZ	equ	0
SBS_VERT	equ	1
SB_LINEUP	equ	0
SB_LINEDOWN	equ	1
SB_PAGEUP	equ	2
SB_PAGEDOWN	equ	3
SB_THUMBPOSITION	equ	4
SB_THUMBTRACK	equ	5
SB_TOP		equ	6
SB_BOTTOM	equ	7
SB_ENDSCROLL	equ	8
dmNormal	equ	0
dmText		equ	00100h
dmTextOnly	equ	dmText
dmForeBack	equ	00200h
dmAttrOnly	equ	dmForeBack
dmFore		equ	00300h
dmBack		equ	00400h
dmTextFore	equ	00500h
dmTextBack	equ	00600h
dmSpecialMin	equ	00700h
dmTextMapB	equ	00700h
dmTextMapF	equ	00800h
dmMapB		equ	00900h
dmMapF		equ	00A00h
fdmKeepFfont	equ	08000h
VK_MIN		equ	00100h
VK_LBUTTON	equ	00101h
VK_RBUTTON	equ	00102h
VK_CANCEL	equ	00103h
VK_MBUTTON	equ	00104h
VK_BACK		equ	00108h
VK_TAB		equ	00109h
VK_CLEAR	equ	0010Ch
VK_RETURN	equ	0010Dh
VK_SHIFT	equ	00110h
VK_CONTROL	equ	00111h
VK_ALT		equ	00112h
VK_PAUSE	equ	00113h
VK_CAPLOCK	equ	00114h
VK_KANA		equ	00115h
VK_ROMAJI	equ	00116h
VK_ZENKAKU	equ	00117h
VK_HIRAGANA	equ	00118h
VK_KANJI	equ	00119h
VK_CONVERT	equ	0011Ch
VK_NONCONVERT	equ	0011Dh
VK_ACCEPT	equ	0011Eh
VK_MODECHANGE	equ	0011Fh
VK_ESCAPE	equ	0011Bh
VK_SPACE	equ	00120h
VK_PRIOR	equ	00121h
VK_NEXT		equ	00122h
VK_END		equ	00123h
VK_HOME		equ	00124h
VK_LEFT		equ	00125h
VK_UP		equ	00126h
VK_RIGHT	equ	00127h
VK_DOWN		equ	00128h
VK_0		equ	00130h
VK_9		equ	00139h
VK_A		equ	00141h
VK_Z		equ	0015Ah
VK_SELECT	equ	00129h
VK_PRINT	equ	0012Ah
VK_EXECUTE	equ	0012Bh
VK_INSERT	equ	0012Dh
VK_DELETE	equ	0012Eh
VK_HELP		equ	0012Fh
VK_NUMPAD0	equ	00160h
VK_NUMPAD1	equ	00161h
VK_NUMPAD2	equ	00162h
VK_NUMPAD3	equ	00163h
VK_NUMPAD4	equ	00164h
VK_NUMPAD5	equ	00165h
VK_NUMPAD6	equ	00166h
VK_NUMPAD7	equ	00167h
VK_NUMPAD8	equ	00168h
VK_NUMPAD9	equ	00169h
VK_MULTIPLY	equ	0016Ah
VK_ADD		equ	0016Bh
VK_SEPARATOR	equ	0016Ch
VK_SUBTRACT	equ	0016Dh
VK_DECIMAL	equ	0016Eh
VK_DIVIDE	equ	0016Fh
VK_F1		equ	00170h
VK_F2		equ	00171h
VK_F3		equ	00172h
VK_F4		equ	00173h
VK_F5		equ	00174h
VK_F6		equ	00175h
VK_F7		equ	00176h
VK_F8		equ	00177h
VK_F9		equ	00178h
VK_F10		equ	00179h
VK_F11		equ	0017Ah
VK_F12		equ	0017Bh
VK_F13		equ	0017Ch
VK_F14		equ	0017Dh
VK_F15		equ	0017Eh
VK_F16		equ	0017Fh
VK_OAX		equ	00180h
VK_NUMLOCK	equ	00190h
VK_SCRLOCK	equ	00191h
VK_MENU		equ	VK_ALT
VK_CAPITAL	equ	VK_CAPLOCK
VK_OEM_NUMBER	equ	VK_NUMLOCK
VK_OEM_SCROLL	equ	VK_SCRLOCK
VK_SEPARATER	equ	VK_SEPARATOR
ichSelectEnd	equ	(07FFFh)
EN_CHANGE	equ	1
EN_CURSORMOVED	equ	2
fmemFixed	equ	1
fmemNear	equ	2
cbNearMemServiceMax	equ	128
indtNil		equ	0
indtKeyboard	equ	1
indtCharacterScreen	equ	2
indtGraphicScreen	equ	3
indtCharacterPrinter	equ	4
indtGraphicPrinter	equ	5
indtSystem	equ	6
indtSerialComm	equ	7

indtIndv	equ	[byte ptr 00000h]
fillerIndv	equ	[byte ptr 00001h]
rglpfnIndv	equ	[word ptr 00002h]
cpfnNeedMinIndv	equ	[word ptr 00004h]
cpfnNeedMacIndv	equ	[word ptr 00006h]
psLoadedIndv	equ	[word ptr 00008h]
cpfnLoadedIndv	equ	[word ptr 0000Ah]
cbIndvMin	equ	0000Ch

rgchMagicIndh	equ	[dword ptr 00000h]
LO_rgchMagicIndh	equ	[word ptr 00000h]
HI_rgchMagicIndh	equ	[word ptr 00002h]
dlfaTableIndh	equ	[dword ptr 00004h]
LO_dlfaTableIndh	equ	[word ptr 00004h]
HI_dlfaTableIndh	equ	[word ptr 00006h]
cbIndhMin	equ	00008h

indtInds	equ	[byte ptr 00000h]
floadInds	equ	[byte ptr 00001h]
cbCodeInds	equ	[word ptr 00002h]
dlfaCodeInds	equ	[dword ptr 00004h]
LO_dlfaCodeInds	equ	[word ptr 00004h]
HI_dlfaCodeInds	equ	[word ptr 00006h]
cbIndsMin	equ	00008h

rgchMagicIndt	equ	[dword ptr 00000h]
LO_rgchMagicIndt	equ	[word ptr 00000h]
HI_rgchMagicIndt	equ	[word ptr 00002h]
cindsIndt	equ	[word ptr 00004h]
rgindsIndt	equ	[00006h]
cbIndtMin	equ	0000Eh
floadFixed	equ	1
floadRealMode	equ	00010h
floadProtMode	equ	00020h
floadAnyMode	equ	(floadRealMode or floadProtMode)
floadStandard	equ	00080h

sdGlisInos	equ	[word ptr 00000h]
sdLoisInos	equ	[word ptr 00002h]
cpfnInos	equ	[word ptr 00004h]
lpfnDosGetVersionInos	equ	[dword ptr 00006h]
OFF_lpfnDosGetVersionInos	equ	[word ptr 00006h]
SEG_lpfnDosGetVersionInos	equ	[word ptr 00008h]
lpfnDosGetEnvInos	equ	[dword ptr 0000Ah]
OFF_lpfnDosGetEnvInos	equ	[word ptr 0000Ah]
SEG_lpfnDosGetEnvInos	equ	[word ptr 0000Ch]
lpfnDosDevConfigInos	equ	[dword ptr 0000Eh]
OFF_lpfnDosDevConfigInos	equ	[word ptr 0000Eh]
SEG_lpfnDosDevConfigInos	equ	[word ptr 00010h]
lpfnDosGetCtryInfoInos	equ	[dword ptr 00012h]
OFF_lpfnDosGetCtryInfoInos	equ	[word ptr 00012h]
SEG_lpfnDosGetCtryInfoInos	equ	[word ptr 00014h]
lpfnDosGetDBCSEvInos	equ	[dword ptr 00016h]
OFF_lpfnDosGetDBCSEvInos	equ	[word ptr 00016h]
SEG_lpfnDosGetDBCSEvInos	equ	[word ptr 00018h]
lpfnDosGetInfoSegInos	equ	[dword ptr 0001Ah]
OFF_lpfnDosGetInfoSegInos	equ	[word ptr 0001Ah]
SEG_lpfnDosGetInfoSegInos	equ	[word ptr 0001Ch]
lpfnDosDevIOCtlInos	equ	[dword ptr 0001Eh]
OFF_lpfnDosDevIOCtlInos	equ	[word ptr 0001Eh]
SEG_lpfnDosDevIOCtlInos	equ	[word ptr 00020h]
lpfnDosPortAccessInos	equ	[dword ptr 00022h]
OFF_lpfnDosPortAccessInos	equ	[word ptr 00022h]
SEG_lpfnDosPortAccessInos	equ	[word ptr 00024h]
lpfnDosGetModHandleInos	equ	[dword ptr 00026h]
OFF_lpfnDosGetModHandleInos	equ	[word ptr 00026h]
SEG_lpfnDosGetModHandleInos	equ	[word ptr 00028h]
lpfnDosLoadModuleInos	equ	[dword ptr 0002Ah]
OFF_lpfnDosLoadModuleInos	equ	[word ptr 0002Ah]
SEG_lpfnDosLoadModuleInos	equ	[word ptr 0002Ch]
lpfnDosGetProcAddrInos	equ	[dword ptr 0002Eh]
OFF_lpfnDosGetProcAddrInos	equ	[word ptr 0002Eh]
SEG_lpfnDosGetProcAddrInos	equ	[word ptr 00030h]
lpfnDosCreateThreadInos	equ	[dword ptr 00032h]
OFF_lpfnDosCreateThreadInos	equ	[word ptr 00032h]
SEG_lpfnDosCreateThreadInos	equ	[word ptr 00034h]
lpfnDosSetPrtyInos	equ	[dword ptr 00036h]
OFF_lpfnDosSetPrtyInos	equ	[word ptr 00036h]
SEG_lpfnDosSetPrtyInos	equ	[word ptr 00038h]
lpfnDosExitInos	equ	[dword ptr 0003Ah]
OFF_lpfnDosExitInos	equ	[word ptr 0003Ah]
SEG_lpfnDosExitInos	equ	[word ptr 0003Ch]
lpfnDosOpenInos	equ	[dword ptr 0003Eh]
OFF_lpfnDosOpenInos	equ	[word ptr 0003Eh]
SEG_lpfnDosOpenInos	equ	[word ptr 00040h]
lpfnDosCloseInos	equ	[dword ptr 00042h]
OFF_lpfnDosCloseInos	equ	[word ptr 00042h]
SEG_lpfnDosCloseInos	equ	[word ptr 00044h]
lpfnDosReadInos	equ	[dword ptr 00046h]
OFF_lpfnDosReadInos	equ	[word ptr 00046h]
SEG_lpfnDosReadInos	equ	[word ptr 00048h]
lpfnDosWriteInos	equ	[dword ptr 0004Ah]
OFF_lpfnDosWriteInos	equ	[word ptr 0004Ah]
SEG_lpfnDosWriteInos	equ	[word ptr 0004Ch]
lpfnDosMonOpenInos	equ	[dword ptr 0004Eh]
OFF_lpfnDosMonOpenInos	equ	[word ptr 0004Eh]
SEG_lpfnDosMonOpenInos	equ	[word ptr 00050h]
lpfnDosMonCloseInos	equ	[dword ptr 00052h]
OFF_lpfnDosMonCloseInos	equ	[word ptr 00052h]
SEG_lpfnDosMonCloseInos	equ	[word ptr 00054h]
lpfnDosMonRegInos	equ	[dword ptr 00056h]
OFF_lpfnDosMonRegInos	equ	[word ptr 00056h]
SEG_lpfnDosMonRegInos	equ	[word ptr 00058h]
lpfnDosMonReadInos	equ	[dword ptr 0005Ah]
OFF_lpfnDosMonReadInos	equ	[word ptr 0005Ah]
SEG_lpfnDosMonReadInos	equ	[word ptr 0005Ch]
lpfnDosMonWriteInos	equ	[dword ptr 0005Eh]
OFF_lpfnDosMonWriteInos	equ	[word ptr 0005Eh]
SEG_lpfnDosMonWriteInos	equ	[word ptr 00060h]
lpfnDosAllocSegInos	equ	[dword ptr 00062h]
OFF_lpfnDosAllocSegInos	equ	[word ptr 00062h]
SEG_lpfnDosAllocSegInos	equ	[word ptr 00064h]
lpfnDosReAllocSegInos	equ	[dword ptr 00066h]
OFF_lpfnDosReAllocSegInos	equ	[word ptr 00066h]
SEG_lpfnDosReAllocSegInos	equ	[word ptr 00068h]
lpfnDosFreeSegInos	equ	[dword ptr 0006Ah]
OFF_lpfnDosFreeSegInos	equ	[word ptr 0006Ah]
SEG_lpfnDosFreeSegInos	equ	[word ptr 0006Ch]
lpfnVioGetBufInos	equ	[dword ptr 0006Eh]
OFF_lpfnVioGetBufInos	equ	[word ptr 0006Eh]
SEG_lpfnVioGetBufInos	equ	[word ptr 00070h]
lpfnVioShowBufInos	equ	[dword ptr 00072h]
OFF_lpfnVioShowBufInos	equ	[word ptr 00072h]
SEG_lpfnVioShowBufInos	equ	[word ptr 00074h]
lpfnVioGetConfigInos	equ	[dword ptr 00076h]
OFF_lpfnVioGetConfigInos	equ	[word ptr 00076h]
SEG_lpfnVioGetConfigInos	equ	[word ptr 00078h]
lpfnVioGetModeInos	equ	[dword ptr 0007Ah]
OFF_lpfnVioGetModeInos	equ	[word ptr 0007Ah]
SEG_lpfnVioGetModeInos	equ	[word ptr 0007Ch]
lpfnVioSetModeInos	equ	[dword ptr 0007Eh]
OFF_lpfnVioSetModeInos	equ	[word ptr 0007Eh]
SEG_lpfnVioSetModeInos	equ	[word ptr 00080h]
lpfnVioGetStateInos	equ	[dword ptr 00082h]
OFF_lpfnVioGetStateInos	equ	[word ptr 00082h]
SEG_lpfnVioGetStateInos	equ	[word ptr 00084h]
lpfnVioSetStateInos	equ	[dword ptr 00086h]
OFF_lpfnVioSetStateInos	equ	[word ptr 00086h]
SEG_lpfnVioSetStateInos	equ	[word ptr 00088h]
lpfnVioGetCurTypeInos	equ	[dword ptr 0008Ah]
OFF_lpfnVioGetCurTypeInos	equ	[word ptr 0008Ah]
SEG_lpfnVioGetCurTypeInos	equ	[word ptr 0008Ch]
lpfnVioSetCurTypeInos	equ	[dword ptr 0008Eh]
OFF_lpfnVioSetCurTypeInos	equ	[word ptr 0008Eh]
SEG_lpfnVioSetCurTypeInos	equ	[word ptr 00090h]
lpfnVioGetCurPosInos	equ	[dword ptr 00092h]
OFF_lpfnVioGetCurPosInos	equ	[word ptr 00092h]
SEG_lpfnVioGetCurPosInos	equ	[word ptr 00094h]
lpfnVioSetCurPosInos	equ	[dword ptr 00096h]
OFF_lpfnVioSetCurPosInos	equ	[word ptr 00096h]
SEG_lpfnVioSetCurPosInos	equ	[word ptr 00098h]
lpfnVioGetFontInos	equ	[dword ptr 0009Ah]
OFF_lpfnVioGetFontInos	equ	[word ptr 0009Ah]
SEG_lpfnVioGetFontInos	equ	[word ptr 0009Ch]
lpfnVioSetFontInos	equ	[dword ptr 0009Eh]
OFF_lpfnVioSetFontInos	equ	[word ptr 0009Eh]
SEG_lpfnVioSetFontInos	equ	[word ptr 000A0h]
lpfnVioGetCPInos	equ	[dword ptr 000A2h]
OFF_lpfnVioGetCPInos	equ	[word ptr 000A2h]
SEG_lpfnVioGetCPInos	equ	[word ptr 000A4h]
lpfnVioSetCPInos	equ	[dword ptr 000A6h]
OFF_lpfnVioSetCPInos	equ	[word ptr 000A6h]
SEG_lpfnVioSetCPInos	equ	[word ptr 000A8h]
lpfnVioScrollUpInos	equ	[dword ptr 000AAh]
OFF_lpfnVioScrollUpInos	equ	[word ptr 000AAh]
SEG_lpfnVioScrollUpInos	equ	[word ptr 000ACh]
lpfnKbdOpenInos	equ	[dword ptr 000AEh]
OFF_lpfnKbdOpenInos	equ	[word ptr 000AEh]
SEG_lpfnKbdOpenInos	equ	[word ptr 000B0h]
lpfnKbdCloseInos	equ	[dword ptr 000B2h]
OFF_lpfnKbdCloseInos	equ	[word ptr 000B2h]
SEG_lpfnKbdCloseInos	equ	[word ptr 000B4h]
lpfnKbdCharInInos	equ	[dword ptr 000B6h]
OFF_lpfnKbdCharInInos	equ	[word ptr 000B6h]
SEG_lpfnKbdCharInInos	equ	[word ptr 000B8h]
lpfnKbdGetStatusInos	equ	[dword ptr 000BAh]
OFF_lpfnKbdGetStatusInos	equ	[word ptr 000BAh]
SEG_lpfnKbdGetStatusInos	equ	[word ptr 000BCh]
lpfnKbdSetStatusInos	equ	[dword ptr 000BEh]
OFF_lpfnKbdSetStatusInos	equ	[word ptr 000BEh]
SEG_lpfnKbdSetStatusInos	equ	[word ptr 000C0h]
lpfnDosBeepInos	equ	[dword ptr 000C2h]
OFF_lpfnDosBeepInos	equ	[word ptr 000C2h]
SEG_lpfnDosBeepInos	equ	[word ptr 000C4h]
lpfnVioGetPhysBufInos	equ	[dword ptr 000C6h]
OFF_lpfnVioGetPhysBufInos	equ	[word ptr 000C6h]
SEG_lpfnVioGetPhysBufInos	equ	[word ptr 000C8h]
lpfnCwBeginIOInos	equ	[dword ptr 000CAh]
OFF_lpfnCwBeginIOInos	equ	[word ptr 000CAh]
SEG_lpfnCwBeginIOInos	equ	[word ptr 000CCh]
lpfnCwEndIOInos	equ	[dword ptr 000CEh]
OFF_lpfnCwEndIOInos	equ	[word ptr 000CEh]
SEG_lpfnCwEndIOInos	equ	[word ptr 000D0h]
cbInosMin	equ	000D2h

cpfnIncs	equ	[word ptr 00000h]
cbIncsMin	equ	00002h

lpwDataInsh	equ	[dword ptr 00000h]
LO_lpwDataInsh	equ	[word ptr 00000h]
HI_lpwDataInsh	equ	[word ptr 00002h]
pinosInsh	equ	[word ptr 00004h]
pincsInsh	equ	[word ptr 00006h]
cpfnInsh	equ	[word ptr 00008h]
cbInshMin	equ	0000Ah

dxCharInft	equ	[byte ptr 00000h]
dyCharInft	equ	[byte ptr 00001h]
dyBaseLineInft	equ	[byte ptr 00002h]
ifontInft	equ	[byte ptr 00003h]
cbInftMin	equ	00004h
finstText	equ	1
finstGraphics	equ	2
finstMonochrome	equ	4
finstAlternate	equ	8
finstFont	equ	00010h
finstCgaSnow	equ	00020h
finstDisableMouse	equ	01000h
finstFastScroll	equ	02000h
finstQuestionable	equ	04000h
finstAvailable	equ	08000h

finstInst	equ	[word ptr 00000h]
axMacInst	equ	[byte ptr 00002h]
ayMacInst	equ	[byte ptr 00003h]
coMacInst	equ	[byte ptr 00004h]
covMacInst	equ	[byte ptr 00005h]
coiMacInst	equ	[word ptr 00006h]
imodeInst	equ	[word ptr 00008h]
inftInst	equ	[dword ptr 0000Ah]
LO_inftInst	equ	[word ptr 0000Ah]
HI_inftInst	equ	[word ptr 0000Ch]
ffontSupportedInst	equ	[word ptr 0000Eh]
psPrimInst	equ	[word ptr 00010h]
psSecInst	equ	[word ptr 00012h]
cwExtraInst	equ	[word ptr 00014h]
psExtraInst	equ	[word ptr 00016h]
bits0Inst	equ	[word ptr 00018h]
bits0recInst	record	fillerInst:15,fAllocPrimInst:1
wDriver1Inst	equ	[word ptr 0001Ah]
reserved2Inst	equ	[0001Ch]
cbInstMin	equ	0002Ah
imodeUnknown	equ	0FFFFh

_chTopLeftCorner1Inch	equ	[byte ptr 00000h]
_chTopRightCorner1Inch	equ	[byte ptr 00001h]
_chBottomLeftCorner1Inch	equ	[byte ptr 00002h]
_chBottomRightCorner1Inch	equ	[byte ptr 00003h]
_chTopSide1Inch	equ	[byte ptr 00004h]
_chBottomSide1Inch	equ	[byte ptr 00005h]
_chLeftSide1Inch	equ	[byte ptr 00006h]
_chRightSide1Inch	equ	[byte ptr 00007h]
_chMiddleLeft1Inch	equ	[byte ptr 00008h]
_chMiddleRight1Inch	equ	[byte ptr 00009h]
_chTopLeftCorner2Inch	equ	[byte ptr 0000Ah]
_chTopRightCorner2Inch	equ	[byte ptr 0000Bh]
_chBottomLeftCorner2Inch	equ	[byte ptr 0000Ch]
_chBottomRightCorner2Inch	equ	[byte ptr 0000Dh]
_chTopSide2Inch	equ	[byte ptr 0000Eh]
_chBottomSide2Inch	equ	[byte ptr 0000Fh]
_chLeftSide2Inch	equ	[byte ptr 00010h]
_chRightSide2Inch	equ	[byte ptr 00011h]
_chUpArrowInch	equ	[byte ptr 00012h]
_chDownArrowInch	equ	[byte ptr 00013h]
_chLeftArrowInch	equ	[byte ptr 00014h]
_chRightArrowInch	equ	[byte ptr 00015h]
_chBulletInch	equ	[byte ptr 00016h]
_chMiddleDotInch	equ	[byte ptr 00017h]
_chScrollbarInch	equ	[byte ptr 00018h]
_chElevatorInch	equ	[byte ptr 00019h]
_chShadowInitInch	equ	[byte ptr 0001Ah]
_chCloseInch	equ	[byte ptr 0001Bh]
_chZoomInInch	equ	[byte ptr 0001Ch]
_chZoomOutInch	equ	[byte ptr 0001Dh]
_chUpDownArrowInch	equ	[byte ptr 0001Eh]
_chLeftRightArrowInch	equ	[byte ptr 0001Fh]
reservedInch	equ	[00020h]
cbInchMin	equ	00040h
ffontNormal	equ	00000h
ffontUnderline	equ	00001h
ffontDoubleUnderline	equ	00002h
ffontOrUnderline	equ	00003h
ffontStrikeThrough	equ	00004h
ffontBold	equ	00008h
ffontSubscript	equ	00010h
ffontSuperscript	equ	00020h
ffontMiniCap	equ	00030h
ffontItalic	equ	00040h
ffontOrCharacter	equ	00080h
ffontReservedBits	equ	00F00h
ffontExtraMask	equ	0F000h
ffontOrUnderlineSupport	equ	01000h
fvidsChAttr	equ	1

modeVids	equ	[byte ptr 00000h]
pageVids	equ	[byte ptr 00001h]
fvidsVids	equ	[word ptr 00002h]
cwVidDataVids	equ	[word ptr 00004h]
cwExtraVids	equ	[word ptr 00006h]
rgwExtraVids	equ	[word ptr 00008h]
cbVidsMin	equ	0000Ah

lpfnImodeGuessCurrentCsdInsj	equ	[dword ptr 00000h]
OFF_lpfnImodeGuessCurrentCsdInsj	equ	[word ptr 00000h]
SEG_lpfnImodeGuessCurrentCsdInsj	equ	[word ptr 00002h]
lpfnFQueryInstCsdInsj	equ	[dword ptr 00004h]
OFF_lpfnFQueryInstCsdInsj	equ	[word ptr 00004h]
SEG_lpfnFQueryInstCsdInsj	equ	[word ptr 00006h]
lpfnFInitCsdInsj	equ	[dword ptr 00008h]
OFF_lpfnFInitCsdInsj	equ	[word ptr 00008h]
SEG_lpfnFInitCsdInsj	equ	[word ptr 0000Ah]
lpfnTermCsdInsj	equ	[dword ptr 0000Ch]
OFF_lpfnTermCsdInsj	equ	[word ptr 0000Ch]
SEG_lpfnTermCsdInsj	equ	[word ptr 0000Eh]
lpfnMoveHwCursCsdInsj	equ	[dword ptr 00010h]
OFF_lpfnMoveHwCursCsdInsj	equ	[word ptr 00010h]
SEG_lpfnMoveHwCursCsdInsj	equ	[word ptr 00012h]
lpfnFQueryInftCsdInsj	equ	[dword ptr 00014h]
OFF_lpfnFQueryInftCsdInsj	equ	[word ptr 00014h]
SEG_lpfnFQueryInftCsdInsj	equ	[word ptr 00016h]
lpfnFGetColorPaletteCsdInsj	equ	[dword ptr 00018h]
OFF_lpfnFGetColorPaletteCsdInsj	equ	[word ptr 00018h]
SEG_lpfnFGetColorPaletteCsdInsj	equ	[word ptr 0001Ah]
lpfnSetColorPaletteCsdInsj	equ	[dword ptr 0001Ch]
OFF_lpfnSetColorPaletteCsdInsj	equ	[word ptr 0001Ch]
SEG_lpfnSetColorPaletteCsdInsj	equ	[word ptr 0001Eh]
lpfnPrepUpdateCsdInsj	equ	[dword ptr 00020h]
OFF_lpfnPrepUpdateCsdInsj	equ	[word ptr 00020h]
SEG_lpfnPrepUpdateCsdInsj	equ	[word ptr 00022h]
lpfnDoUpdateCsdInsj	equ	[dword ptr 00024h]
OFF_lpfnDoUpdateCsdInsj	equ	[word ptr 00024h]
SEG_lpfnDoUpdateCsdInsj	equ	[word ptr 00026h]
lpfnDoneUpdateCsdInsj	equ	[dword ptr 00028h]
OFF_lpfnDoneUpdateCsdInsj	equ	[word ptr 00028h]
SEG_lpfnDoneUpdateCsdInsj	equ	[word ptr 0002Ah]
lpfnSpecialUpdateCsdInsj	equ	[dword ptr 0002Ch]
OFF_lpfnSpecialUpdateCsdInsj	equ	[word ptr 0002Ch]
SEG_lpfnSpecialUpdateCsdInsj	equ	[word ptr 0002Eh]
lpfnCbSizeVidsCsdInsj	equ	[dword ptr 00030h]
OFF_lpfnCbSizeVidsCsdInsj	equ	[word ptr 00030h]
SEG_lpfnCbSizeVidsCsdInsj	equ	[word ptr 00032h]
lpfnFSaveVidsCsdInsj	equ	[dword ptr 00034h]
OFF_lpfnFSaveVidsCsdInsj	equ	[word ptr 00034h]
SEG_lpfnFSaveVidsCsdInsj	equ	[word ptr 00036h]
lpfnFRestoreVidsCsdInsj	equ	[dword ptr 00038h]
OFF_lpfnFRestoreVidsCsdInsj	equ	[word ptr 00038h]
SEG_lpfnFRestoreVidsCsdInsj	equ	[word ptr 0003Ah]
lpfnSaveVidDataCsdInsj	equ	[dword ptr 0003Ch]
OFF_lpfnSaveVidDataCsdInsj	equ	[word ptr 0003Ch]
SEG_lpfnSaveVidDataCsdInsj	equ	[word ptr 0003Eh]
lpfnRestoreVidDataCsdInsj	equ	[dword ptr 00040h]
OFF_lpfnRestoreVidDataCsdInsj	equ	[word ptr 00040h]
SEG_lpfnRestoreVidDataCsdInsj	equ	[word ptr 00042h]
lpfnEnableVidsMonitorCsdInsj	equ	[dword ptr 00044h]
OFF_lpfnEnableVidsMonitorCsdInsj	equ	[word ptr 00044h]
SEG_lpfnEnableVidsMonitorCsdInsj	equ	[word ptr 00046h]
lpfnBltArcCsdInsj	equ	[dword ptr 00048h]
OFF_lpfnBltArcCsdInsj	equ	[word ptr 00048h]
SEG_lpfnBltArcCsdInsj	equ	[word ptr 0004Ah]
lpfnGetCharMapCsdInsj	equ	[dword ptr 0004Ch]
OFF_lpfnGetCharMapCsdInsj	equ	[word ptr 0004Ch]
SEG_lpfnGetCharMapCsdInsj	equ	[word ptr 0004Eh]
cbInsjMin	equ	00050h
cpfnCsdMin	equ	20
KK_EXTENDED	equ	08000h
KK_CAPLOCK	equ	04000h
KK_NUMLOCK	equ	02000h
KK_SCRLOCK	equ	01000h
KK_ALT		equ	00800h
KK_CONTROL	equ	00400h
KK_SHIFT	equ	00200h
KK_VK		equ	001FFh
KK_SC		equ	000FFh

lpfnKeyboardMessageInkb	equ	[dword ptr 00000h]
OFF_lpfnKeyboardMessageInkb	equ	[word ptr 00000h]
SEG_lpfnKeyboardMessageInkb	equ	[word ptr 00002h]
lpfnFTestKeyboardEmptyInkb	equ	[dword ptr 00004h]
OFF_lpfnFTestKeyboardEmptyInkb	equ	[word ptr 00004h]
SEG_lpfnFTestKeyboardEmptyInkb	equ	[word ptr 00006h]
lpfnSpecialAbortInkb	equ	[dword ptr 00008h]
OFF_lpfnSpecialAbortInkb	equ	[word ptr 00008h]
SEG_lpfnSpecialAbortInkb	equ	[word ptr 0000Ah]
fAbortInkb	equ	[word ptr 0000Ch]
fPollKeyboardInkb	equ	[word ptr 0000Eh]
fKeyIsUpInkb	equ	[byte ptr 00010h]
fKeyWasUpInkb	equ	[byte ptr 00011h]
wRateKeyRepeatInkb	equ	[word ptr 00012h]
fNormalKeyboardInkb	equ	[word ptr 00014h]
fNonAltKeyHitInkb	equ	[byte ptr 00016h]
fDisableExtendedInkb	equ	[word ptr 00018h]
cbInkbMin	equ	0001Ah

pfnEnableKeyboardKbdInkj	equ	[dword ptr 00000h]
OFF_pfnEnableKeyboardKbdInkj	equ	[word ptr 00000h]
SEG_pfnEnableKeyboardKbdInkj	equ	[word ptr 00002h]
pfnPollKeyboardKbdInkj	equ	[dword ptr 00004h]
OFF_pfnPollKeyboardKbdInkj	equ	[word ptr 00004h]
SEG_pfnPollKeyboardKbdInkj	equ	[word ptr 00006h]
pfnFlushKeyRgchKbdInkj	equ	[dword ptr 00008h]
OFF_pfnFlushKeyRgchKbdInkj	equ	[word ptr 00008h]
SEG_pfnFlushKeyRgchKbdInkj	equ	[word ptr 0000Ah]
pfnMkGetShiftStatesKbdInkj	equ	[dword ptr 0000Ch]
OFF_pfnMkGetShiftStatesKbdInkj	equ	[word ptr 0000Ch]
SEG_pfnMkGetShiftStatesKbdInkj	equ	[word ptr 0000Eh]
pfnSetShiftKkKbdInkj	equ	[dword ptr 00010h]
OFF_pfnSetShiftKkKbdInkj	equ	[word ptr 00010h]
SEG_pfnSetShiftKkKbdInkj	equ	[word ptr 00012h]
pfnChAlternateKeytopKbdInkj	equ	[dword ptr 00014h]
OFF_pfnChAlternateKeytopKbdInkj	equ	[word ptr 00014h]
SEG_pfnChAlternateKeytopKbdInkj	equ	[word ptr 00016h]
cbInkjMin	equ	00018h
cpfnKbdMin	equ	6

lpfnDoSoundSydInyj	equ	[dword ptr 00000h]
OFF_lpfnDoSoundSydInyj	equ	[word ptr 00000h]
SEG_lpfnDoSoundSydInyj	equ	[word ptr 00002h]
lpfnLGetTimeSydInyj	equ	[dword ptr 00004h]
OFF_lpfnLGetTimeSydInyj	equ	[word ptr 00004h]
SEG_lpfnLGetTimeSydInyj	equ	[word ptr 00006h]
cbInyjMin	equ	00008h
cpfnSydMin	equ	2

colHotMcb	equ	[word ptr 00000h]
rowHotMcb	equ	[word ptr 00002h]
rgwAndMaskGfxMcb	equ	[00004h]
rgwXorMaskGfxMcb	equ	[00024h]
wAndMaskTextMcb	equ	[word ptr 00044h]
wXorMaskTextMcb	equ	[word ptr 00046h]
cbMcbMin	equ	00048h

leftXMcob	equ	[word ptr 00000h]
upperYMcob	equ	[word ptr 00002h]
rightXMcob	equ	[word ptr 00004h]
lowerYMcob	equ	[word ptr 00006h]
cbMcobMin	equ	00008h
MK_LBUTTON	equ	00001h
MK_RBUTTON	equ	00002h
MK_SHIFT	equ	00004h
MK_CONTROL	equ	00008h
MK_MBUTTON	equ	00010h
MK_NONCLIENT	equ	00060h
MK_NONCLIENT_X	equ	00020h
MK_NONCLIENT_Y	equ	00040h
MK_MENU		equ	08000h
rerrOk		equ	0
rerrBadFile	equ	2
rerrBadPath	equ	3
rerrNoMemory	equ	8
rerrBadFormat	equ	11
rerrBadRead	equ	30
rerrBadVersion	equ	90
rerrBadMemReq	equ	91

xGpt		equ	[word ptr 00000h]
yGpt		equ	[word ptr 00002h]
cbGptMin	equ	00004h

xLeftRect	equ	[word ptr 00000h]
yTopRect	equ	[word ptr 00002h]
xRightRect	equ	[word ptr 00004h]
yBottomRect	equ	[word ptr 00006h]
cbRectMin	equ	00008h

cbPolygon	equ	[word ptr 00000h]
rectBoundPolygon	equ	[00002h]
rggptPolygon	equ	[dword ptr 0000Ah]
LO_rggptPolygon	equ	[word ptr 0000Ah]
HI_rggptPolygon	equ	[word ptr 0000Ch]
cbPolygonMin	equ	0000Eh
cpenMax		equ	41
icoMax		equ	41

fingdIngd	equ	[word ptr 00000h]
reserved2Ingd	equ	[word ptr 00002h]
fingpSupportedIngd	equ	[word ptr 00004h]
dimHIngd	equ	[word ptr 00006h]
dimVIngd	equ	[word ptr 00008h]
dxScreenIngd	equ	[word ptr 0000Ah]
dyScreenIngd	equ	[word ptr 0000Ch]
dimPenHIngd	equ	[word ptr 0000Eh]
dimPenVIngd	equ	[word ptr 00010h]
cpenIngd	equ	[byte ptr 00012h]
icoAvailMacIngd	equ	[byte ptr 00013h]
icoPrefMacIngd	equ	[byte ptr 00014h]
ipaLineMacIngd	equ	[byte ptr 00015h]
ipaAreaMacIngd	equ	[byte ptr 00016h]
ccoplnIngd	equ	[byte ptr 00017h]
rgcoAvailIngd	equ	[00018h]
rgcoPrefIngd	equ	[0006Ah]
rgpaLineIngd	equ	[000BCh]
rgpaAreaIngd	equ	[000C1h]
szNameIngd	equ	[000D1h]
cbIngdMin	equ	000FAh
fingdRstrVctr	equ	00004h
fingdRasterFonts	equ	00040h
fingdMultiColor	equ	00100h
fingdVirtualPen	equ	00200h
fingdFilm	equ	00400h
fingdVarPenSize	equ	01000h
fingdNotAvailable	equ	08000h
fingpSetAreaPat	equ	00001h
fingpSetLinePat	equ	00002h
fingpSetLineWeight	equ	00004h
fingpSetColor	equ	00008h
fingpText	equ	00010h
fingpRectangle	equ	00020h
fingpArc	equ	00040h
fingpPolygon	equ	00080h
fingpBitBlt	equ	00100h

pfnFInitGraphicsIndj	equ	[dword ptr 00000h]
OFF_pfnFInitGraphicsIndj	equ	[word ptr 00000h]
SEG_pfnFInitGraphicsIndj	equ	[word ptr 00002h]
pfnTermGraphicsIndj	equ	[dword ptr 00004h]
OFF_pfnTermGraphicsIndj	equ	[word ptr 00004h]
SEG_pfnTermGraphicsIndj	equ	[word ptr 00006h]
pfnMoveIndj	equ	[dword ptr 00008h]
OFF_pfnMoveIndj	equ	[word ptr 00008h]
SEG_pfnMoveIndj	equ	[word ptr 0000Ah]
pfnDrawIndj	equ	[dword ptr 0000Ch]
OFF_pfnDrawIndj	equ	[word ptr 0000Ch]
SEG_pfnDrawIndj	equ	[word ptr 0000Eh]
pfnSetAreaPatIndj	equ	[dword ptr 00010h]
OFF_pfnSetAreaPatIndj	equ	[word ptr 00010h]
SEG_pfnSetAreaPatIndj	equ	[word ptr 00012h]
pfnSetLinePatIndj	equ	[dword ptr 00014h]
OFF_pfnSetLinePatIndj	equ	[word ptr 00014h]
SEG_pfnSetLinePatIndj	equ	[word ptr 00016h]
pfnSetLineWeightIndj	equ	[dword ptr 00018h]
OFF_pfnSetLineWeightIndj	equ	[word ptr 00018h]
SEG_pfnSetLineWeightIndj	equ	[word ptr 0001Ah]
pfnSetColorIndj	equ	[dword ptr 0001Ch]
OFF_pfnSetColorIndj	equ	[word ptr 0001Ch]
SEG_pfnSetColorIndj	equ	[word ptr 0001Eh]
pfnTextIndj	equ	[dword ptr 00020h]
OFF_pfnTextIndj	equ	[word ptr 00020h]
SEG_pfnTextIndj	equ	[word ptr 00022h]
pfnRectangleIndj	equ	[dword ptr 00024h]
OFF_pfnRectangleIndj	equ	[word ptr 00024h]
SEG_pfnRectangleIndj	equ	[word ptr 00026h]
pfnArcIndj	equ	[dword ptr 00028h]
OFF_pfnArcIndj	equ	[word ptr 00028h]
SEG_pfnArcIndj	equ	[word ptr 0002Ah]
pfnPolygonIndj	equ	[dword ptr 0002Ch]
OFF_pfnPolygonIndj	equ	[word ptr 0002Ch]
SEG_pfnPolygonIndj	equ	[word ptr 0002Eh]
pfnBitBltIndj	equ	[dword ptr 00030h]
OFF_pfnBitBltIndj	equ	[word ptr 00030h]
SEG_pfnBitBltIndj	equ	[word ptr 00032h]
cbIndjMin	equ	00034h
cpfnGxdMin	equ	13
cpfnGsdMin	equ	cpfnGxdMin
cpfnGpdMin	equ	cpfnGxdMin
IFNDEF NOPROCS 
externFP	FEnableMouse
externFP	EnableKeyboard
externFP	PollKeyboard
externFP	SetShiftKk
externFP	DisableExtendedKeyboard
externFP	GetCharMap
externFP	MessageBox
externFP	SetDialogCaption
externFP	HiliteDialogAccel
externFP	SetEditText
externFP	GetEditText
externFP	EditWndProc
externFP	SetAlarm
externFP	KillAlarm
externFP	UngetMessage
externFP	PeekMessage
externFP	GetFocus
externFP	SetFocus
externFP	FlushAbort
externFP	SetCapture
externFP	ReleaseCapture
externFP	DispatchMessage
externFP	PostMessage
externFP	SendMessage
externFP	InitMenu
externFP	FEnableMenuBar
externFP	EnableMenu
externFP	EnableMenuItem
externFP	CheckMenuItem
externFP	FMenuItemChecked
externFP	FindMenuItem
externFP	DrawMenubar
externFP	SetCursorBlock
externFP	ScrollBarWndProc
externFP	SetScrollPos
externFP	GetScrollPos
externFP	SetScrollRange
externFP	AddChild
externFP	RemoveChild
externFP	EnableWindow
externFP	GetClientRrc
externFP	SetWindowStyle
externFP	SetWindowSize
externFP	DrawWindow
externFP	DrawBorder
externFP	TextOut
externFP	CharOut
externFP	FillRrc
externFP	BltRrc
externFP	DrawBox
externFP	SaveRrc
externFP	RestoreRrc
externFP	EnableCursor
externFP	MoveCursor
externFP	MoveWindow
externFP	IntersectRect
externFP	UnionRect
externFP	IsRectEmpty
externFP	SetRect
externFP	PtInRect
externFP	CwSizeRrc
externFP	ShadowArc
externFP	ListBoxWndProc
externFP	DoSound
externFP	ClockTicks
externFP	FValidDrive
externFP	SetIsaColor
externFP	GetIsaColor
externFP	SetIsaRgca
externFP	TmcDoDlg
externFP	HcabAlloc
externFP	FreeCab
externFP	FreeCabData
externFP	SzToCab
externFP	SzFromCab
externFP	EnableTmc
externFP	FEnabledTmc
externFP	SetTmcVal
externFP	GetTmcVal
externFP	SetTmcSel
externFP	GetTmcText
externFP	SetTmcText
externFP	SetFocusTmc
externFP	RedisplayListBox
externFP	RedisplayTmc
externFP	SetDefaultTmc
externFP	InitCab
externFP	ReinitCab
externFP	PwndOfListbox
ENDIF ;!NOPROCS 
