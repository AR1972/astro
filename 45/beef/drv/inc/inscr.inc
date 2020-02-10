
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
