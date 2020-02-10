
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
