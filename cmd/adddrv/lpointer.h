;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

#define	LP_HIGH(lp) ((WORD)(((unsigned long)(char far *)(lp)) >>16))
#define	LP_LOW(lp) ((WORD)(((unsigned long)(char far *)(lp)) & 0xFFFF))

#define	TO_LP(s,o) ((((DWORD)(s))<<16)|((DWORD)(o)))

#define	TO_PARA(x) (((unsigned)(x)+15)>>4)

