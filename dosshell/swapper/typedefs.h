;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/*--------------------------------------------------------------------------*/
/*  General Purpose Defines															    */
/*--------------------------------------------------------------------------*/

#define NULL		    0
#define FALSE		    0
#define TRUE		    1

#define FAR		    	 far
#define NEAR		    near
#define LONG		    long
#define VOID		    void
#define PASCAL		    pascal


#define MAKELONG(a, b)	    ((LONG)(((WORD)(a)) | ((DWORD)((WORD)(b))) << 16))
#define LOWORD(l)	    ((WORD)(l))
#define HIWORD(l)	    ((WORD)(((DWORD)(l) >> 16) & 0xFFFF))
#define LOBYTE(w)	    ((BYTE)(w))
#define HIBYTE(w)	    ((BYTE)(((WORD)(w) >> 8) & 0xFF))

typedef int		   	  BOOL;
typedef unsigned char  BYTE;
typedef unsigned int	  WORD;
typedef unsigned long  DWORD;
typedef char near	    *PSTR;
typedef char near	    *NPSTR;
typedef char far	    *LPSTR;
typedef BYTE near	    *PBYTE;
typedef BYTE far	    *LPBYTE;
typedef int near	    *PINT;
typedef int far 	    *LPINT;
typedef WORD near	    *PWORD;
typedef WORD far	    *LPWORD;
typedef long near	    *PLONG;
typedef long far	    *LPLONG;
typedef DWORD near	 *PDWORD;
typedef DWORD far	    *LPDWORD;
typedef void far	    *LPVOID;

