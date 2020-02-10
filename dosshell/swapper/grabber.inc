;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */

;
; NAME
;	GRABBER.INC
;
; DESCRIPTION
;	This file defines various grabber entrypoints, functions, subfunctions
;	and constants shared by the grabbers and winoldap.
;
;	If defined, the following flags inhibit definition of the corresponding
;	symbols:
;
;	NO_STDGRABENTRY
;	NO_EXTGRABENTRY
;	NO_GRABINFO
;	NO_GRABREQUEST
;	NO_LC
;	NO_GRABBUF
;
; AUTHOR
;	Jim Christy
;
; HISTORY
;	1.00	060187	jhc	Epoch
;	1.01	100887	jhc	Added constants (DI_OLIVETTI and ST_SPECGRAB)
;				per Bill Hall's request at Olivetti.
;



;
; Define the offsets from the grabber code segment for the standard
; grabber entrypoints.	Used only by winoldap since grabbers create them.
;
			ifndef	NO_STDGRABENTRY
InquireGrab		=	000h
EnableGrab		=	003h
DisableGrab		=	006h
PromptGrab		=	009h		;OBSOLETE
InquireSave		=	00Ch
SaveScreen		=	00Fh
RestoreScreen		=	012h
InitScreen		=	015h		;OPTIONAL
			endif


			ifndef	NO_EXTGRABENTRY
;
; Define standard subfunction numbers for InquireGrab
;

GRAB_INQUIRETEXT	=	00001h
GRAB_INQUIREGRPH	=	00002h

MAX_STDCALL		=	GRAB_INQUIREGRPH


;
; Define extended subfunction numbers for InquireGrab
;

GRAB_ENDPAINT		=	0FFF4h
GRAB_BEGINPAINT 	=	0FFF5h
GRAB_MARKBLOCK		=	0FFF6h
GRAB_PUTBLOCK		=	0FFF7h
GRAB_GETBLOCK		=	0FFF8h
GRAB_REALIZECOLOR	=	0FFF9h
GRAB_GETVERSION 	=	0FFFAh
GRAB_DISABLESAVE	=	0FFFBh
GRAB_ENABLESAVE 	=	0FFFCh
GRAB_SETSWAPDRIVE	=	0FFFDh
GRAB_GETINFO		=	0FFFEh
GRAB_GETID		=	0FFFFh

MIN_EXTCALL		=	GRAB_ENDPAINT
			endif


			ifndef	NO_GRABINFO
;
; GRABINFO STRUCTURE
;
;	The extended grabber call GRAB_GETINFO fills a structure provided by
;	Winoldap with the current video state information.  This information
;	can then be used by Winoldap to aid in mark/copy/paste operations, to
;	determine the current display adapter in use, etc.  All
;	non-dimensionless quantities are 1-based.
;

GrabInfo		struc
  giDisplayId		db	?		;see below
  giScrType		db	?		;see below
  giSizeX		dw	?		;X raster size in .1mm units
  giSizeY		dw	?		;Y raster size in .1mm units
  giCharsX		db	?		;# X char cells (columns)
  giCharsY		db	?		;# Y char cells (rows)
  giMouseScaleX 	db	?		;X transform for MS-MOUSE
  giMouseScaleY 	db	?		;Y transform for MS-MOUSE
  giReserved		db	38	dup (?)
GrabInfo		ends

GRABINFOLEN		=	SIZE GrabInfo


;
; Define codes for giDisplayId field in GrabInfo structure
;

DI_CGA			=	000h
DI_EGA			=	001h
DI_HERCULES		=	002h
DI_MULTIMODE		=	003h
DI_VGA			=	004h
DI_OLIVETTI		=	005h


;
; Define bitmaped codes for giScrType field in GrabInfo structure
;

ST_TEXT 		=	00000000b
ST_GRPH 		=	00000001b
ST_LARGE		=	00000010b
ST_SPECGRAB		=	00000100b
			endif


			ifndef	NO_GRABREQUEST
;
; GRABREQUEST STRUCTURE
;
;	Upon entry, all block operations expect es:di to point to a
;	GrabRequest structure of the format below.  Since not all fields are
;	used by some functions, field usage is detailed in the header for each
;	function.
;

GrabRequest		struc
  grlpData		dd	?		;long ptr to I/O buffer
  grXorg		db	?		;x origin (unsigned)
  grYorg		db	?		;y origin (unsigned)
  grXext		db	?		;x extent (unsigned)
  grYext		db	?		;y extent (unsigned)
  grStyle		db	?		;style flags
  grChar		db	?		;char code for fill ops
  grAttr		db	?		;attribute for fill ops
GrabRequest		ends

GRABREQUESTLEN		=	SIZE GrabRequest


;
; Define codes for fScreenOps field of GrabRequest.Style
;

SCR_OP_MASK		=	00000111b

F_BOTH			=	000h		;fill w/ single char and attr
F_CHAR			=	001h		;fill w/ single char only
F_ATTR			=	002h		;fill w/ single attr only
C_BOTH			=	003h		;copy chars and attrs from lpData
C_CHAR			=	004h		;copy chars only from lpData
C_ATTR			=	005h		;copy attrs only from lpData
C_CHAR_F_ATTR		=	006h		;copy chars from lpData, fill w/ attr
C_ATTR_F_CHAR		=	007h		;copy attrs from lpData, fill w/ char


;
; Define bitmapped codes for fFormat field of GrabRequest.Style
;

FORMAT_MASK		=	10000000b

FMT_NATIVE		=	00000000b
FMT_OTHER		=	10000000b


;
; Define error codes for block operations
;

ERR_UNSUPPORTED 	=	0FFh
ERR_BOUNDARY		=	0FEh
			endif


			ifndef	NO_LC
;
; Define logical colors for GRAB_REALIZECOLOR
;

LC_SELECTED		=	000h
LC_UNSELECTED		=	001h
LC_GRAYED		=	002h
LC_SELECTGRAY		=	003h
LC_TITLEBAR		=	004h
LC_SYSTEMUNSELECT	=	005h
LC_SYSTEMSELECT 	=	006h
LC_APPTITLE		=	007h
LC_MNEMONIC		=	008h

MAX_LC_COLOR		=	LC_MNEMONIC
			endif


			ifndef	NO_GRABBUF
;
; GRAB BUFFER STRUCTURE
;

GrabSt			struc
  gbType		dw	?		;see below
  gbSize		dw	?		;length (not including first four bytes)
  gbWidth		dw	?		;width of bitmap in pixels
  gbHeight		dw	?		;height of bitmap in raster lines
  gbPlanes		dw	?		;# of color planes in the bitmap
  gbPixel		dw	?		;# of adjacent color bits on each plane
  gbWidth2		dw	?		;width of bitmap in 0.1 mm units
  gbHeigh2		dw	?		;height of bitmap in 0.1 mm units
  gbBits		dw	?		;the actual bits
GrabSt			ends

GRABSTLEN		=	SIZE GrabSt


;
; Define codes for gbType field of GrabSt
;

GT_TEXT 		=	1
GT_OLDBITMAP		=	2
GT_NEWBITMAP		=	3
GT_RESERVED4		=	4
GT_RESERVED5		=	5


;
; Define alias fieldname in case of GT_OLDBITMAP
;

gbOldBits		equ	word ptr gbWidth2
			endif

