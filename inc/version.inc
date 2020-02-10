;
;	Microsoft Confidential
;	Copyright (C) Microsoft Corporation 1991
;	All Rights Reserved.
;

ifndef    TRUE              ; Some modules may want TRUE = 0FFH.
TRUE      EQU 0FFFFh
endif

ifndef    TRUEBYTE
TRUEBYTE  EQU 0FFh
endif

ifndef    FALSE
FALSE     EQU 0
endif

;----------------------------------------------------------------------------
; CONDITIONAL ASSEMBLY OPTIONS
;
; The following assembly variables should be defined to enable
; the corresponding options, or left undefined to disable the
; corresponding options.  Place a semicolon ";" at the beginning
; of the definition line to leave a variable undefined.

; This flag should be defined to include the DblSpace hooks in the
; build of IO.SYS.  It should be undefined for OEMs who choose not
; to purchase DblSpace.

IFNDEF OEMBASE
IFNDEF DBLSPACE_HOOKS
dblspace_hooks equ TRUE
ENDIF
ENDIF

; This flag should be defined to create the ROM executable version of DOS.
; To build the regular version, it should be UNDEFINED.
;
;ROMDOS  equ TRUE

; This flag should be defined to build power management into the DOS BIOS.
;
;POWER	equ TRUE

; This flag should be defined to build the ROMDRIVE driver into the DOS BIOS.
;
;ROMDRIVE equ TRUE

; This flag should be defined to enable support for CMOS boot options.
;
;CMOS    equ TRUE

; This flag should be defined to include the CONFIG.SYS processing code
; in DOS.  Remove this definition to build DOS without the config.sys
; processing code.
;
CONFIGPROC equ TRUE

; This flag should be defined to keep the data stored in the CDS
; created at startup.  The default behaviour is to rebuild the CDS
; when each device is initialized.  Defining this allows device drivers
; to make lasting changes to the CDS structure.
;
;	BUGBUG -- 16 Nov 92 -- chuckst -- COPYCDS was desirable for
;	DblSpace, but it broke SuperStore (DR) compression's SWAP
;	function, since it didn't take CDSs into account in its
;	swapping.  It is hereby removed.
;
;COPYCDS equ TRUE

; END OF CONDITIONAL ASSEMBLY OPTIONS
;----------------------------------------------------------------------------


if1
	ifdef romdos
		%out ***** BUILDING ROM EXECUTABLE DOS *****
	endif
endif


; This flag should be defined to enable a special version of the EXEC
; function that will find and execute ROM executable programs.
; For our purposes it is always set the same as ROMDOS
;
ifdef ROMDOS
ROMEXEC equ	TRUE
endif

;
; Use the following switches to control cmacros.inc
;
?PLM       =       0
?WIN       =       0

memS       EQU     1			; Small model
;
; Use the switches below to produce the standard Microsoft version or the IBM
; version of the operating system
;
; The below chart will indicate how to set the switches to build the various
; versions
;
;                     IBMVER          IBMCOPYRIGHT
; --------------------------------------------------------
;  IBM Version     |   TRUE              TRUE
; --------------------------------------------------------
;  ISA Version     |   TRUE              FALSE
; --------------------------------------------------------
;  non ISA Version |   FALSE             FALSE
;
IBMVER          EQU     TRUE
IBMCOPYRIGHT    EQU     FALSE

;smr; removal of bufferflag; BUFFERFLAG	EQU	NOT IBMCOPYRIGHT

;mw 8/23/90 ; removal of ROMVER, superceded by ROMDOS and ROMEXEC, above...
; ROMVER		EQU	FALSE

CPU8086		EQU	FALSE

ifndef	MSVER	
MSVER           EQU     NOT IBMVER
endif
IBM             EQU     IBMVER
;
;
  IF1
	IF IBMVER
	    IF IBMCOPYRIGHT
              %OUT ... IBM version build switch on ...
	    ELSE
              ; %OUT ... ISA version build switch on ...
	    ENDIF
	ELSE
	    IF NOT IBMCOPYRIGHT
              %OUT ... non ISA version build switch on ...
	    ELSE
	      %OUT !!!!!!!!!  VERSION SWITCHES SET INCORECTLY  !!!!!!!!!
	      %OUT !!!!!!!!! CHECK SETTINGS IN INC\VERSION.INC !!!!!!!!!
	    ENDIF
	ENDIF
	IF CPU8086
	    %OUT !!!!!!! 8080 / 8086 Version !!!!!!!
	ENDIF
  ENDIF

;****************************************************************************
;
;		<<< Followings are the DBCS relating Definition >>>
;
;
;	To build DBCS version, Define DBCS by using MASM option via
;	Dos environment.
;
;	ex.		set MASM=-DDBCS
;
;
;
;	To build Country depend version, Define JAPAN, KOREA or TAIWAN
;	by using MASM option via Dos environment.
;
;	ex.		set MASM=-DJAPAN
;			set MASM=-DKOREA
;			set MASM=-DTAIWAN
;
;****************************************************************************


BUGFIX	   EQU	   TRUE


IBMJAPVER  EQU	   FALSE		; if TRUE define DBCS also
IBMJAPAN   EQU	   FALSE		; if TRUE define DBCS also


	ifdef TAIWAN
CSIBIOS    EQU	   FALSE		; BIOS	       CSI
CSIDRV	   EQU	   TRUE 		; Device Drive CSI
CSITSR	   EQU	   FALSE		; TSR	       CSI
	else
CSIBIOS    EQU	   FALSE
CSIDRV	   EQU	   FALSE
CSITSR	   EQU	   FALSE
	endif

;
; Set DBCS Blank constant
;
	ifndef DBCS
DB_SPACE EQU	 2020h
DB_SP_HI EQU	 20h
DB_SP_LO EQU	 20h
	else

	ifdef JAPAN
DB_SPACE EQU	 8140h
DB_SP_HI EQU	 81h
DB_SP_LO EQU	 40h
	endif

	ifdef TAIWAN
DB_SPACE EQU	 8130h
DB_SP_HI EQU	 81h
DB_SP_LO EQU	 30h
	endif

	ifdef KOREA
DB_SPACE EQU	 0A1A1h
DB_SP_HI EQU	 0A1h
DB_SP_LO EQU	 0A1h
	endif

	endif

ifndef altvect				; avoid jerking off vector.inc
ALTVECT    EQU	   FALSE		;Switch to build ALTVECT version
endif

;
	IF1

	ifdef DBCS
%OUT    DBCS Version Build Switch ON

	ifdef JAPAN
%OUT    JAPAN Version Build Switch ON
	endif

	ifdef TAIWAN
%OUT    TAIWAN Version Build Switch ON
	endif

	ifdef KOREA
%OUT    KOREA Version Build Switch ON
	endif

	endif

	ENDIF
