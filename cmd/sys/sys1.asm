 ;******************************************************************************
;
;	Microsoft Confidential
;	Copyright (c) Microsoft Corporation 1991
;	All Rights Reserved
;
;  Change Log:
;
;    Date    Who   #			  Description
;  --------  ---  ---  ------------------------------------------------------
;  03/26/90  EGH  C00  Cleaned up build by removing unused variables, declaring
;		       functions properly, changing long JMPs to short JMPs,
;		       etc.
;  03/26/90  EGH  C01  Problem fixed - if a source path was specified with an
;		       ending backslash (i.e. SYS A:\ C:), the message "Insert
;		       system diskette in drive A:  Press any key to continue"
;		       would be displayed endlessly with every keypress.  Fix
;		       is to insure that all paths have an ending backslash and
;		       remove the backslash from the file names.  STR #1976.
;  03/26/90  EGH  C02  Problem fixed - attempting to SYS to a non-FAT file
;		       system would result in the message "Not able to SYS to
;		       xxx file system" with only the first three letters of
;		       the file system name displayed.	Fix is to allow a
;		       maximum of 8 characters to be displayed.
;  03/27/90  EGH  C03  Problem fixed - specifying a source path longer than 53
;		       characters resulted in an error message.  Fix is to
;		       increase the buffer size to support the maximum DOS
;		       path size of 63 characters and change the associated
;		       code.
;  03/28/90  EGH  C04  Problem fixed - source and target drives being the same
;		       (i.e. SYS A: A:) did not generate an error and made the
;		       disk unbootable.  Fix is to add a check to see if the
;		       source drive is the same as the target drive and issue
;		       an appropriate error message.
;  03/28/90  EGH  C05  Problem fixed - an ASSIGNed source drive, specified or
;		       default, that is really the target drive did not
;		       generate an error and made the disk unbootable.	Fix is
;		       to translate the source drive before comparing it to
;		       the target drive. STR #1977
;  03/29/90  EGH  C06  Problem fixed - the buffer for the system files was
;		       being overwritten when a cluster needed to be moved
;		       and the cluster size was greater than 4K.  Fix is to
;		       use the system file buffer for cluster transfers and
;		       load in the system files afterwards.
;  11/21/90  DLB  M008 Made TargDrv public for use in updating CDS.
;  01/09/91  DLB  M009 Do not use COMSPEC unless it specifies COMMAND.COM.
;  01/09/91  DLB  M010 Send "System transferred" msg to STDOUT instead of
;		       STDERR.
;  02/05/91  DLB  M011 Fix computation of Transfer buffer size (cbBuf).
;                      Display error message and exit if buffer too small.
;
;  05/22/91  MD   M012 Check for ROM DOS, exit if found
;
;  08/05/91  MD        Removed M012 changes.
;
;******************************************************************************
	TITLE	SYS-1-	Program
	include version.inc
	include find.inc
	include dpb.inc
	include syscall.inc
	INCLUDE SYSHDR.INC
	page	80,132

false	= 0

DATA	SEGMENT PARA PUBLIC

	public	TargDrvNum, TargSpec, bio_owns_it, DOS_VER
	public	packet, packet_sectors, packet_buffer
        public  TargDrv                 ;M008

	extrn	THIS_DPB:dword, BUF:word, DIR_SECTOR:word, first_dir_sector:word

;			$SALUT (4,25,30,41)

; DOS_VER was formerly used to allow SYS to run on DOS < 4.0.
; Now we just reject DOS less than the current version.
; References to DOS_VER haven't been cleaned up, though.
DOS_VER 		DB   0		; DOS Version - 0 = current
					;		
DEFALT			DB   0		; Default Drive (source - NUMBER
TargAttr                DW   1          ; Attributes for target command.com (+R)
TargDrvNum		DB   0		; Target Drive (destination) - NUMBER
TargDrv 		DB   0		; Target Drive (destination) - LETTER
TargSpec		DB   "A:\",0	; z string for target name

;M002 - begin
IF IBMCOPYRIGHT
BIOSName		DB   "A:\IBMBIO.COM",0 ; z string for target name
DOSName 		DB   "A:\IBMDOS.COM",0 ; z string for target name
OTHERBIOSName		DB   "A:\IO.SYS",0
OTHERDOSName 		DB   "A:\MSDOS.SYS",0
ELSE
BIOSName		DB   "A:\IO.SYS",0
DOSName 		DB   "A:\MSDOS.SYS",0
OTHERBIOSName		DB   "A:\IBMBIO.COM",0
OTHERDOSName 		DB   "A:\IBMDOS.COM",0
ENDIF
;M002 - end

SourceBIOSName		LABEL WORD
SourceSpec		DB   "A:"
			DB   "\"					   ;C01
			DB   64 dup (0) 				   ;C03
IF IBMCOPYRIGHT
SourceBIOS		DB   "IBMBIO.COM",0				   ;C01
ELSE
SourceBIOS		DB   "IO.SYS",0 				   ;C01
ENDIF

IF  IBMCOPYRIGHT
NameLen 		equ  $ - SourceBios
ELSE
BiosNameLen		equ  $ - SourceBios
ENDIF

SourceDOSName		DB   "A:"
			DB   "\"					   ;C01
			DB   64 dup (0) 				   ;C03

IF IBMCOPYRIGHT
SourceDOS		DB   "IBMDOS.COM",0				   ;C01
ELSE
SourceDOS		DB   "MSDOS.SYS",0				   ;C01
ENDIF
IF  IBMCOPYRIGHT
ELSE
DosNameLen		equ  $ - SourceDOS
ENDIF

SourceSize		dw   3						   ;C01
Spec_flag		db   0

; M000 - begin
SourceCommandName	DB   "A:\"
			DB   64 dup (0) 				
targ_com		db   "?:"

; M009: The code in Copy_Command() expects the following of command_string:
;       1) The first character must be a '\'.
;       2) "command.com" must be lower case.
;       3) It must be NULL terminated.
command_string		db   "\command.com", 0
LEN_COMMAND_STRING      equ  $-command_string           ;M009

IFDEF DBLSPACE_HOOKS
SourceDblSpaceName      db   "A:\"
                        db   64 dup (0)

; Do not insert anything between the next two data items!
TargDblSpace            db   "A:\"
SourceDblSpace          db   "DBLSPACE.BIN",0
DblSpaceNameLen         equ $ - offset SourceDblSpace
ENDIF

old_psp			dw   0
rdhndle			dw   0
wthndle			dw   -1
lastround		db   0
trying_comspec  	db   0

IBMBIO_LOW		DW   0		;length of IBMBIO on target disk
IBMBIO_HIGH		DW   0
IBMDOS_LOW		DW   0		;length of old IBMDOS on target disk
IBMDOS_HIGH		DW   0

IFDEF DBLSPACE_HOOKS
DblSpace_Low            DW   0          ;length of Dblspace.bin on target
DblSpace_High           DW   0
ENDIF

CommandCom_Low          DW   0          ;length of command.com on target
CommandCom_High         DW   0

Need_Clusters		dw   0
Bytes_Per_Cluster	dw   0
Number_Free_Clusters	dw   0

;	$SALUT	(4,9,17,41)
					;---------------------------------------
					;  SRORAGE FOR COMMAND LINE PARAMETERS
					;---------------------------------------

PARMS	LABEL	WORD
	DW	OFFSET PARMSX		; POINTER TO PARMS STRUCTURE
	DB	0			; NO DELIMITER LIST FOLLOWS
	DB	0			; NUMBER OF ADDITIONAL DELIMITERS

					;---------------------------------------
					;  STRUCTURE TO DEFINE SYS SYNTAX REQUIREMENT
					;---------------------------------------

PARMSX	LABEL	BYTE
PAR_MIN DB	1			; MINIMUM POSITIONAL PARAMETERS = 1    ;AC021;
	DB	2			; MAXIMUM PARAMETERS = 2	       ;AC021;
	DW	OFFSET POS1		; POINTER TO POSITIONAL DEFINITION
	DW	OFFSET POS1		; POINTER TO SAME POSITIONAL DEFINITION;AC021;
	DB	1			; THERE IS 1 SWITCH
	DW	OFFSET SW1		; POINTER TO SWITCH DEFINITION
	DB	0			; THERE ARE NO KEYWORDS IN PRINT SYNTAX

					;---------------------------------------
					;  STRUCTURE TO DEFINE THE POSITIONAL PARAMETER (Drive ID)
					;---------------------------------------

POS1	LABEL	WORD
POSREP	DB	reqd			; MATCH FLAG LOW		       ;AC021;
POSTYP	DB	f_spec + drv_id 	; MATCH FLAG HIGH		       ;AC021;
	DW	0001H			; CAPS BY FILE TABLE
	DW	OFFSET POS_BUFF 	; PLACE RESULT IN POSITIONAL BUFFER
	DW	OFFSET NOVALS		; NO VALUES LIST REQUIRED
	DB	0			; NO KEYWORDS

reqd	equ	0
f_spec	equ	2
drv_id	equ	1
					;---------------------------------------
					;  STRUCTURE TO DEFINE THE /? SWITCH
					;---------------------------------------
	PUBLIC	SW1, SW1_SYN
SW1	LABEL	WORD
	DW	0			; MATCH MASK
	DW	0			; FUNC MASK
	DW	OFFSET SW_BUFF 		; PLACE RESULT IN SWITCH BUFFER
	DW	OFFSET NOVALS		; NO VALUES LIST REQUIRED
	DB	1			; 1 SYNONYM
SW1_SYN	DB	"/?",0			; TEXT OF SYNONYM TO MATCH

					;---------------------------------------
					;  VALUE LIST FOR POSITIONAL
					;---------------------------------------

NOVALS	LABEL	WORD
	DB	0			; NO VALUES

;			$SALUT (4,25,30,41)

					;---------------------------------------
					;  RETURN BUFFER FOR POSITIONAL INFORMATION
					;---------------------------------------
POS_BUFF		LABEL BYTE
POS_TYPE		DB   ?		; TYPE RETURNED
POS_ITEM_TAG		DB   ?		; SPACE FOR ITEM TAG
POS_SYN 		DW   ?		; POINTER TO LIST ENTRY
POS_OFF 		LABEL WORD
POS_DRV_ID		DB   ?		; SPACE FOR DRIVE NUMBER (1=A, 2=B, ect)
			DB   ?		;				       ;AC021;
POS_SEG 		DW   ?		;				       ;AC021;


					;---------------------------------------
					;  RETURN BUFFER FOR SWITCH INFORMATION
					;---------------------------------------
	PUBLIC	SW_BUFF,SW_SYN
SW_BUFF			LABEL BYTE
SW_TYPE			DB   ?		; TYPE RETURNED
SW_ITEM_TAG		DB   ?		; SPACE FOR ITEM TAG
SW_SYN 			DW   ?		; POINTER TO LIST ENTRY
			DD	?

failopen		equ  0		; extended open 'does not exist action
openit			equ  1		; extended open 'exists' action
replaceit		equ  2		; extended open 'exists' action - replace

OPEN_PARMS		label dword

open_off		dw   ?		; name pointer offset
open_seg		dw   ?		; name pointer segment

PACKET			dw   0,0	; CONTROL PACKET		       ;AN001;
packet_sectors		dw   0		; COUNT 			       ;AN001;
PACKET_BUFFER		dw   0,0	; BUFFER ADDRESS		       ;AN001;

					;---------------------------------------
					;  Buffer for IOCtl Get/Set Media
					;---------------------------------------

IOCTL_BUF		LABEL BYTE

IOCtl_Level		DW   0		; INFO LEVEL (SET ON INPUT)
IOCtl_Ser_No_Low	DW   ?		; SERIAL #
IOCtl_Ser_No_Hi 	DW   ?		; SERIAL #
IOCtl_Vol_ID		DB   "NO NAME    " ; VOLUME LABEL - 11 bytes
IOCTL_File_Sys		DB   8 DUP(' ') ; FILE SYSTEM TYPE

IOCTL_Ser_Vol_Sys	equ  $ - IOCtl_Ser_No_Low
file_sys_size		equ  $ - IOCtl_File_Sys

File_Sys_End		LABEL WORD

			db   0		; safety

fat_12			DB   "FAT12   " ; 12 bit FAT
FAT_len 		equ  $ - fat_12
fat_16			DB   "FAT16   " ; 16 or 32 bit FAT

					;---------------------------------------
					; SUBLIST for Message call
					;---------------------------------------

.xlist
			include sysmsg.inc

			MSG_UTILNAME <SYS> ;				       ;AN000;

			MSG_SERVICES <MSGDATA> ;			       ;AN000;
.list

SUBLIST 		LABEL DWORD

			DB   sub_size	; size of sublist
			DB   0		; reserved
insert_ptr_off		DW   ?		; pointer to insert - offset
insert_ptr_seg		DW   ?		; pointer to insert - segment
insert_number		DB   1		; number of insert
			DB   Char_Field_ASCIIZ ;type flag
insert_max		DB   3		; maximum field size (limited to 3)
					;   - this handles - SYS
					;   - and - D:\
			DB   1		; minimum field size
			DB   " "	; pad character

sub_size		equ  $ - SUBLIST ; size of sublist

sys_ptr 		db   "SYS",0

bio_owns_it		db   0
EntryFree		db   0		; for create file

IFDEF DBLSPACE_HOOKS
NoDblSpace              db   FALSE      ; flag is Dblspace must be copied
ENDIF

;*** WARNING ***
; KEEP THE FOLLOWING ITEMS IN THE EXACT ORDER BELOW!!!
DOSEntFree		DB   1
BIOSEntFree		DB   1

Xfer_data		STRUC

InFH			DW   ?		; file handle of source
LenLow			DW   ?		; 32-bit length of source
LenHigh 		DW   ?
FTime			DW   ?		; place to store time of write
FDate			DW   ?		; place to store date of write
OutFH			DW   ?		; fh of destination
BufPos                  DW   ?          ; start of this file in xfer buffer
BufferedSize            DW   ?          ; count of bytes read into buffer
OutFileName             DW   ?          ; pathname of destination file

Xfer_data		ENDS

; following are instances of the xfer_data structure

BIOSInFH		DW   -1		; file handle of source BIOS
BIOSLenLow		DW   0		; 32-bit length of BIOS
BIOSLenHigh		DW   0
BIOSTime		DW   2 DUP (?)	; place to store time of BIOS write
BIOSOutFH		DW   -1		; fh of BIOS destination
BIOSBufPos 		DW   0  	; start of file in xfer buffer
BIOSBufferedSize        DW   0          ; count of bytes read into buffer
BIOSOutFileName         DW   offset BIOSName

DOSInFH 		DW   -1		; file handle of source DOS
DOSLenLow		DW   0		; 32-bit length of DOS
DOSLenHigh		DW   0
DOSTime 		DW   2 DUP (?)	; place to store time of DOS write
DOSOutFH		DW   -1		; fh of DOS destination
DOSBufPos 		DW   0  	; start of file in xfer buffer
DOSBufferedSize         DW   0          ; count of bytes read into buffer
DOSOutFileName          DW   offset DOSName

IFDEF DBLSPACE_HOOKS
DblSpaceInFH 		DW   -1		; file handle of source DblSpace
DblSpaceLenLow		DW   0		; 32-bit length of DblSpace
DblSpaceLenHigh		DW   0
DblSpaceTime 		DW   2 DUP (?)	; place to store time of DblSpace write
DblSpaceOutFH		DW   -1		; fh of DblSpace destination
DblSpaceBufPos 		DW   0  	; start of file in xfer buffer
DblSpaceBufferedSize    DW   0          ; count of bytes read into buffer
DblSpaceOutFileName     DW   offset TargDblSpace
ENDIF

; array of pointers to the xfer_data structures.  Last field of 0 is
; the terminator.  Loops that use this array check for this terminator
; as loop end.
IFDEF DBLSPACE_HOOKS
; If DblSpace.bin isn't going to be copied, the entry for it (FHArrayDS) will
; be cleared, so the loop termination comes after the DOS file is copied.
ENDIF

FHArray                 dw   offset BIOSInFH
                        dw   offset DOSInFH
IFDEF DBLSPACE_HOOKS
FHArrayDS               dw   offset DblSpaceInFH
ENDIF
                        dw   0
IF IBMCOPYRIGHT
FCBDOS			DB   "IBMDOS  COM"
FCBBIO			DB   "IBMBIO  COM"
ELSE
FCBDOS			DB   "MSDOS   SYS"
FCBBIO			DB   "IO      SYS"
ENDIF

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   The following is a Extended FCB
ExtFCB			db   0FFh
			db   5 dup (0)
			db   DOS_volume_atrib
ExtFCB_Drive		db   0
ExtFCB_Name		db   "???????????"
			db   24 dup (0)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

DOS_BUFFER		DB   80h DUP (?)
cbBuf			DW   ?		; number of bytes in buffer
pDOS			DW   ?		; offset of beginning of DOS in buffer
pDOSEnd 		DW   ?		; offset of end of DOS in buffer


public			boot
BOOT			LABEL BYTE
.xlist
			INCLUDE BOOT.INC
.list
					;
					; Following structure used by Generic IOCTL call Get Device Parameters to get
					; the BPB of a hard disk. It 'overflows' into area of BUF.
					;
DeviceParameters	a_DeviceParameters <1,DEV_HARDDISK>

DATA			ENDS

CODE			SEGMENT PARA PUBLIC

			EXTRN SYSLOADMSG:near, SYSDISPMSG:near, SYSPARSE:near
			EXTRN Data_Space:WORD, Find_DPB:near,
			EXTRN Move_DIR_Entry:near, Direct_Access:near
                        EXTRN Find_Path_In_Environment:near
                        EXTRN Search:near, Path_Crunch:near
                        EXTRN Find_Comspec_In_Environment:near
IFDEF NEED_CLUSTER_2
                        EXTRN Free_Cluster:near
ENDIF

			BREAK <SYS - Main>
;******************* START OF SPECIFICATIONS ***********************************
;Routine name:	Main
;*******************************************************************************
;
;Description: Main control routine. Subroutines are structured so that they
;	      will pass back an error return code (message number) and set
;	      the fail flag (CF) if there was a fatal error.
;
;	NOTES:
;
;  1 -	This program uses its own internal stack.  The stack space provided
;	by DOS is used as an input buffer for transfering IBMBIO and IBMDOS.
;
;	SYS is linked with the CODE segment followed by the DATA segment. The
;	last symbol in DATA is BUF. It marks the end end of data and the
;	start of the BUFfer.  The BUFfer extends from here to SP.  The first
;	6.5Kb (13 sectors) in BUFfer are used for up to 12 sectors of the FAT
;	or the directory. In Main, the remaining space is set
;	as follows:
;		      cdBuf = SP - ( FAT_BUF + BUF )
;
;  2 -	The main line program calls 1 routine that loops until specific
;	requirements are met. It is:
;			Get_System_Files - if default drive has replaceable
;					   media this routine loops until
;					   a diskette with the correct system
;					   files is inserted.
;
;Called Procedures: Init_Input_Output
;		    Validate_Target_Drive
;		    Get_System_Files
;		    Check_SYS_Conditions
;		    Do_SYS
;		    Message
;
;Input: Command line input in PSP
;
;Ouput: no error - System transfered to target media
;	   error - Appropriate error message displayed
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Main
;
;	setup messages and parsing (CALL Init_Input_Output)
;	if there is no error and
;		verify target drive is valid (CALL Validate_Target_Drive)
;	if there is no error and
;		get system files loaded (CALL Get_System_Files)
;	if there is no error and
;		verify target drive is SYSable (Check_SYS_Conditions)
;	if there is no error and
;		perform SYS operation (CALL Do_SYS)
;	if no error and
;		clean up loose ends (CALL Do_End)
;	if no error
;		load return code (System transfered)
;	endif
;	display message (CALL Message)
;	ret
;
;	END  Main
;
;******************-  END  OF PSEUDOCODE -**************************************

			ASSUME CS:CODE,DS:NOTHING,ES:NOTHING

			ORG  80H

PSP_PRAM		DB   128 DUP(?)

START:			JMP  BEGIN

			DB   " - SYS - Utility "
			DB   01Ah

			even

			db   510 dup(0) ; stack

EOS			EQU  BYTE PTR $

			DW   0		; RETURN OFFSET


public			Begin

BEGIN			PROC NEAR

   mov	ax,OFFSET Data_Space
   add	ax,15				; round up to next segment
   mov	cl,4				; convert to segment value
   shr	ax,cl
   mov	cx,ds
   add	ax,cx				; generate DATA segment value
   mov	ds,ax

   ASSUME DS:DATA,ES:NOTHING

   mov  ax,  es
   mov  old_psp, ax

   mov	cbBuf,sp			; M011: Temporarily save SP.
   mov	sp,OFFSET EOS			; set up internal stack

   mov	dx,OFFSET DOS_BUFFER		; set up DTA
   mov	ah,SET_DMA
   INT	21h

   call Init_Input_Output		; setup messages and parsing	
   jc   $$if1
;
; close reserved file handles 0 and 3-4 so we can reuse them.  Leave
; 1 & 2 open for our output messages.
;
; Don't close file handle 0, leave it open so the standard input
; remains available.
;
;  mov	ah,CLOSE
;  xor	bx,bx
;  int	21h				; Close stdin

   mov  ah,CLOSE
   mov  bx,3
   int  21h                             ; Close stdaux

   mov  ah,CLOSE
   inc  bx
   int  21h                             ; Close stdprn

;M011
; Ensure that a Transfer buffer of at least XFER_BUF exists beyond
; CODE + DATA + FAT_BUF (FAT sector buffer).  (This check MUST be done
; after messages are setup in Init_Input_Output.)
; If we are not supporting relocating the owner of cluster 2, we don't
; need to include the FAT_BUF space.
;
   mov  ax,(util_B shl 8) + no_mem	; Return code = Insufficient memory.
   mov  cx,(OFFSET Data_Space)          ; CX = End of code.
IFDEF NEED_CLUSTER_2
   add	cx,(OFFSET BUF) + FAT_BUF + XFER_BUF ; CX = End of minimum buffer.
ELSE
   add  cx,(OFFSET BUF) + XFER_BUF
ENDIF
   jc   $$if1                           ; Error return if buffer too small.
   sub  cbBuf,cx                        ; cbBuf = SP - End of min. buffer.
   jb   $$if1                           ; Error return if buffer too small.
   add  cbBuf,XFER_BUF                 	; cbBuf = Transfer buffer size.
;M011

   call Validate_Target_Drive		; verify target drive is valid	
   jc   $$if1

   call Get_System_Files		; get system files loaded	
   jc   $$if1

   call Check_SYS_Conditions		; verify target drive is SYSable
   jc   $$if1

   call Do_SYS				; perform SYS operation 	
   jc   $$if1

   call Do_End				; clean up loose ends		
   jc   $$if1

   call Copy_Command                    ; copy command.com
   jc   $$if1

   mov  ax,(util_B shl 8) + done

$$IF1:
   push ax				;Save message number		
   mov  ah,DISK_RESET                   ; reset the disks to make sure
                                        ; our writes get flushed out
   int  21h
   pop  ax
   push ax
   call Message 			; display message		
   pop	ax				;Retrieve message number	

   cmp	ax,(util_B shl 8) + done 	;Was it successful ?		
   jne  $$if3

   xor  ax,ax           	 	;  set return code = zero	
   jmp  short   $$en3

$$IF3:
   mov   ax,error_rc	;  set return code = error	     ;AN999;APAR;RussW

$$EN3:
   mov	ah,exit 			; just set function - RC set by MAIN
   int	21h				; if version is < DOS 2.0 the int 21

   ret					; ret if version < 2.00

BEGIN ENDP

; M000 - begin
;**************************************************************************
;Routine name: Copy_Command
;
; Function: Copy command.com onto the target disk.  Search hierarchy for
; command.com is 1) if they specify a source directory for system files,
; look ONLY there.  Else 2) if boot disk == source disk, try the command.
; com specified by comspec.  If there isn't one, or boot disk != source
; then 3) look in the root directory.
;
; Returns carry clear if successful, carry set if not
;
;**************************************************************************
Copy_Command  Proc  Near
public  Copy_Command

; set correct drive in targ_com
	mov	al, TargDrv
	mov	targ_com, al

;M002 - begin
;
; Remove the opposite DOS if present. The opposite DOS are the files
; IBMBIO.COM and IBMDOS.COM for Microsoft compilation and IO.SYS and
; MSDOS.SYS for IBM compilation. Not that command.com is untouched
; but will probably be replaced by Copy_command. This is done after
; the correct DOS files have been copyied but before copying command.com
;
	mov	byte ptr OTHERBIOSName,al
	mov	byte ptr OTHERDOSName,al

	lea	dx,OTHERBIOSName		; first process IBMBIO.COM (IO.SYS)

CC_Process_one_other_DOS:

	mov	ax,(CHMod shl 8)+1		; set attribute
	xor	cx,cx				; to no attribute at all
	int	21h
	jc	CC_one_processed		; if no can do, just to bad
						; (in all likelihood the file
						;  is just not there)

	mov	ah,Unlink			; Delete file
	int	21h				; no error check

CC_one_processed:
	lea	ax,OTHERDOSName			; get address of IBMDOS.COM (MSDOS.SYS)
	cmp	ax,dx				; is it the one we just processed?
	jz	CC_Other_DOS_removed		;    if so we're done
	mov	dx,ax				;    if not let's go do it
	jmp	SHORT CC_Process_one_other_DOS

CC_Other_DOS_removed:

;M002 - end

find_source:
; if user specified a source directory for system files, we look for
; command.com there and only there
	test	Spec_Flag, 1
	jz 	try_comspec

; copy the source path into SourceCommandName
	mov	si,  OFFSET SourceSpec
	mov	di,  OFFSET SourceCommandName
	xor	al,  al
	call	string_len
	push 	cx
	rep	movsb

; backtrack to the '\', since tail end of SourceSpec is "io.sys".  now
; di is positioned so "command.com" gets copied over "io.sys"
	std
	mov 	al,  '\'
	pop	cx
	inc	cx			; in case we have to go all the way
					;    to start of string
ifdef DBCS
	push	bx
	lea	bx,SourceCommandName
@@:
	repne	scasb
	jcxz	@f
	push	di
	inc	di
	call	CheckDBCSTailByte
	pop	di
	jz	@b
@@:
	pop	bx
else
	repne	scasb
endif
	cld
	inc	di
	jmp	short  add_com_to_source

try_comspec:
; if the source disk == boot disk, we try to find command.com based on
; comspec.  if this fails we will look for command.com in the root directory
; of the source drive

; see if the source disk == boot disk
	mov 	ax, 3305h
	int 	21h
	jc	look_in_root

	cmp	dl,  defalt
	jne	look_in_root

;M009
; get the path COMSPEC points to
 	mov	es, old_psp
	mov	es, es:[002ch]   		; get ENV seg ptr
	call	Find_Comspec_in_Environment	; es:di -> first byte after
						;    the "COMSPEC="
	jc	look_in_root                    ; Jump if no COMSPEC found.

	push	ds				; Save ds.
        push    di                              ; es:di -> COMSPEC string.

; verify that COMSPEC specifies "COMMAND.COM".

        push    ds                              ; Exchange ds, es.
        push    es
        pop     ds
        pop     es

        mov	si,di				; ds:si -> COMSPEC string.
        xor     al,al                           ; NULL terminator char.
        call    string_len                      ; cx = COMSPEC string len.
        push    cx
        sub     cx,LEN_COMMAND_STRING-1
        jae	cc120	                  	; Jump if COMSPEC long enough.

cc100:  pop     cx                              ; Restore ds; don't care di,cx.
        pop     di
        pop     ds
        jmp     short look_in_root              ; COMSPEC is not COMMAND.COM.

cc120:  add     si,cx                           ; ds:si -> first character of
                                                ;  string to verify.
	mov     di,OFFSET command_string
        inc     di                              ; es:di -> "command.com",0
        mov     cx,LEN_COMMAND_STRING-2         ; Ignore '\' and NULL.

; Perform case insensitive compare of string at es:[di] ("command.com",0) to
; string at ds:[si] (COMSPEC).

cc140:	mov     al,es:[di]
        sub     al,ds:[si]
        je      cc160                           ; Jump if match.
        jb      cc100                           ; Jump if match failed (since
                                                ;  lower case > upper case).
        cmp     al,'a'-'A'                      ; Upper case?
        jne     cc100                           ;  No, jump: match failed.
cc160:  inc     si                              ; Setup for next character.
        inc     di
        loop    cc140                           ; Fall thru if string matches.

; Copy the COMSPEC string to SourceCommandName.

        pop     cx                              ; cx = COMSPEC string length.
	pop	si                              ; ds:si -> COMSPEC string.
	mov 	di, OFFSET SourceCommandName    ; es:di -> dest. for string.
	rep  	movsb
;M009
	pop	ds				; restore ds
	mov	trying_comspec, TRUE
	jmp	short  	find_com

look_in_root:
	push	ds				; set es == ds
	pop	es
	mov  	di, OFFSET SourceCommandName

add_com_to_source:
; add the string "\command.com" to tail of SourceCommandName
	mov	si,  OFFSET command_string		
	cmp  	byte ptr [di],  '\'		; if path already has '\'
	jne  	add_slash		
	inc  	si				;    set SI -> "command.com"
	inc  	di

add_slash:
	xor  	al,  al
	call 	string_len
	rep  	movsb				; now add command.com to
						;  SourceCommandName

; Now try to find COMMAND.COM in the correct directory
find_com:
	mov   	dx, OFFSET SourceCommandName
	xor   	cx, cx
	mov   	ah, 4eh
	int   	21h
	jnc   	go_read
	cmp	trying_comspec, TRUE
	mov	trying_comspec, FALSE
	je	look_in_root
	jmp   	print_errmsg

;M003 - begin
go_read:
	mov	ax, 3d00h
	mov	dx, OFFSET SourceCommandName
	int	21h
	mov	rdhndle, ax
	jnc	read_more
	jmp	print_errmsg

; open the target file for writing

; while (there are more bytes to transfer)
;   read in as much as we can and write it out
read_more:
	mov	ah, 3fh
	mov	bx, rdhndle
	mov	cx, cbBuf
	mov	dx, (OFFSET BUF)
	int	21h
	jc	close_read
	cmp	ax, cx
	je	dont_flag
	mov	lastround, TRUE

dont_flag:
	cmp	wthndle,-1
	jnz	got_write_handle

	push	ax
;
; before we create the new command.com file, get its current attribute set,
; so we can restore it later, and also clear any read-only attribute.
;
        mov     ax, 4300h                       ; get attribute
        mov     dx, OFFSET targ_com
        int     21h
        jc      clear_attr                      ; don't save if error
        mov     TargAttr, cx                    ; save current attribute

clear_attr:
        mov     ax, 4301h                       ; set new attribute
        xor     cx,cx                           ; normal file, writable
        int     21h

	mov	ah, 3ch
	xor	cx, cx
	mov	dx, OFFSET targ_com
	int	21h
	mov	wthndle, ax
	pop	ax
	mov	dx, (OFFSET BUF)
	jc	close_read

got_write_handle:
	mov	cx, ax
	mov	ah, 40h
	mov	bx, wthndle
	int	21h
	jc	close_write
	cmp	ax,cx
	stc
	jne	close_write

no_prob:
	cmp	lastround, TRUE
	jne 	read_more

; set the date & time stamps of target command.com == source
	mov	ax,  5700h
	mov	bx,  rdhndle
	int	21h
	jc	no_date_and_time
	mov	al,  1
	mov	bx,  wthndle
	int	21h

no_date_and_time:
	clc				; we will later test for carry,
					; but if carry was set in date&time
					; stuff, it doesn't count, clear it
; close the file handles
close_write:
	pushf				; first save flags so we can
					;  tell if there was previous error
	mov	ah, 3eh
	mov	bx, wthndle
	int	21h
	popf

close_read:
	pushf				; first save flags so we can
					;  tell if there was previous error
	mov	ah, 3eh
	mov	bx, rdhndle
	int	21h
;
; reset original attributes of command.com
;
        mov     ax, 4301h               ; set attributes
        mov     cx, TargAttr
        mov     dx, OFFSET targ_com
        int     21h

	popf

	jnc	alldone

;M003 - end
	
; there was a previous problem, so delete <target>:\command.com
; turn off that read-only attribute we just set first

        mov     ax,4301h
        xor     cx,cx
        mov     dx,OFFSET targ_com
        int     21h

	mov	ah,  4eh
	xor	cx,  cx
	int	21h
	jc	print_errmsg 			; it never got created

	mov	ah, 41h
	int	21h

print_errmsg:
        mov     ax,(util_B shl 8) + CMD_NOT_PROCESSED
        stc                                     ; signal error

alldone:
	ret

Copy_Command  endp

;***************************************************************************
; string_len expects ds:si to point to the desired string, and al to contain
; the desired terminator character.  it returns the length of the string
; INCLUDING a terminating NUL.
;***************************************************************************
string_len 	proc	near

; save es:di because we  trash them
	push	es
	push	di

; set es:di = ds:si
	push	ds
	pop	es
	mov	di, si	

; find the terminator character somewhere in the first 256 bytes
	mov	cx, 100h
	repne	scasb
	jne	finis

; set CX equal to string length
	sub	cx, 100h
	neg	cx
	
finis:	pop	di
	pop	es
	ret

string_len	endp


   BREAK <SYS - Init_Input_Output >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Init_Input_Output
;*******************************************************************************
;
;Description: Initialize messages and Parse command line.
;
;Called Procedures: Preload_Messages
;		    Parse_Command_Line
;
;Input: PSP command line at 81h and length at 80h
;
;Output: no error  - CF = 0	  AX = 0
;	    error  - CF = 1	  AX = return code (message #)
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Init_Input_Output
;
;	load messages (CALL Preload_Messages)
;	if no error
;	    get DOS version
;	    if not = current and
;	    set not current flag
;	    if not = current - 1
;		load incorrect DOS version message
;		set fail flag
;	    else
;		if no error and
;			parse the saved command line (CALL Parse_Command_Line)
;		if no error
;			load return code (success)
;		endif
;	    endif
;	endif
;	ret
;
;	END  Init_Input_Output
;
;******************-  END  OF PSEUDOCODE -**************************************

public Init_Input_Output

   Init_Input_Output PROC NEAR

command_line		  equ	 081H	; offset of command line in PSP    ;C00

       call SysLoadMsg			; preload all error messages	       ;AN000;

                			; if error  - set to Utility	       ;AN000;
       JNC    $$IF6
       mov  ah,0bh		        ;				       ;AN000;
       JMP SHORT $$EN6
$$IF6:
       mov  ax,(GET_VERSION shl 8)      ;				       ;AN019;
       int  21h 		        ;				       ;AN019;
       xchg al,ah		        ;				       ;AN019;
       cmp  ax,(major_version shl 8) + minor_version ;			       ;AN019;
       je   $$EN10

$$IF10:
       mov ax,(util shl 8) + DOS_error ;				       ;AN019;
       stc				;				       ;AN019;
$$EN10:
                  			; no error and			       ;AN000;
       JC $$IF15
       xor  cx,cx			; zero out # of parms processed so far ;AN000;
       mov  si,command_line		; move here to loop thru twice	       ;AN000;
       call Parse_Command_Line		; parse the saved command line	       ;AN000;
              				; no error			       ;AN000;
       JC $$IF15
       mov al,noerror	        	; load return code (success)	       ;AN000;

$$IF15:
$$EN6:
       ret					;				       ;AN000;

   ENDPROC Init_Input_Output

   BREAK <SYS - Parse_Command_Line >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Parse_Command_Line
;*******************************************************************************
;
;Description: Parse the command line. Check for errors, loading return code and
;	      setting fail flag if found. Use parse error messages except in
;	      case of no parameters, which has its own message.
;
;Called Procedures: SysParse
;
;Input: None
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = return code (Parse error + error #)
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Parse_Command_Line
;
;	parse command line (Call Do_Parse)
;	if parse error
;		call GetError to find out what happened
;		(fail flag set)
;	else
;		if filespec found
;			set up to move filespec into SourceBIOSName
;			call Move_It to do the move
;			save size of filespec
;			set source spec flag
;		else
;			call Set_Target to process drive id (only other non error
;		endif
;		turn off filespec as valid input
;		if first parm was NOT a filespec (ie a drive id)
;			turn on optional bit
;		else
;			force required parms to 2
;		endif
;		call Do_Parse
;		if no errors
;			call Set_Target to initialize drive id
;			call Do_Parse to look for EOF or error
;			if eol
;				clear error flag
;			else
;				call Get_Error to see what went wrong
;			endif
;		else
;			if not EOL
;				call Get_Error to see what went wrong
;			else
;				clear error flag
;			endif
;		endif
;	endif
;
;	ret
;
;	END  Parse_Command_Line
;
;******************-  END  OF PSEUDOCODE -**************************************

public Parse_Command_Line

   Parse_Command_Line PROC NEAR
					;---------------------------------------
					; Parse Equates
					;---------------------------------------
eol			  equ	 -1	; Indicator for End-Of-Line	       ;AN000;
noerror 		  equ	 0	; Return Indicator for No Errors       ;AN000;
command_line		  equ	 081H	; offset of command line in PSP        ;AN000;
Syntax_Error		  equ	 9	; PARSE syntax error		       ;AN000;
					;---------------------------------------
					;  Get address of command line
					;---------------------------------------
   push ds				;				       ;AN000;
   pop	es				;				       ;AN000;
   lea	di,PARMS			;				       ;AC021:
   call Do_Parse			;				       ;AC021:
   cmp	ax,0				; did we find our required parm?       ;AN000;
   je   $$if18

   call Get_Error			;				       ;AC021;
   jmp  short  $$en18

$$IF18:
   cmp  POS_TYPE,5			; is it a file spec?		       ;AN021;
   jne  $$if20				; if not, skip this

   push ds				; copy spec into source 	       ;AN021;
   push di				;				       ;AN021;
   push si				;				       ;AN021;
   lea	di,SourceSpec			;				       ;AN021;
   mov	si,word ptr POS_OFF		;				       ;AN021;
   mov	ax,POS_SEG			;				       ;AN021;
   mov	ds,ax				;				       ;AN021;

   ASSUME ds:nothing,es:DATA
   xor	bx,bx				;				       ;AN021;
   call Move_Source			;				       ;AN021;
   pop	si				;				       ;AN021;
   pop	di				;				       ;AN021;
   pop	ds				;				       ;AN021;

   ASSUME ds:DATA,es:nothing
   mov	SourceSize,bx			;				       ;AN021;
   mov	Spec_Flag,1			; set spec flag 		       ;AN021;
   jmp  short  $$en20

$$IF20:
   call Set_Target			; initialize target just in case       ;AN021;
   mov	SourceSpec,al			; save Source Spec		       ;AN000;
					; remember that the colon and size
$$EN20:
   and  POSTYP,drv_id			; off filespec bit - on drive bit      ;AN021;
   cmp  Spec_Flag,0 			; do we have a source spec ?	       ;AN021;
   jne  $$if23

   inc	POSREP				; turn on optional		       ;AN021;
   jmp  short  $$en23

$$IF23:
   inc	PAR_MIN 			; must have the second parm.	       ;AN021;
$$EN23:
   call Do_Parse			;				       ;AN021;
   cmp  ax,0				; no parse errors?		       ;AN000;
   jne  $$if26
	
   call Set_Target			; initialize target		       ;AN021;
   cmp	Spec_Flag,0			; do we have a source spec ?	       ;AN021;
   jne  $$if27
	
   inc  Spec_Flag			; turn it on			       ;AN021;

$$IF27:
   call Do_Parse			; make sure there are no extra parms.  ;AN021;
   cmp	ax,eol				;				       ;AN021;
   jne  $$if29
	
   clc					;				       ;AN021;
   jmp  short  $$en29

$$IF29:
   call Get_Error			;				       ;AN021;
$$EN29:
   jmp  short  $$en26
$$IF26:
   cmp	ax,eol				; is it EOL ?			       ;AN021;
   je   $$if33

   call Get_Error			; error - make sure it makes sense     ;AN021;
   jmp  short  $$en33
$$IF33:
   clc					;				       ;AN021;
$$EN33:
$$EN26:
$$EN18:

   ret					;				       ;AN000;


public Move_Source
   Move_Source PROC NEAR
ifdef DBCS
	mov	dx,di			; save start address
endif
;  $search				;				       ;AN021;
$$DO38:
       lodsb				;				       ;AN021;
       stosb				;				       ;AN021;
       inc  bl				;				       ;AN021;
       cmp  bl,67			; are we past the maximum?	   ;C03
;  $exitif a				;				       ;AN021;
   JNA $$IF38
       mov  ax,(util_B SHL 8) + bad_path ; Invalid path 		       ;AN021;
       stc				;				       ;AN021;
;  $orelse				;				       ;AN021;
   JMP SHORT $$SR38
$$IF38:
       or   al,al			;				       ;AN021;
;  $endloop z				;				       ;AN021;
   JNZ $$DO38
       dec  bl				;				       ;AN021;

ifdef DBCS
	cmp	byte ptr es:[di-2],"\"
	jnz	@f			; if no '\'
	push	bx
	push	di
	mov	bx,dx			; set the start address
	sub	di,2
	call	CheckDBCSTailByte
	pop	di
	pop	bx
	jnz	yes_backslash		; if this is not tail byte
@@:
else
       cmp  byte ptr es:[di-2],"\"      ;Q: Ending backslash?              ;C01
       je   yes_backslash		; Y: Then exit			   ;C01
endif
       mov  byte ptr es:[di-1],"\"      ; N: Add one                       ;C01
       mov  byte ptr es:[di],0		;    replace zero		   ;C01
       inc  bl				;    One byte larger		   ;C01
yes_backslash:				;				   ;C01
       clc				;				       ;AN021;
;  $endsrch				;				       ;AN021;
$$SR38:

   ret					;				       ;AN021;

   ENDPROC Move_Source

   Do_Parse PROC NEAR

   mov	insert_ptr_off,si		; save it in case of error	       ;AN024;
   push cs				;				       ;AN000;
   pop	ds				;				       ;AN000;
   xor	dx,dx				;				       ;AN021;

   ASSUME ds:nothing,es:DATA

   call SysParse			; parse command line		       ;AN000;

   push es				;				       ;AN000;
   pop	ds				;				       ;AN000;

   ASSUME ds:DATA,es:nothing

   call Check_Options_Exit		; may not return...

   ret					;				       ;AN021;

   ENDPROC Do_Parse

   Set_Target PROC NEAR

   mov	al,byte ptr pos_drv_id		; initalize drive id		       ;AN000;
   mov	TargDrvNum,al			; save it for later		       ;AN000;
   mov	ExtFCB_Drive,al 		; save it for finding VOL id	       ;AN000;
   or	al,num_2_letter 		; convert to a drive letter	       ;AC000;
   mov	TargSpec,al			; save it for later		       ;AN000;
   mov	TargDrv,al			;				       ;AC000;
   ret					;				       ;AN021;

   ENDPROC Set_Target

   Get_Error PROC NEAR

   lea	bx,Parse_Ret_Code		; error - make sure it makes sense     ;AN000;
   xlat cs:[bx] 			;				       ;AN000;
   mov	ah,parse_error			; indicate parse error CLASS	       ;AN000;
   stc					; set fail flag 		       ;AN000;
   ret					;				       ;AN021;

   ENDPROC Get_Error

Parse_Ret_Code label byte

   db	0				; Ret Code 0 -			       ;AN000;
   db	1				; Ret Code 1 - Too many operands       ;AN000;
   db	2				; Ret Code 2 - Required operand missing;AC002;
   db	9				; Ret Code 3 - Not in switch list provided ;AC002;
   db	9				; Ret Code 4 - Not in keyword list provided;AC002;
   db	9				; Ret Code 5 - (not used)	       ;AN000;
   db	9				; Ret Code 6 - Out of range specified  ;AN000;
   db	9				; Ret Code 7 - Not in value list provided
   db	9				; Ret Code 8 - Not in string list provided
   db	9				; Ret Code 9 - Syntax error

   ENDPROC Parse_Command_Line

   BREAK <SYS - Check_Options_Exit >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Check_Options_Exit
;*******************************************************************************
;
;Description: Check to see if the parser has just found "/?" on the
;             command line.  If so, display the options help message
;             lines, and exit so the user may try again.
;
;Called Procedures: SysDispMsg
;
;Input: SW_SYN may have been set by the parser
;
;Output: no /? - flags modified, returns to caller
;	 /?    - displays message lines and exits - does not return
;
;Change History: Created	5/01/89 	c-PaulB
;
;******************* END OF SPECIFICATIONS *************************************

public Check_Options_Exit

Check_Options_Exit PROC NEAR

	; See if /? was matched by parser
	; return now if not

	cmp	[SW_SYN], offset SW1_SYN	; was /? specified?
	jne	COE_Return			;  back to caller if not

	; Display the options help message lines.

	mov	ax, MSG_OPTIONS_FIRST		; message to display
	mov	bx, STDOUT			; output handle
	mov	cx, 0				; no substitutions
	mov	dh, UTILITY_MSG_CLASS		; message class
	mov	dl, NO_INPUT			; no input wanted
	mov	si, 0				; no substitution list
COE_Disp_Loop:
	call	SysDispMsg			; display the message
	cmp	ax, MSG_OPTIONS_LAST		; last message?
	je	COE_Disp_Done			;  we're done if so
	inc	ax				; else get next msg
	jmp	short COE_Disp_Loop		;  and go do it
COE_Disp_Done:

	; Exit so the user can try again.

	mov	ax, (exit SHL 8)		; exit function, clear ret code
	int	21h				;  we're outta here!

COE_Return:
	ret

ENDPROC Check_Options_Exit


   BREAK <SYS - Validate_Target_Drive >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Validate_Target_Drive
;*******************************************************************************
;
;Description: Verify that target drive was specified, is not default drive,
;	      is a valid drive letter, and is not a network drive
;
;Called Procedures: Check_Default_Drive
;		    Check_Target_Drive
;		    Check_For_Network
;
;Input: None
;
;Output: no error  - CF = 0	  AX = 0
;	    error  - CF = 1	  AX = return code (message #)
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Validate_Target_Drive
;
;	can't have target as default (CALL Check_Default_Drive)
;	if no error and
;		can't have target as network (CALL Check_For_Network)
;	if no error
;		see if valid drive letter (CALL Check_Target_Drive)
;	ret
;
;	END  Validate_Target_Drive
;
;******************-  END  OF PSEUDOCODE -**************************************

public Validate_Target_Drive

   Validate_Target_Drive PROC NEAR

   call Check_Default_Drive		; can't have target as default         ;AN000;

;  $if	nc,and				; no error and			       ;AN000;
   JC $$IF43

   call Check_For_Network		; can't have target as network         ;AC022;

;  $if	nc				; no error			       ;AN000;
   JC $$IF43

       call Check_Target_Drive		; see if valid drive letter	       ;AC022;

IFDEF DBLSPACE_HOOKS
;; a-emoryh: Running SYS on a DblSpace drive is dangerous, so i'm leaving
;;           this check in.
ENDIF

;; IFDEF DBLSPACE_HOOKS
       jc $$IF43

	; SYS to a DblSpace Compressed Volume File is not supported,
	; so check if that's what the user has requested.

	push	ax
	mov	al, TargDrvNum		; 1 based target drive
	dec	al			;   to 0 based
	call	Get_Host_Drive		; host will != target if CVF
	inc	al			; 1 based
	cmp	al, TargDrvNum		; trying to SYS a CVF?
	pop	ax
	je	$$IF43			;   no, return with CY clear

	mov	ax,(util_B shl 8) + cant_sys_target  ; indicate error
	stc
;; ENDIF

;  $endif				;				       ;AN000;
$$IF43:

   ret					;				       ;AN000;

   ENDPROC Validate_Target_Drive

   BREAK <SYS - Check_Default_Drive >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Check_Default_Drive
;*******************************************************************************
;
;Description: Check to see if drive specified is default drive. If it is,
;	      load return code and set fail flag.  Initialize source names
;             for IO.SYS, MSDOS.SYS, and DBLSPACE.BIN.
;
;Called Procedures: None
;
;Input: None
;
;Output: no error - CF = 0
;	    error - CF = 1	AX = 16d - Can not specify default drive
;
;Change History: Created	5/01/87 	FG
;Change History: Ax021		2/22/88 	FG
;                               11/17/92        MD Added Dblspace support
;
;******************* END OF SPECIFICATIONS *************************************

public Check_Default_Drive

   Check_Default_Drive PROC NEAR
   push ds
   pop	es

   ASSUME DS:DATA,ES:DATA

   cmp	Spec_Flag,1			; was a source specified ?	       ;AN021;
                			; if a source was specified	       ;AN021;
   JNE $$IF45
;
; we have source specifier, check that it isn't same as target
;
   mov  si,offset SourceSpec	        ; point to Source Spec		   ;C04
   mov  di,offset DIR_SECTOR	        ; point at output buffer	   ;C05
   mov  ax,(xNameTrans SHL 8)	        ; check for name translation	   ;C05
   int  21h 			        ; get real path 		   ;C05
   mov  si,offset DIR_SECTOR	        ; pointer to source path	   ;C05
   mov  di,offset TargSpec		; pointer to target path	   ;C04
   mov  cx,4			        ; 3 chars in target path + zero    ;C04
   repz cmpsb			        ; compare source and target	   ;C04
               				; If equal - we have a problem	   ;C04
   JNZ $$IF46
;
; Source == target, report error
;
   mov	ax,(util_B shl 8) + src_targ_same  ; load return code	   ;C04
   stc				        ; indicate error		   ;C04
   JMP $$IF54
;
; Source != target, so get base path into source names.  Note that SourceSpec
; is part of the SourceBIOSName field, so we don't need to initialize
; SourceBIOSName with SourceSpec
;
$$IF46:
   lea	si,SourceSpec		        ; copy source for IBMDOS.COM	       ;AN021;
   mov	al,[si] 		        ; get the drive ID		       ;AN025;
   sub	al,num_2_letter 	        ; convert it to a 1 base number        ;AN025;
   mov	DEFALT,al		        ; save it in case its removable        ;AN025;
   lea	di,SourceDOSName	        ;				       ;AN021;
   mov	cx,SourceSize		        ; set up size to move		       ;AN021;
   rep	movsb			        ; move it!

IFDEF DBLSPACE_HOOKS
   lea  si,SourceSpec                   ; copy source for dblspace.bin
   lea  di,SourceDblSpaceName
   mov  cx,SourceSize
   rep  movsb
ENDIF

   JMP SHORT $$EN45                     ; now go compose source BIOS name
;
; No source specified on command line, so figure out default source location
; If default source is a double-spaced drive, locate the host drive, and
; use it instead.
;
$$IF45:

   mov  ax,(Get_Default_Drive shl 8) + not_used ; get_default_drive
   INT  21h 			        ;   Get_Default_Drive  <1900>
   call Get_Host_Drive                  ; translate to Dblspace Host drive
   inc  al				; turn from phys drive to logical drive
   mov  DEFALT,al			; save default for later
   mov  SourceSpec,al
   or   SourceSpec,num_2_letter	        ; covert number to letter
   mov  si,offset SourceSpec	        ; point to source spec		   ;C05
   mov  di,offset DIR_SECTOR	        ; point at output buffer	   ;C05
   mov  ax,(xNameTrans SHL 8)	        ; check for name translation	   ;C05
   int  21h 			        ; get real path 		   ;C05
   mov  al,byte ptr DIR_SECTOR	        ; get real drive		   ;C05
   sub  al,num_2_letter		        ; convert it to a 1 base number    ;C05
   cmp  al,TargDrvNum		        ; is target drive = default drive
        				; if it is the same - we have a problem;AC000;
   JNE $$IF50

   mov	ax,(util_B shl 8) + not_on_default ; load return code
   			                ;      - Can not specify default drive
   stc				        ; set fail flag
   JMP SHORT $$IF54                     ; go report error
;
; We've now determined the correct default source path.  Set up the source names
;
$$IF50:
					; initalize SourceBIOSNane, SourceDOSName
   mov	al,DEFALT
   or	al,num_2_letter 	        ; turn into letter
   mov	byte ptr SourceBIOSName,AL      ; twiddle source name
   mov	SourceDOSName,AL	        ; twiddle source name
IFDEF DBLSPACE_HOOKS
   mov  SourceDblSpaceName,AL
ENDIF

$$EN45:
   cld				
   IF   IBMCOPYRIGHT
   mov  bx,NameLen		
   mov  cx,bx			
   ELSE
   mov  cx,BIOSNameLen
   ENDIF

   lea  di,SourceBiosName		; move IBMBIO.COM into place	       ;AN021;
   add  di,SourceSize		        ; move to end of specified part        ;AN021;
   lea  si,SourceBIOS		        ; point to system file name	       ;AN021;
   rep  movsb			

   IF   IBMCOPYRIGHT
   mov  cx,bx			
   ELSE
   mov  cx,DosNameLen
   ENDIF

   lea  di,SourceDOSName		; move IBMDOS.COM into place	       ;AN021;
   add  di,SourceSize		        ; move to end of specified part        ;AN021;
   lea  si,SourceDOS		        ; point to system file name	       ;AN021;
   rep  movsb			        ;				       ;AN021;

IFDEF DBLSPACE_HOOKS
   mov  cx,DblSpaceNameLen
   lea  di,SourceDblSpaceName           ; move DBLSPACE.BIN into place
   add  di,SourceSize
   lea  si,SourceDblSpace
   rep  movsb
ENDIF

$$IF54:
   ret

   ENDPROC Check_Default_Drive


   BREAK <SYS - Get_Host_Drive >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Get_Host_Drive
;*******************************************************************************
;
;Description: Given a drive number in AL, check to see if it is a dblspace
;             drive, and if so, translate the drive number to the host
;             drive number.
;
;Called Procedures: None
;
;Input: 0-based drive number in AL
;
;Output: drive number in AL
;
;Change History: Created       11/21/92         MD
;
;******************* END OF SPECIFICATIONS *************************************

public Get_Host_Drive

   Get_Host_Drive PROC NEAR

        push    ax
   	mov	ax,4a11h	; DBLSPACE multiplex number
	xor	bx,bx		; inquire version number
	int	2fh
	or	ax,ax		; error?
	jnz	not_dblspace
	cmp	bx,'DM'		; stamp returned correctly?
	jnz	not_dblspace

;	DBLSPACE.BIN is loaded.  At this time:
;
;	(dx & 0x7fff) == driver internal version number
;	high bit of DH set of driver has not yet been permanently placed
;	cl == first disk letter reserved for DBLSPACE
;	ch == number of disk letters reserved for DBLSPACE

	mov	ax,4a11h	; DBLSPACE multiplex number
	mov	bx,1		; inquire drive map
        pop     dx
        push    dx
	int	2fh
	test	bl,80h		; COMPRESSED bit true?
	jz	not_dblspace

;	Drive is compressed.  At this time:
;
;	(bl & 0x7f) == host drive's CURRENT drive number
;	bh          == CVF extension number
;
        mov     al,bl
        and     al,7Fh
        cbw
        pop     dx
        push    ax

not_dblspace:
        pop     ax
        ret

    ENDPROC Get_Host_Drive

   BREAK <SYS - Check_Target_Drive >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Check_Target_Drive
;*******************************************************************************
;
;Description: Determine if target drive is valid. To do this, we will make an
;	      IOCTL - check media ID call.
;
;Called Procedures:
;
;Input:  Default_Drive
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = 16d - Can not specify default drive
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Check_Target_Drive
;
;	Check media ID (INT21 IOCTL + IOCTL_CHANGEABLE? <4408>)
;	if no error
;		if invalid drive
;			set return code
;			set fail flag
;		else
;			clear fail flag
;	else
;		reset fail flag
;	endif
;	if no error
;		if ASSIGNed or SUBSTd drive
;			set return code
;			set fail flag
;		else
;			clear fail flag
;	else
;		reset fail flag
;	endif
;	ret
;
;	END  Check_Target_Drive
;
;******************-  END  OF PSEUDOCODE -**************************************

public Check_Target_Drive

   Check_Target_Drive PROC NEAR

   mov	bl,TargDrvNum			; get the target drive number	       ;AN000;
   mov	ax,(IOCTL SHL 8) + IOCTL_CHANGEABLE? ; do a media check 	       ;AC000;
   INT	21h				;	  IOCtl + 08 <4408>	       ;AC000;

   cmp	ax,0fh				; is it invalid - al = F (CF may be set;AC000;

;  $if	e				;				       ;AC000;
   JNE $$IF56

       mov  ax,(DOS_error shl 8) + extended_15 ; load return code	       ;AC000;
					;		- invalid drive
       stc				;				       ;AC000;

;  $else				; if valid device so far - make sure   ;AN012;
   JMP SHORT $$EN56
$$IF56:
					; its not ASSIGNed or SUBSTed drive
       mov  si,offset TargSpec		; point to Target Spec		       ;AN012;
       mov  di,offset DIR_SECTOR	; point at output buffer	       ;AN012;
       mov  ax,(xNameTrans SHL 8)	; check for name translation	       ;AN012;
       int  21h 			; get real path 		       ;AN012;
;      $if  nc				;				       ;AC012;
       JC $$IF58
	   mov	bl,byte ptr [TargSpec]	; get drive letter from path	       ;AN012;
	   cmp	bl,byte ptr DIR_SECTOR	; did drive letter change?	       ;AN012;
;	   $if	ne			; if not the same, it be bad	       ;AN012;
	   JE $$IF59
	       lea  si,sys_ptr		; set insert pointer in SUBLIST        ;AN012;
	       mov  [insert_ptr_off],si ;				       ;AN012;
	       mov  [insert_ptr_seg],ds ;				       ;AN012;
	       lea  si,sublist		; set pointer to SUBLIST	       ;AN012;
	       mov  ax,(util_C shl 8) + cant_assign ; load ret cd (Cannot..SUB);AN012;
	       stc			; tell user			       ;AN012;
;	   $else			; - its ok			       ;AN012;
	   JMP SHORT $$EN59
$$IF59:
	       clc			; keep going			       ;AN012;
;	   $endif			;				       ;AN012;
$$EN59:
;      $else				; - its a critical error	       ;AN012;
       JMP SHORT $$EN58
$$IF58:
	   xor	ah,ah			; set up for extended error call       ;AN012;
	   inc	ah			;				       ;AN012;
;      $endif				;				       ;AN012;
$$EN58:
;  $endif				;				       ;AN012;
$$EN56:

   ret					;				       ;AC000;

   ENDPROC Check_Target_Drive

   BREAK <SYS - Check_For_Network >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Check_For_Network
;*******************************************************************************
;
;Description: Verify that the target drive is local, and not a shared drive.
;	      If shared,load return code and set fail flag.
;
;	      NOTE: This is a design point on how to determine net
;
;CALLed Procedures: None
;
;Input:  None
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = return code = 7 - Cannot SYS to a Network drive
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Check_For_Network
;
;	IOCtl call to see if target drive is local
;	if target drive not local (INT21 IOCtl + 09 <4409>) and
;	if return code indicates network drive (test 1200h)
;		set insert pointer in SUBLIST
;		set pointer to SUBLIST
;		load return code (Cannot SYS to a Network drive)
;		set fail flag
;	else
;		reset fail flag
;	endif
;	ret
;
;	END  Check_For_Network
;
;******************-  END  OF PSEUDOCODE -**************************************

public Check_For_Network

   Check_For_Network PROC NEAR

					; IOCtl call to see if target drive is local
   mov	bl,TargDrvNum			;   x = IOCTL (getdrive, Drive+1)      ;AC022;
   mov	ax,(IOCTL SHL 8) + dev_local
   INT	21h				; IOCtl + dev_local  <4409>

;  $if	nc,and				; target drive local and	       ;AC000;
   JC $$IF65

   test dx,1200H			; check if (x & 0x1000)
					;      (redirected or shared)
;  $if	nz				; return code indicates network drive  ;AC000;
   JZ $$IF65

       lea  si,sys_ptr			; set insert pointer in SUBLIST        ;AN000;
       mov  [insert_ptr_off],si 	;				       ;AN000;
       mov  [insert_ptr_seg],ds 	;				       ;AN000;
       lea  si,sublist			; set pointer to SUBLIST	       ;AN000;
       mov  ax,(util_C shl 8) + cant_network ; load return code (Cannot SYS to.;AC000;
       stc				; set fail flag 		       ;AN000;

;  $else				;				       ;AC000;
   JMP SHORT $$EN65
$$IF65:

       clc				; reset fail flag		       ;AC000;

;  $endif				;				       ;AC000;
$$EN65:

   ret					;				       ;AN000;

   ENDPROC Check_For_Network

   BREAK <SYS - Get_System_Files >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Get_System_Files
;*******************************************************************************
;
;Description:	    Ensure that the the files IBMBIO, IBMDOS and DBLSPACE.BIN
;                   are available
;		    on the source media. If they are not on the source media,
;		    and the media is removeable, a prompt will be issued to
;		    insert a new source.
;
;Called Procedures: Prompt_For_Media	       Open_File
;		    Check_Removable	       Fill_Memory
;
;Input: 	    IBMBIO, IBMDOS, and DBLSPACE.BIN on source media
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = return code (message #)
;
;Change History:    Created	   5/01/87	   FG
;		    Major change   1/07/88	   FG	Ax019 now makes SYS check
;							for the CORRECT version
;							of IBMBIO !
;							IBMBIO looks like this:
;				       1	2	 3	  4	  5
;				  ÚÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÂÄ
;				  ³  JMP   ³   LO   ³	HI   ³extected_version³
;				  ÀÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÁÄ
;
;                                  11/17/92        MD   Added support for
;                                                       DBLSPACE.BIN
;******************* END OF SPECIFICATIONS *************************************

public Get_System_Files

        Get_System_Files PROC NEAR
        cld

$$DO68:
        lea  si,DOS_BUFFER		; set up addressability
;
; Find and open BIOS file
;
        mov  dx,OFFSET SourceBIOSName	; look on source for IBMBIOS
        mov  di,OFFSET BIOSInFH		; pointer to block of data
        call Open_File			; open file
        JC   $$IF69                     ; jump on open error
;
; Now find and open DOS file
;
        mov  dx,OFFSET SourceDOSName	; look on source for IBMDOS
        mov  di,OFFSET DOSInFH		; pointer to block of data
        call Open_File			; open file
        JC   $$IF69                     ; jump on open error


IFDEF DBLSPACE_HOOKS
;
; Now find and open Dblspace.bin file
;
gsf_open_Dblspace:
        mov  dx,OFFSET SourceDblSpaceName ; look on source for Dblspace
        mov  di,OFFSET DblSpaceInFH	; pointer to block of data
        call Open_File			; open file
        JNC  gsf_got_dblspace           ; carry on if we located it
;
; We didn't locate the DBLSPACE.BIN file on the source drive.  Look for
; it along the path.
;
        call Find_DblSpace_on_Path
        jnc  gsf_open_dblspace

; Unable to locate Dblspace.bin file.  This isn't a fatal error, but
; we need to avoid further attempts to process the Dblspace file.  Set the
; flag that tells us to skip the create, and clear out the entry for
; it in the FHArray, to signal termination in Fill_Memory and Dump_Memory
;
        mov  NoDblSpace,TRUE            ; don't create it
        mov  FHArrayDS,0                ; don't copy it

gsf_got_dblspace:
ENDIF


        mov  ah,Read			; read the first sector of BIO	   ;C06
        mov  bx,BIOSInFH 		; get bios source handle	   ;C06
        mov  cx,512			; only one sector		   ;C06
IFDEF NEED_CLUSTER_2
        mov  dx,OFFSET BUF+FAT_BUF	; point to beginning of buffer	   ;C06
ELSE
        mov  dx,OFFSET BUF
ENDIF
        int  21h 			; read + not_used <3F00>	   ;C06
        JC $$IF69                        ; jump on read error
IFDEF NEED_CLUSTER_2
        cmp  WORD PTR BUF+FAT_BUF+3,expected_version ; point to beginning       ;AN019;
ELSE
        cmp  word ptr BUF+3,expected_version
ENDIF
					;  of buffer + near jump instruction
        JNE $$IF69                       ; jump if wrong version
;
; Seek back to beginning of file, so Fill_Memory doesn't get confused
; later
;
        mov  ax,LSEEK Shl 8 + 0          ; seek from beginning
        mov  cx,0                        ; seek to offset 0
        mov  dx,cx
        int  21h

        clc				; reset fail flag		       ;AN019:
        mov	al,noerror		; load success return code	       ;AN000;
        JMP SHORT $$EN69

;
; Some error occurred accessing source files
;
$$IF69:
        mov  bl,defalt		        ;; specify drive ;;dcl		       ;AN001;
        call Check_Removeable	        ; check if source media replaceable    ;AC000;
        JC   $$EN68                      ; error if not

        mov  ax,(util_C shl 8) + sys_disk ; load message number	
       		                	;    - Insert system disk....
        lea  si,SourceSpec	        ; set insert pointer to DRIVE ID
        mov  bx,SourceSize	        ; only display correct path length
        call Prompt_For_Media	        ; prompt for source media	
        JC   $$EN68                      ;

        mov  ax,error_RC	                ; load return code (try again)	

$$EN69:
        				; if fail flag set		       ;AC018;
        JC $$EN68
        cmp  al,noerror			; is it an error return code?	       ;AC018;
        				; quit if success return code	       ;AC018;
        JE   $$en68
;
; Before we go try again, close all the files we just opened
;
        mov  bx,BIOSInFH
        mov  ah,CLOSE
        int  21h

        mov  bx,DOSInFH
        mov  ah,CLOSE
        int  21h

IFDEF DBLSPACE_HOOKS
        cmp  NoDblSpace,TRUE
        je   gsf_retry
        mov  bx,DblSpaceInFH
        mov  ah,CLOSE
        int  21h

gsf_retry:
;
;      reset DblSpace state variables, in case we find it on the second try
;
        mov  NoDblSpace,FALSE
        mov  FHArrayDS,offset DblSpaceInFH
ENDIF

        jmp  $$DO68


$$EN68:
        ret

   ENDPROC Get_System_Files


IFDEF DBLSPACE_HOOKS
   BREAK <SYS - Find_DblSpace_on_Path >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Find_DblSpace_on_Path
;*******************************************************************************
;
;Description:	    Search Path for Dblspace.bin
;
;Output: no error - CF = 0        SourceDblSpaceName filled in with
;                                 full path to dblspace.bin
;	    error - CF = 1	  Dblspace.bin not found
;
;******************* END OF SPECIFICATIONS *************************************

Find_DblSpace_on_Path PROC NEAR
        public Find_DblSpace_on_Path

        push es
        push ds                         ; save our segments
        push si                         ; save DTA address

        mov  es,[old_psp]               ; get our PSP
        call Find_Path_In_Environment   ; returns ptr to path string in ES:DI
        jc   fdp_exit                   ; no path, can't find dblspace.bin

        assume es:nothing
        mov  ax,ds                      ; swap DS and ES
        push es
        pop  ds
        assume ds:nothing
        mov  si,di                      ; DS:SI ==> Path string
        mov  es,ax
        assume es:data

fdp_path_loop:
        mov  bh,';'                     ; path separator character
        mov  dx,offset SourceDblSpace   ; base file name
        mov  di,offset SourceDblSpaceName ; buffer to stick full path in
        call Path_Crunch                ; concatenate name and path
        pushf                           ; save result
        push ds                         ; save segment of Path
        push es
        pop  ds
        assume ds:data
        mov  dx,offset SourceDblSpaceName ; buffer with full path name
        mov  bx,offset DOS_BUFFER       ; DMA buffer for finds
        mov  al,1                       ; extension is specified
        call Search
        or   al,al                      ; found the file?
        pop  ds                         ; recover path segment
        assume ds:nothing
        pop  ax                         ; recover flags in AX
        jnz  fdp_exit                   ; found it!
        xchg ah,al
        sahf                            ; check Path_Crunch result
        jnc  fdp_path_loop

fdp_exit:
        pop  si
        pop  ds
        pop  es
        assume ds:data
        ret

EndProc Find_DblSpace_on_Path
ENDIF


   BREAK <SYS - Prompt_For_Media >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Prompt_For_Media
;*******************************************************************************
;
;Description:	    Make call to Message to display:
;
;			Insert system disk in drive %1
;			and strike any key when ready
;
;Called Procedures: Message
;
;Input: 	    (AL) = message #
;		    (BL) = drive/path length
;		    (SI) = insert pointer
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = return code (DOS error)
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Prompt_For_Media
;
;	set up for message call
;	call Message  - display first line
;	if no error
;		clear insert indicator
;		load Message #x - Press any key to continue
;		ask for keystroke response (direct CON in no echo)
;		call Message  - display second line
;	endif
;	if error
;		load return code (DOS extended error)
;	endif
;	ret
;
;	END  Prompt_For_Media
;
;******************-  END  OF PSEUDOCODE -**************************************

public Prompt_For_Media

   Prompt_For_Media PROC NEAR

   mov	[insert_ptr_off],si		; set up for message call	       ;AN000;
   mov	[insert_ptr_seg],ds
   mov	insert_max,bl			; only display correct path length     ;AN025;
   lea	si,sublist			; set pointer to SUBLIST	       ;AN000;

   call Message 			; display first line		       ;AN000;

;  $if	nc				; if no error			       ;AN000;
   JC $$IF81

       mov  ax,(util_D shl 8) + press_key ; load Message		       ;AN000;
					;    - Press any key to continue
					; the class will signal to ask for
					; keystroke response
					;	 (direct CON in no echo)
       call Message			; display second line		       ;AN000;

;  $endif				;				       ;AN000;
$$IF81:

;  $if	c				; if an error occured		       ;AN000;
   JNC $$IF83

       mov  ah,DOS_error		; load return code (DOS extended error);AN000;

;  $endif				;				       ;AN000;
$$IF83:

   ret					;				       ;AN000;

   ENDPROC Prompt_For_Media

   BREAK <SYS - Check_Removeable >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Check_Removeable
;*******************************************************************************
;
;Description:	    Make IOCtl call to see if media in the drive indicated in
;		    BX is removable
;
;Called Procedures: None
;
;Input: 	    BX has drive (0=default, 1=A)
;
;Output: removeable	       - CF = 0
;	 nonremovable or error - CF = 1
;				 AX = 11d - No system on default drive
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Check_Removeable
;
;	if source not specified
;	       do an IOCTL changeable check (INT21 IOCtl + 08 <4408>)
;	       if no error
;		       test if removeable
;		       if removeable
;			       reset fail flag
;		       else
;			       load return code (No system on default drive)
;			       set fail flag
;		       endif
;	       endif
;	else
;	       load return code (No system on specified path)
;	       set fail flag
;	endif
;	ret
;
;	END  Check_Removeable
;
;******************-  END  OF PSEUDOCODE -**************************************

public Check_Removeable

   Check_Removeable PROC NEAR

   mov	ax,(IOCTL SHL 8) + IOCTL_CHANGEABLE? ; do a media check
   INT	21h				;	  IOCtl + 08 <4408>
					; cy set if remote or invalid device ;;dcl;;
;  $if	nc				;
   JC $$IF85
       cmp  ax,0			;
;      $if  e				;
       JNE $$IF86
	   clc				;
;      $else				;
       JMP SHORT $$EN86
$$IF86:
	   cmp	Spec_Flag,1		;				       ;AC025;
;	   $if	ne			;				       ;AC025;
	   JE $$IF88
	       mov  ax,(util_B shl 8) + no_sys_on_def ; No system on...        ;AC000;
;	   $else			;				       ;AC025;
	   JMP SHORT $$EN88
$$IF88:
	       mov  ax,(util_B shl 8) + system_not_found ; Invalid path or Sy..;AN021;
;	   $endif			;				       ;AC025;
$$EN88:
	   stc				;
;      $endif				;
$$EN86:
;  $endif				;
$$IF85:

   ret					;				       ;AN021;

   ENDPROC Check_Removeable

   BREAK <SYS - Open_File >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Open_File
;*****************************************************************************
;
;Description:	    Finds and opens file and gets date and size
;
;Called Procedures: None
;
;Input: 	    SI ==> DTA used for find operation
;                   DX ==> file name to search for
;                   DI = xfer_data struc to record info in
;
;Output: no error - CF = 0
;	    error - CF = 1	      AX = DOS extended errors
;
;Change History:    Created	   5/01/87	   FG
;                                  11/17/92        MD  Rewrote
;
;******************* END OF SPECIFICATIONS *************************************

public open_file

   Open_File PROC NEAR

   mov  CX,DOS_system_atrib 	        ; its an 'everything' file
   mov  ah,Find_First		        ; do a find first INT21
   INT  21h 			        ;      Find_First <4Exx>
   JC   of_err_exit                     ; jump if not found
;
; Found file, store size data
;
   mov  ax,[si].find_buf_size_l	        ; move size (low and high)	
   mov  [di].LenLow,AX                  ;     from DTA
   mov  ax,[si].find_buf_size_h	        ;	to			
   mov  [di].LenHigh,AX                 ;	  SYS data space
;
; Now open file to get time and date
;
   mov	ax,(OPEN SHL 8) + not_used	; open file for read
   INT	21h				;    Read + not_used <3D00>
   JC   of_err_exit

   mov  [di].InFH,ax		        ; save file handle		
   mov  bx,ax			        ; move handle for next call
   mov  ax,(File_Times SHL 8) + 0	; find last write time
   INT  21h 			        ;	 File_Times + not_used <5700>
   mov  [di].FTime,cx		        ; save time			
   mov  [di].FDate,dx		        ; save date			
   JMP  SHORT of_end

of_err_exit:
    mov  ah,DOS_error		        ; load return code (DOS extended error)

of_end:
   ret

   ENDPROC Open_File

   BREAK <SYS - Fill_Memory >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Fill_Memory
;*******************************************************************************
;
;Description:  Read in as much of IBMBIOS, IBMDOS and DBLSPACE.BIN
;              as room permits
;
;Called Procedures: None
;
;Input: 	    None
;
;Output: no error - CF = 0
;	    error - CF = 1
;
;Change History:    Created	   5/01/87	   FG
;                   rewritten     11/18/92         MD
;
;******************* END OF SPECIFICATIONS *************************************

public fill_memory

   Fill_Memory PROC NEAR

   mov  si,offset FHArray               ; get pointers at xfer_data strucs
   mov  cx,cbBuf                        ; initial buffer size
IFDEF NEED_CLUSTER_2
   mov  dx,OFFSET BUF+FAT_BUF           ; buffer starting address
ELSE
   mov  dx,OFFSET BUF
ENDIF
   xor  ax,ax                           ; size of last read = 0 to start

; Entry:
;   si = FHArray entry (which file we are reading)
;   dx = position in buffer to read into
;   cx = space remaining in buffer
;   ax = size of last read operation (0 initially)
;
; NOTE: The BUF buffer is always less than 64K, because we are a COM
;       file.  This means that we will only make one read call per file
;       per call to Fill_Memory.
;
fm_loop:
   mov  di,[si]                         ; get next xfer_data struc
   mov  [di].BufPos,ax                  ; store our starting location in buf
   add  dx,ax                           ; set buffer address
   sub  cx,ax                           ; adjust buffer size

   mov  bx,[di].InFH                    ; get file handle
   mov  bp,cx                           ; save starting buffer size
   cmp  [di].LenHigh,0                  ; more than 64Kb to read?
   jne  fm_do_read                      ; yes, just proceed

   cmp  [di].LenLow,cx                  ; more than buffer size to read?
   jae  fm_do_read                      ; yes, just proceed

   mov  cx,[di].LenLow                  ; adjust size of read
   or   cx,cx                           ; nothing to read?
   jnz  fm_do_read                      ; no, carry on with read

; nothing left to read from this file, so close it so we can reuse the SFT

   push ax
   mov  ah,CLOSE
   int  21h
   mov  [di].InFH,-1                    ; forget this file handle
   pop  ax
   jmp  short fm_adjust_size

fm_do_read:
   mov  ah,Read
   int  21h
   jc   fm_end                          ; quit on read error

   cmp  ax,cx                           ; check that we read it all
   je   fm_adjust_size                  ; yes, go adjust size left to read

   stc
   jmp  short fm_end                    ; else go report error

fm_adjust_size:
   sub  [di].LenLow,ax                  ; subtract size read
   sbb  [di].LenHigh,0
   mov  [di].BufferedSize,ax            ; store size of data read in

   cmp  ax,bp                           ; any space left in buffer?
   jae  fm_end                          ; no, done with this pass

;* We read all of the current file, and still have space in the buffer,
;  so read the next file.

   mov  cx,bp                           ; recover original buffer size
   add  si,TYPE FHArray                 ; advance to next xfer_data struc
   cmp  word ptr [si],0
   jne  fm_loop                         ; continue with next file

fm_end:
   ret

   ENDPROC Fill_Memory

   BREAK <SYS - Dump_Memory >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Dump_Memory
;*******************************************************************************
;
;Description:	    Write out as much of IBMBIOS, IBMDOS, and DBLSPACE.BIN
;                       as is in memory.
;
;Called Procedures: None
;
;Input: 	    None
;
;Output: no error - CF = 0
;	    error - CF = 1
;
;Change History:    Created	   5/01/87	   FG
;                   Rewritten     11/18/92         MD
;
;******************* END OF SPECIFICATIONS *************************************

   public Dump_Memory

   Dump_Memory PROC NEAR

   xor  ax,ax
   mov  si,offset FHArray               ; point at xfer_data strucs
IFDEF NEED_CLUSTER_2
   mov  dx,offset BUF+FAT_BUF           ; start of buffer
ELSE
   mov  dx,offset BUF
ENDIF
   mov  bp,cbBuf                        ; original buffer size

; Entry:
;   si = FHArray entry (which file we are writing)
;   dx = position in buffer to read from
;   ax = size of last write operation (0 initially)
;   bp = count of bytes in buffer to be written
;
dm_loop:
   mov  di,[si]                         ; get next xfer_data struc
   mov  cx,[di].BufferedSize            ; bytes of this file in buffer
   jcxz dm_get_next                     ; anything there to write?

   add  dx,[di].BufPos                  ; start of this file in buffer
   mov  bx,[di].OutFH                   ; target file handle

; file may not yet be open, file handle will be -1 if so.  If that's
; the case, open the target file here.  We'll be opening the file for
; its initial write, so we don't need to seek into the file.

   cmp  bx,-1
   jne  dm_write                        ; file already open, continue

   push ax
   push dx
   mov  ax,OPEN shl 8 + 1               ; open for write access
   mov  dx,[di].OutFileName
   int  21h

   mov  [di].OutFH,ax                   ; save the file handle
   mov  bx,ax                           ; ready for write
   pop  dx
   pop  ax
   jc   dm_err_exit                     ; error on open, shouldn't happen

dm_write:
   mov  ah,WRITE
   int  21h                             ; write the file
   jc   dm_err_exit                     ; quit on error
   cmp  ax,cx                           ; check that we wrote it all
   jne  dm_err_exit                     ; quit if not

dm_get_next:
   sub  bp,ax                           ; account for part of buffer now used
   jbe  dm_exit                         ; quit if buffer all used
   add  si,TYPE FHArray                 ; point at next xfer_data struc
   cmp  word ptr [si],0                 ; reached last file?
   jne  short dm_loop                   ; go try again

dm_exit:
   ret

dm_err_exit:
;
;       On error, close and delete the file we failed on, so we don't
;       leave partial files lying around.
;
   mov  ah,CLOSE
   int  21h                             ; close the output file
   mov  dx,[di].OutFileName             ; get the target file name
   mov  ah,UNLINK
   int  21h                             ; delete it
   stc                                  ; signal error
   ret


   ENDPROC Dump_Memory

   BREAK <SYS - Check_SYS_Conditions >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Check_SYS_Conditions
;*******************************************************************************
;
;Description: Verify that the target disk is in a state that a SYS to it will
;	      be allowed. If an error occurs in any of the called routines,
;	      the return code will already be loaded by the failing routine.
;
;Called Procedures: Verify_File_System
;		    Read_Directory
;		    Verify_File_Location
;		    Determine_Free_Space
;
;Input: None
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = return code (message #)
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Check_SYS_Conditions
;
;	verify target is a FAT file system (CALL Verify_File_System)
;	if no error and
;		load root directory of target (CALL Read_Directory)
;	if no error and
;		check that IBMBIO,IBMDOS are in right place (CALL Verify_File_Location)
;	if no error and
;		check for  sufficient space for system files (CALL Determine_Free_Space)
;	if no error
;		load return code (success)
;		reset fail flag
;	endif
;	ret
;
;	END  Check_SYS_Conditions
;
;******************-  END  OF PSEUDOCODE -**************************************

public Check_SYS_Conditions

   Check_SYS_Conditions PROC NEAR

   call Verify_File_System		; verify target is a FAT file system   ;AN000;

;  $if	nc,and				; no error and			       ;AN000;
   JC $$IF108

   call Read_Directory			; load root directory of target        ;AN000;

;  $if	nc,and				; no error and			       ;AN000;
   JC $$IF108

   call Verify_File_Location		; check that IBMBIO,IBMDOS are in right;AN000;
					;    place
;  $if	nc				; no error and			       ;AN000;
   JC $$IF108

       call Determine_Free_Space	; check if enough space for system file;AN000;

;  $endif				;				       ;AN000;
$$IF108:

   ret					;				       ;AN000;

   ENDPROC Check_SYS_Conditions

   BREAK <SYS - Verify_File_System >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Verify_File_System
;*******************************************************************************
;
;Description: Get the file system for the specified drive, then compare to
;	      FAT. If not, issue message and exit. Must ensure that target
;	      drive has media in it before this routine is called
;
;Note:	      This routine contains code that is specifically required for
;	      operation on DOS 3.3.  This code must be removed for later releases
;	      of DOS.
;
;Called Procedures: None
;
;Input: 	    Drive Number (0=default, 1=A, 2=B) in BL
;
;Output: no error - CF = 0
;	    error - CF = 1
;		    AX = return code
;			  AH = utility messages
;			  AL = 15d - Not able to SYS to xxx file system
;		    CX = 1 - only one substitution
;		 DS:SI = sublist for substitution
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Verify_File_System
;
;	if   dos = current
;		load drive id (BL)
;		get_extended_device_parameters (INT21 IOCtl + 0Dh <440D> CX=086E) for drive
;		if error - check if old version destination
;			find out what the error was (CALL Get_DOS_Error)
;			if not old version error
;				load return code (DOS Extended Error Class)
;				set fail flag
;			else
;				reset fail flag
;			endif
;		else
;			if returned file system type = "FAT12   " or
;			if returned file system type = "FAT16   "
;				reset fail flag
;			else
;				indicate insert required
;				set up pointer for insert - sublist
;				load return code (Unable to SYS to xxxxxxxx file system)
;				set fail flag
;			endif
;		endif
;	endif
;
;	ret
;
;	END  Verify_File_System
;
;******************-  END  OF PSEUDOCODE -**************************************

public Verify_File_System

   Verify_File_System PROC NEAR

   cmp	DOS_VER,0			; running on current DOS ?	       ;AN019;
;  $if	e,long				; if we are		      ;AC999;FAT;APAR;;AN019;
   JE $$XL1
   JMP $$IF110
$$XL1:
       mov  bl,TargDrvNum		; load drive id (BL)		       ;AN000;
       lea  dx,IOCtl_Buf		; point to output buffer	       ;AN000;
       mov  ax,(GetSetMediaID shl 8) + 0 ; get volid, ser# and filetype        ;AC019;
       INT  21h 			;  INT 21 GetSetMediaID request <6900> ;AC019;

;      $if  c				; error - check if old version dest    ;AN000;
       JNC $$IF111

	   call Get_DOS_Error		; find out what the error was	       ;AN000;

	   cmp	al,old_type_media	; is it IBM but < 4.0 ? 	       ;AN000;

;	   $if	ne			; not old version error 	       ;AN000;
	   JE $$IF112

	       mov  ah,DOS_error	; load return code (DOS Extended Error);AN000;
	       stc			; set fail flag 		       ;AN000;

;	   $else			;				       ;AN000;
	   JMP SHORT $$EN112
$$IF112:

	       clc			; reset fail flag		       ;AN000;

;	   $endif			;				       ;AN000;
$$EN112:

;      $else				; ELSE it is => 4.00		       ;AN000;
       JMP SHORT $$EN111
$$IF111:

	   lea	si,IOCtl_File_Sys	; see if file type is fat12	       ;AN000;
	   lea	di,fat_12		;				       ;AN000;
	   mov	cx,file_sys_size	;				       ;AN000;
	   cld				;				       ;AN000;
	   repe cmpsb			;				       ;AN000;
;--------------------			; IF failed at 4th character, then it
	   cmp	cx,4			;  may have been "FAT     "
;	   $IF	e,and			;			
	   JNE $$IF116
	     cmp  BYTE PTR ds:[si-1]," ";  Is 4th char a blank?
;	   $IF	e,and			;			
	   JNE $$IF116
	     cmp  BYTE PTR ds:[si  ]," ";  Is 5th char a blank?
;	   $IF	e,and			;			
	   JNE $$IF116
	     cmp  BYTE PTR ds:[si+1]," ";  Is 6th char a blank?
;	   $IF	e,and			;			
	   JNE $$IF116
	     cmp  BYTE PTR ds:[si+2]," ";  Is 7th char a blank?
;	   $IF	e,and			;			
	   JNE $$IF116
	     cmp  BYTE PTR ds:[si+3]," ";  Is 8th char a blank?
;	   $IF	e			;			
	   JNE $$IF116
	     mov  cx,0						
;	   $ELSE						
	   JMP SHORT $$EN116
$$IF116:
;--------------------		MAYBE IT IS FAT12 OR FAT16?
	   cmp	cx,3			; did it fail at the 2 in fat12 ?      ;AN000;

;	   $if	e,and			; if it did and............	       ;AN000;
	   JNE $$IF118

	   cmp	BYTE PTR ds:[si-1],"6"	; was it a 6 ?		  :	       ;AN000;

;	   $if	e			; if it was...............:	       ;AN000;
	   JNE $$IF118

	       repe cmpsb		; then keep going		       ;AN000;

;	   $endif			;				       ;AN000;
$$IF118:
;	   $ENDIF			    ;DONE SCANNING	      ;AN999;FAT;APAR;
$$EN116:

	   cmp	cx,0			; did we reach the end ?	       ;AN000;

;	   $if	e			; if we did it was "FAT12   " or       ;AN000;
	   JNE $$IF121
					;     "FAT16   " so its OK to SYS

	       clc			; reset fail flag		       ;AN000;

;	   $else			;				       ;AN000;
	   JMP SHORT $$EN121
$$IF121:

	       lea  di,File_Sys_End	; set up pointer to end of insert      ;AN000;
	       dec  di			; back up to last character	       ;AN000;
	       mov  cx,file_sys_size	; strip off trailing blanks	       ;AN017;
	       mov  al," "		; strip off trailing blanks	       ;AN017;
	       std			; scan backwards		       ;AN017;
	       repe scas IOCTL_File_Sys ;				       ;AN017;
	       cld			; stops at 2 past last " "	       ;AN017;
	       inc  di			; 1 past			       ;AN017;
	       inc  di			; last (first) blank		       ;AN017;
	       xor  al,al		; make it an ASCIIZ		       ;AN017;
	       stos IOCTL_File_Sys	;				       ;AN017;
	       lea  si,IOCTL_File_Sys	; set up pointer to the insert	       ;AN000;
	       mov  [insert_ptr_off],si ;				       ;AN017;
	       mov  [insert_ptr_seg],ds ;				       ;AN017;
	       lea  si,sublist		; set pointer to SUBLIST	       ;AN017;
	       mov  insert_max,8	; max characters for file system   ;C02
	       mov  ax,(util_C shl 8) + cant_sys ; load return code	       ;AN000;
					;  - Unable to SYS to xxx file system
	       stc			; set fail flag 		       ;AN000;

;	   $endif			;				       ;AN000;
$$EN121:

;      $endif				;				       ;AN000;
$$EN111:

;  $else				; not running on current DOS	       ;AN019;
   JMP SHORT $$EN110
$$IF110:

       clc				; keep going			       ;AN019;

;  $endif				; running on current DOS	       ;AN019;
$$EN110:

   ret					;				       ;AN000;

   ENDPROC Verify_File_System

   BREAK <SYS - Read_Directory >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Read_Directory
;*******************************************************************************
;
;Description: Read in first sector of directory. The reason that we do the
;	      direct read of the directory is the find first/next or search
;	      first/next do an exclusive search for volume labels. By using
;	      these CALLs, there is no way to determine if a volume label
;	      occupies the first location in the directory. Hence we get sleazy
;	      and read the directory directly (no pun intended) to get this
;	      info. Only read in the first sector of directory. Also, this
;	      ensures there is a media in the drive.
;
;CALLed Procedures: Prompt_for_Media, Find_DPB
;
;Input:  None
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = return code (message #)
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Read_Directory
;
;	set up drive letter in destignation filespecs
;	call Find_DPB to get directory location
;	load first DIR sector number
;	point at buffer for directory
;	read first sector of directory (INT 25h)
;	ret
;
;	END  Read_Directory
;
;******************-  END  OF PSEUDOCODE -**************************************

   public Read_Directory

   Read_Directory PROC NEAR

   mov	al,TargDrv			; set up drive letter in destignation filespecs
   mov	BIOSName,al			; point names at destination drive
   mov	DOSName,al
   mov  Targ_Com,al
IFDEF DBLSPACE_HOOKS
   mov  TargDblSpace,al
ENDIF

   MOV	DL,TargDrvNum			; load drive
   PUSH DS				; save register
   call Find_DPB			; initalize DPB parameters
   pop  dx
   mov	ds,dx
   mov	es,dx
   jc   rd_exit                         ; exit on error from Find_DPB
   xor	ax,ax				; request a read
   mov	dx,[first_dir_sector]		; read starting dir sector
   mov	[packet],dx			; get starting dir sector	       ;AN001;
   mov	bx,offset DIR_SECTOR
   mov	PACKET_BUFFER[0],bx		;				       ;AN001;
   mov	PACKET_BUFFER[2],ds		;				       ;AN001;
   mov	word ptr [packet_sectors],1	;				       ;AN001;
   call Direct_Access			; to read the sector

   mov	ax, (util_B shl 8) + write_fail ; load message			       ;AC000;
					;   - Write failure, diskette unuseable
					;  in case an error occured
rd_exit:
   ret

   ENDPROC Read_Directory

   BREAK <SYS - Verify_File_Location >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Verify_File_Location
;*******************************************************************************
;
;Last Update: 09/22/87
;
;Description: Determines if IBMBIO and IBMDOS are the first two directory
;	      entries, or if these entries are empty. If so, find the size
;	      of the files if they exist. If spaces not empty and don't
;	      contain IBMBIO and IBMDOS, set fail flag and load return code.
;	      Also determines if existing IBMBIO starts in cluster 2. If not
;	      set fail flag and load return code.
;             Also records sizes for COMMAND and DBLSPACE, if they are present
;             on the target drive.
;
;CALLed Procedures: None
;
;Input: 	    DIR in BUFFER
;
;Output: no error - CF = 0
;	    error - CF = 1
;		    AX = return code
;			  AH = utility messages
;			  AL = 8 - No room for system on destination disk
;			       9 - Incompatible system size
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************

   public Verify_File_Location

   Verify_File_Location PROC NEAR

;---------------------------------------					
; see if the first two directory					
; entries are available...					
; First check for being free:					
;---------------------------------------					

   mov	bp,OFFSET DIR_SECTOR
   mov	si,bp
   cmp	BYTE PTR [si],empty		; empty dir?
        				; if all files deleted		       ;AC012;
   JE   vfl_first_free
					;   (Dir_Name in dir is 00h)
   cmp	BYTE PTR [si],deleted		; is first file deleted ?
                			; if it is			       ;AC012;
   JNE  vfl_first_not_free
;
; First directory entry is empty or deleted
;
vfl_first_free:
   clc				        ; clear error flag		       ;AN003;

IFDEF NEED_CLUSTER_2
   call Free_Cluster		        ; check the cluster chain just in case ;AC012;
ENDIF

   JMP SHORT vfl_check_second           ; go check the second entry

vfl_first_not_free:

;---------------------------------------					
; The first entry is not free.	See if					
;     the BIOS is there.					
;---------------------------------------					

   mov  di,OFFSET FCBBIO		; pointer to name
   mov  cx,file_spec_length 	        ; length of name
   cld				        ; go forward
   rep  cmpsb			        ; check it
   JNE  vfl_first_not_BIOS              ; jump if not IBMBIO

   dec	BIOSEntFree		        ; indicate we found IBMBIO ( = 0)
   mov	si,bp
   mov	ax,word ptr ds:[si].dir_size_l  ; Get the size of IBMBIO
   mov	word ptr IBMBIO_Low,ax
   mov	ax,word ptr ds:[si].dir_size_h
   mov	word ptr IBMBIO_High,ax

IFDEF NEED_CLUSTER_2
   cmp	ds:[si].dir_first,2	        ; does IBMBIO own Clust 2?	       ;AC005;
   JNE $$IF130
   inc  [bio_owns_it]	                ;     - keep track		       ;AC005;

$$IF130:
   call Free_Cluster		
ENDIF

   JMP SHORT vfl_check_second

;
; Someone is in the first directory entry, and it isn't IBMBIO!
;

vfl_first_not_BIOS:
   call Determine_Free_Space	        ; M004; Need size (IBMBIO+IBMDOS)
   JC   vfl_err			        ; M004; of free clusters

   mov	si,bp			        ; restore pointer to start of entry    ;AN003;
   call Move_DIR_Entry		        ; move the entry out of the way        ;AN003;
   JC   vfl_err

IFDEF NEED_CLUSTER_2
   call Free_Cluster		
ENDIF

   JC   vfl_err

   xor  ax,ax
   JMP SHORT vfl_check_second

;
; Problem relocating owner of first directory entry, report error
;

vfl_err:

   mov  ax,(util_B shl 8) + no_room     ; load return code in case we fail;AN000;
   stc			                ;    - No room for system on dest...   ;AC000;

vfl_check_second:
;---------------------------------------					
; Check the second entry					
;---------------------------------------					

   JNC  vfl_check_second_0              ; exit if we get here with an error
   jmp  vfl_end

vfl_check_second_0:
					; ensure that the first sector of root ;AN003;
					;    is loaded			       ;AN003;
   mov  ax,[first_dir_sector]	        ; get starting dir sector	       ;AN001;
   mov  packet,ax			;				       ;AN001;
   mov  [packet_buffer],offset DIR_SECTOR
   mov  word ptr [packet_sectors],1
   xor  ax,ax			        ; request a read
   call Direct_Access		        ; to read the root
   mov	ax,(util_B shl 8) + no_room     ; load return code in case we fail
   JC   vfl_end

   add	bp,TYPE dir_entry	
   mov	si,bp			
   cmp	BYTE PTR [si],empty     	; empty dir entry?

   JE   vfl_getsizes

   cmp  BYTE PTR [si],deleted           ; deleted ?

   JE   vfl_getsizes
;
; Someone owns the second entry
;
   mov  di,OFFSET FCBDOS	        ;   see if it is IBMDOS
   mov  cx,file_spec_length             ; length of name
   rep  cmpsb		                ; compare it
   mov  si,bp	                	; restore pointer to start	       ;AC000;
   JNE  vfl_move_second

   dec	DOSEntFree	                ; indicate we found IBMDOS
   mov	ax,word ptr ds:[si].dir_size_l  ; Get the size of
   mov	word ptr IBMDOS_Low,ax          ;	   file for IBMDOS
   mov	ax,word ptr ds:[si].dir_size_h
   mov	word ptr IBMDOS_High,ax
   JMP SHORT vfl_getsizes
;
; The owner of the second entry isn't IBMDOS!
;
vfl_move_second:
   call Move_DIR_Entry	
   jc   vfl_err

vfl_getsizes:

IFDEF DBLSPACE_HOOKS
;
; Now get the sizes of DBLSPACE.BIN and COMMAND.COM, if they are present
;
   mov  si,offset DOS_BUFFER
   mov  dx,offset TargDblSpace
   mov  cx,DOS_system_atrib
   mov  ah,Find_First
   int  21h
   jc   vfl_getsizes_0

   mov  ax,[si].find_buf_size_l
   mov  DblSpace_Low,ax
   mov  ax,[si].find_buf_size_h
   mov  DblSpace_High,ax

vfl_getsizes_0:
ENDIF

   mov  dx,offset targ_com
   mov  cx,DOS_system_atrib
   mov  ah,Find_First
   int  21h
   jc   vfl_getsizes_1

   mov  ax,[si].find_buf_size_l
   mov  CommandCom_Low,ax
   mov  ax,[si].find_buf_size_h
   mov  CommandCom_High,ax

vfl_getsizes_1:
   clc

vfl_end:
   ret

   ENDPROC Verify_File_Location



   BREAK <SYS - Determine_Free_Space >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Determine_Free_Space
;*******************************************************************************
;
;Last Update: 3/18/87
;
;Description: Determine if there is enough space on the disk, given the free
;	      space and the space taken up by IBMBIO, IBMDOS, DBLSPACE.BIN
;             and COMMAND.COM to install the system files.  Routine will set
;             fail flag and load return
;	      code if there is not enough room.
;
;	      Here we make some VERY IMPORTANT assumptions.
;
;	      1) If IBMBIO exists on the disk currently, we assume it is in the
;		 correct place, i.e. at the front of the data area & contiguous.
;	      2) The stub loader portion of IBMBIO is less than 2048 bytes long.
;		 This number comes about by assuming we will never overlay
;		 anything smaller than 1920 bytes (DOS 1.10 IBMBIO size). This
;		 can be expanded to 2048 if we also assume the smallest possible
;		 cluster length is 512 bytes.
;
;	      Therefore, if we have an empty disk or IBMBIO exists, then we have
;	      enough contiguous room to install the portion of IBMBIO that
;	      requires itself to be contiguous.
;
;             Note there is a design bug in the computation process - we
;             don't account for the size of COMMAND.COM when checking the
;             amount of free space available.  This is a result of the way
;             that Command.com handling has been tacked on, as an afterthought
;             to the normal processing.  So, there are boundary cases on
;             nearly full disks, where we will fail to detect inadequate
;             space for Command.com.  In that case, we will fail when we
;             attempt to copy it.
;
;
;Input: 	    None
;
;Output: no error - CF = 0
;	    error - CF = 1
;		    AX = return code
;			  AH = utility messages
;			  AL = 8d - No room for system on destination disk
;
;Change History:    Created	   5/01/87	   FG
;                   DblSpace support added 11/18/92 MD
;******************* END OF SPECIFICATIONS *************************************

   public Determine_Free_Space

   Determine_Free_Space PROC NEAR


   mov	ah,Get_Drive_Freespace		; get disk free space
   mov	dl,TargDrvNum			; get the drive number
   INT	21h				; Get_Drive_Freespace  <36xx>
					; compute Bytes/Cluster - 16 bit math ok
					;   AX = sectors / cluster
					;   CX = bytes / sector
					;   BX = available clusters
   mul	cx				; get bytes/cluster
					; result left in AX
   mov	Bytes_Per_Cluster,ax		; save this value for Get_Clusters
   mov	Number_Free_Clusters,bx 	; save available space

   mov	ax,IBMBIO_Low			; low result in AX, High result in DX
   mov	dx,IBMBIO_High
   call Get_Cluster			; convert old IBMBIO into cluster size
   add	Number_Free_Clusters,ax 	; add it to available space

   mov	ax,IBMDOS_Low			; low result in AX, High result in DX
   mov	dx,IBMDOS_High
   call Get_Cluster			; convert old IBMDOS into cluster size
   add	Number_Free_Clusters,AX 	; get total number of clusters available

IFDEF DBLSPACE_HOOKS
   cmp  NoDblSpace,TRUE
   je   dfs_nodbl_1                     ; skip this if no Dblspace.bin
   mov	ax,DblSpace_Low			; low result in AX, High result in DX
   mov	dx,DblSpace_High
   call Get_Cluster			; convert old Dblspace.bin into cluster size
   add	Number_Free_Clusters,AX 	; get total number of clusters available

dfs_noDbl_1:
ENDIF

   mov	ax,CommandCom_Low		; low result in AX, High result in DX
   mov	dx,CommandCom_High
   call Get_Cluster			; convert old Command.com into cluster size
   add	Number_Free_Clusters,AX 	; get total number of clusters available

   mov	ax,BIOSLenLow   		; find total size of new DOS and BIOS
   mov	dx,BIOSLenHigh
   call Get_Cluster			; convert new IBMBIO into cluster size
   mov	Need_Clusters,ax		;save new BIO clusters

   mov	ax,DOSLenLow
   mov	dx,DOSLenHigh
   call Get_Cluster			; convert new IBMDOS into cluster size
   add	AX,Need_Clusters		; get total number of clusters needed

IFDEF DBLSPACE_HOOKS
   cmp  NoDblSpace,TRUE
   je   dfs_noDbl_2                     ; skip this if no Dblspace.bin
   mov  ax,DblSpaceLenLow
   mov  dx,DblSpaceLenHigh
   call Get_Cluster
   add  ax,Need_Clusters

dfs_noDbl_2:
ENDIF

   cmp	AX,Number_Free_Clusters 	;Now see if there is enough room
					;	for all of it on the disk
   JNA $$IF149

   mov  ax,(util_B shl 8) + no_room     ; load return code		       ;AC000;
					; - No room for system on dest..
   stc				        ; set fail flag
   JMP SHORT $$EN149

$$IF149:

   clc				        ; reset fail flag

$$EN149:

   ret					;				       ;AN000;

   ENDPROC Determine_Free_Space

   BREAK <SYS - Get_Cluster >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Get_Cluster
;*******************************************************************************
;
;Description:	    Convert bytes to clusters, rounding up to the next
;		    cluster size if needed.
;
;Called Procedures: None
;
;Input: 	    (AX) = Number of bytes
;		    Bytes_Per_Cluster = # of bytes per cluster
;
;Output:	    (AX) = Number of clusters
;
;Registers used:    AX	BX  DX
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START Get_Cluster
;
;	divide size by bytes_per_cluster
;	if there is a remainder
;		round up to next cluster
;	endif
;	ret
;
;	END Get_Cluster
;
;******************-  END  OF PSEUDOCODE -**************************************

   public Get_Cluster

   Get_Cluster PROC NEAR

   mov	bx,Bytes_Per_Cluster		; Bytes/cluster
   div	bx				; divide size by bytes_per_cluster
   cmp	dx,0				; is there a remainder in DX?

;  $if	ne				; if there is a remainder	       ;AC000;
   JE $$IF152
					; we have another cluster to round up
       inc  ax				; round up to next cluster

;  $endif				;				       ;AC000;
$$IF152:

   ret

   ENDPROC Get_Cluster

   BREAK <SYS - Do_SYS >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Do_SYS
;*******************************************************************************
;
;Description: Control routine to handle the transfer of system files from
;	      memory to target drive.
;
;Called Procedures: Create_System
;		    Fill_Memory
;		    Dump_Memory
;
;Input: IBMBIO_Size_Loaded
;	IBMDOS_Size_Loaded
;
;Output: no error - CF = 0
;	    error - CF = 1	  AX = return code (message #)
;
;Change History: Created	5/01/87 	FG
;
;******************* END OF SPECIFICATIONS *************************************

   public Do_SYS

   Do_SYS PROC NEAR

   call CREATE_SYSTEM			; create IBMBIO and IBMDOS, in place
   jc   ds_end

IFDEF NEED_CLUSTER_2
   push ds
   lds  bx,THIS_DPB 		; set up pointer to DPB 	
   mov  [bx.dpb_next_free],2	; reset Allocation to start of disk
   pop  ds				;  so BIOS goes in right place!
ENDIF

$$DO155:
   call Fill_Memory		; read in file from source	   ;C06
   mov	ax,(util_B shl 8) + cant_read_system ; load error RC (assume error)     ;AC000;
   jc   ds_end

   call Dump_Memory		; write out contents of memory to file
   mov	ax,(util_B shl 8) + no_room ; load error RC (assume error)     ;AC000;
   jc   ds_end

   mov	ax,DOSLenHigh		; more DOS to move ?
   or	AX,DOSLenLow		; more low dos
   or	AX,BIOSLenHigh		; more high BIOS
   or	AX,BIOSLenLow		; more low BIOS

IFDEF DBLSPACE_HOOKS
   or   ax,DblSpaceLenLow       ; more Dblspace?
   or   ax,DblSpaceLenHigh
ENDIF

   jnz  $$do155			; if not all files copied

   clc				;	 reset fail flag	       ;AC000;

ds_end:
   ret

   ENDPROC Do_SYS

   BREAK <SYS - Create_System >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Create_System
;*******************************************************************************
;
;Description:       Creates directory entries for system files.
;
;Called Procedures: Create_File
;
;Input: 	    None
;
;Output:	    IBMBIO_Handle
;		    IBMDOS_Handle
IFDEF DBLSPACE_HOOKS
;                   DblSpace Handle
ENDIF
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************

   public Create_System

   Create_System PROC NEAR

   mov	[open_seg],ds
   mov	dx,OFFSET BIOSName		; point to IBMBIO ASCIIZ string
   mov	al,[BIOSEntFree]		; get status of IBMBIO		       ;AN006;
   mov	[EntryFree],al			; update file status (0 = found,1 = not;AN006;

   call Create_File			; create IBMBIO 		       ;AN000;
   JC $$IF162

   mov	dx,OFFSET DOSName		; pointer to IBMDOS ASCIIZ string
   mov	al,[DOSEntFree] 		; get status of IBMDOS		       ;AN006;
   mov	[EntryFree],al			; update file status (0 = found,1 = not;AN006;

   call Create_File			; create IBMDOS 		       ;AN000;
   JC $$IF162

IFDEF DBLSPACE_HOOKS
   cmp  NoDblSpace,TRUE                 ; check if we need to copy Dblspace.bin
   je   $$IF162                         ; jump if not

   mov  dx,OFFSET TargDblSpace          ; create Dblspace.bin entry
   mov  [EntryFree],0                   ; tell Create_File it doesn't need
                                        ;   to delete it first
   call Create_File
   jc   $$IF162
ENDIF

$$IF162:

   ret				

   ENDPROC Create_System

   BREAK <SYS - Create_File >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Create_File
;*******************************************************************************
;
;Last Update: 9/23/87
;
;Description: Remove the read only attribute from IBMBIO and IBMDOS. If
;	      file not found error occurs, it is okay, because it just
;	      means the file was not there. Do create with read-only
;	      hidden, and system file attributes. This is an in place
;	      create if the file exists already.  Close the file after
;             the create, to conserve SFT entries.  We'll open the file
;             again before we write it.
;
;Called Procedures: None
;
;Input: 	    DS:DX = pointer to ASCIIZ string for file create
;
;Output: no error - CF = 0
;	    error - CF = 1
;		    AX = return code
;			  AH = extended DOS errors
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************

   public Create_File

   Create_File PROC NEAR

   mov	ax,(CHMod shl 8) + SetAtrib	; set file attributes to 0
   xor	cx,cx				; set attributes to 0
   mov	[open_off],dx			; save pointer to ASCIIZ for OPEN
   INT	21h				;   CHMod + SetAtrib <4301>)
   JC   crf_cannot_open

   cmp  [EntryFree],0		        ; is file in the correct spot?	       ;AN006;
   JE   crf_got_file

   mov	dx,[open_off]		        ; get pointer to ASCIIZ for UNLINK     ;AN006;
   mov	ax,(UNLINK shl 8)	        ; UNLINK the file
   INT	21h			        ;   UNLINK	     <4100>)	       ;AN006;
   JMP  SHORT crf_got_file

crf_cannot_open:
   call Get_DOS_Error		        ; find out what went wrong	       ;AN000;
   cmp  al,file_not_found		; not there?
   je   crf_got_file                    ; if not there, we'll create it
   stc                                  ; can't handle any other error
   JMP  SHORT crf_error

crf_got_file:
   JC   crf_error

   lds  si,OPEN_PARMS		
   xor  cx,cx			        ; DOS system file atributes	       ;AC005;
   cmp  DOS_VER,0			; running on current DOS ?	       ;AN019;
   JE   crf_use_extopen

   mov	dx,si			        ; DS:DX - file name		       ;AN019;
   mov	ax,(Creat shl 8) + 0	        ; Create file  <3D00>		       ;AN019;
   JMP  SHORT crf_do_create

crf_use_extopen:
   mov	di,cx			
   dec	di			
   mov	bx,open_mode		        ; set up for mode		       ;AN000;
   mov	dx,(openit shl 4) + replaceit   ; create if does not exist,      ;AN000;
  			                ;     replace it if it does
   mov	ax,(ExtOpen shl 8) + 0	        ; ext Open file with attributes of 0   ;AN000;
                			; ExtOpen + SetAtrib  <6C12> CX=0
crf_do_create:
   INT  21h 			        ; do the open
   JC   crf_error
                                        ; now close the file so we can
                                        ; reuse the SFT
   mov  bx,ax
   mov  ah,CLOSE
   int  21h
   jnc  crf_exit

crf_error:
   call Get_DOS_Error		        ; find out what went wrong	       ;AN000;
   mov  ah,DOS_error		        ; load return code (DOS Extended Error);AN000;
   stc				

crf_exit:
   ret					;				       ;AC000;

   ENDPROC Create_File


   BREAK <SYS - Do_End >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Do_End
;*******************************************************************************
;
;Description:	    Finish off with IBMBIOS and IBMDOS
;
;Called Procedures: Close_File
;		    Write_Boot_Record
;
;Input: 	    None
;
;Output: no error - CF = 0
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Do_End
;
;	finish off and close IBMBIOS and IBMDOS (CALL Close_Files)
;	update boot record (CALL Write_Boot_Record)
;	ret
;
;	END  Do_End
;
;******************-  END  OF PSEUDOCODE -**************************************

   public Do_End

   Do_End PROC NEAR

   call Close_File			; finish off & close IBMBIOS and IBMDOS;AN000;

   call Write_Boot_Record		; update boot record		       ;AN000;

   ret					;				       ;AN000;

   ENDPROC Do_End

   BREAK <SYS - Close_File >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Close_File
;*******************************************************************************
;
;Description:	    Set date and time on IBMBIOS, IBMDOS, and DBLSPACE.BIN,
;                   and close them.
;
;Called Procedures: None
;
;Input: 	
;
;Output:	    IBMBIOS, IBMDOS, and DBLSPACE.BIN closed
;
;Change History:    Created	   5/01/87	   FG
;                   Rewritten     11/18/92         MD
;******************* END OF SPECIFICATIONS *************************************

   public Close_File

   Close_File PROC NEAR

   mov  si,offset FHArray               ; point at xfer_data strucs

cf_loop:
   mov  di,[si]                         ; get next file struc
   mov  bx,[di].OutFH                   ; get handle
   mov  dx,[di].FDate                   ; get date
   mov  cx,[di].FTime                   ; get time
   mov  ax,(FILE_TIMES shl 8) + set     ;
   int  21h                             ; set the time stamp

   mov  ah,CLOSE
   int  21h                             ; close the file

   add  si,TYPE FHArray                 ; advance to next file struc
   cmp  word ptr [si],0                 ; end of array?
   jne  cf_loop                         ; no, keep going
;
; Now set the system/hidden/readonly attributes on the files
;
   mov	dx,offset BIOSName		;				       ;AN001;
   mov	ax,(CHMod shl 8) + SetAtrib	; set file attributes to 0
   mov	cx,DOS_system_atrib		; DOS system file atributes
   INT	21h				;   CHMod + SetAtrib <4301>)

   mov	dx,offset DOSName		;				       ;AN001;
   mov	ax,(CHMod shl 8) + SetAtrib	; set file attributes to 0
   mov	cx,DOS_system_atrib		; DOS system file atributes
   INT	21h				;   CHMod + SetAtrib <4301>)

IFDEF DBLSPACE_HOOKS
   cmp  NoDblSpace,TRUE                 ; did we copy Dblspace.bin?
   je   cf_exit                         ; quit if not
   mov	dx,offset TargDblSpace
   mov	ax,(CHMod shl 8) + SetAtrib	; set file attributes to 0
   mov	cx,DOS_system_atrib		; DOS system file atributes
   INT	21h				;   CHMod + SetAtrib <4301>)
ENDIF

cf_exit:
   ret					;				       ;AN000;

   ENDPROC Close_File

   BREAK <SYS - Write_Boot_Record >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Write_Boot_Record
;*******************************************************************************
;
;Description:	    Get a best guess EBPB and get the Media ID or fill the
;		    information in manually. Write out the canned boot record
;		    and then make IOCtl calls to set the EBPB and Media ID.
;
;Called Procedures: Create_Serial_ID
;
;Input: 	    None
;
;Output:	    Boot record on destination media is installed.
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Write_Boot_Record
;
;	get BPB using IOCtl Get Device Parameters (INT21 IOCtl + 0Dh <440d> CX=0860)
;	get volid, ser# and file type using IOCtl Get Media ID (INT21 IOCtl + 0Dh <440d> CX=086E)
;	if error and
;	get Extended error
;	if 'unknown media' - set fields manually
;		compute serial id and put in field (CALL Create_Serial_ID)
;		copy in volume label if available
;		set pointer to FAT1x (CALL FAT_Size)
;		move file system string into Boot_System_ID field
;	else
;		set fail flag
;		load return code (DOS error)
;	endif
;	if no fail flag
;		if fixed media
;			fill in Ext_PhyDrv in canned boot record
;		endif
;		set BPB using data from GET BPB IOCTL
;		write out canned boot record (INT26)
;		set volid, ser# and file type using Set Media ID (INT21 SetID <6900> CX=084E)
;	endif
;	ret
;
;	END  Write_Boot_Record
;
;******************-  END  OF PSEUDOCODE -**************************************

   public Write_Boot_Record

   Write_Boot_Record PROC NEAR

   mov	bl,TargDrvNum			; Drive number			       ;AN000;
   mov	dx,offset DeviceParameters	;				       ;AN000;
   mov	cx,(rawio shl 8) + get_device_parameters ; Generic IOCtl Request       ;AN000;
					;     CX=0860
   mov	ax,(IOCtl shl 8) + generic_ioctl ; get BPB using Set Device Parm       ;AN000;
   INT	21h				; IOCtl + gen_IOCtl_request <440d>     ;AN000;

   cmp	DOS_VER,0ffh			; is it DOS 3.3?		       ;AN019;
;  $if	ne				; only do a GET if DOS 4.00		;AN019;
   JE $$IF185
       lea  dx,IOCtl_Buf		; point to output buffer	       ;AN000;
       mov  ax,(GetSetMediaID shl 8) + 0 ; get volid, ser# and file type       ;AC008;
       INT  21h 			; GetSetMediaID + 0  INT 21 <6900>     ;AC008;

;      $if  c				; error - see if its 'unknown media'   ;AN000;
       JNC $$IF186

	   call Get_DOS_Error		; get error			       ;AN000;
	   cmp	al,old_type_media	;				       ;AN000;

;	   $if	e			;				       ;AN019;
	   JNE $$IF187
	       stc			; do it all manually		       ;AN019;
;	   $else			;				       ;AN019;
	   JMP SHORT $$EN187
$$IF187:
	       clc			; some other dos error occured - le    ;AN019;
;	   $endif			;	       it go by 	       ;AN019;
$$EN187:

;      $endif				;				       ;AN019;
$$IF186:
;  $else				;				       ;AN019;
   JMP SHORT $$EN185
$$IF185:
       stc				; do it all manually		       ;AN019;
;  $endif				;				       ;AN019;
$$EN185:

;  $if	c				; if it is pre 4.00 IBM format	       ;AN000;
   JNC $$IF193

       call Create_Serial_ID		; compute serial id and put in field   ;AN000;

					; find first with type = VOLUME ID

       mov  dx,OFFSET ExtFCB		; set up for FCB call		       ;AN019;
       mov  ah,Dir_Search_First 	; do a find first INT21 	       ;AN019;

       INT  21h 			;      Find_First <11xx>	       ;AN000;

       cmp  al,0			; was a match found?  al = 0 yes       ;AN019;
					;		      al = ff no
;      $if  e				; if so - copy it in		       ;AN000;
       JNE $$IF194

	   lea	si,DOS_BUFFER + 8	; source id is in DTA		       ;AN019;
	   lea	di,IOCtl_Vol_ID 	; destination is in IOCtl_Buf	       ;AN000;
	   mov	cx,file_spec_length	; move 11 bytes worth		       ;AN000;
	   cld				; copy it in			       ;AN000;
	   rep	movsb			;				       ;AN000;

;      $else
       JMP SHORT $$EN194
$$IF194:

	   clc				; leave it as default - NO NAME

;      $endif				; endif 			       ;AN000;
$$EN194:

					; NOTE:
					; since the GET MEDIA ID failed - its
					; pre 32 bit fat  - so no 32 bit math
					; is required.
       call FAT_Size			; set pointer to FAT1x		       ;AN000;

       mov  cx,FAT_len			; move file system string into	       ;AN000;
					;     Boot_System_ID field
       lea  di,IOCTL_File_Sys		; update buffer 		       ;AN000;
       cld				;				       ;AN000;
       rep  movsb			;				       ;AN000;
;  $endif
$$IF193:

;  $if	nc				;				       ;AN000;
   JC $$IF198

       lea  si,DeviceParameters.DP_BPB
       lea  di,boot.EXT_BOOT_BPB
       mov  cx,type EXT_BPB_INFO
       cld
       rep  movsb

       cmp  DeviceParameters.DP_BPB.BPB_MediaDescriptor,hard_drive ; is it Hard drive?;AC000;

;      $if  e				; if its a hard drive		       ;AC000;
       JNE $$IF199

					; NOTE: The physical hard drive number
					;	is placed in the third byte from
					;	the end in the boot sector in
					;	DOS 3.2. This is a change from
					;	the previous DOS versions.

					; fill in PhyDrv in canned boot record
	   mov	al,80h			; (set physical hard drive number)     ;AC016;
;      $else
       JMP SHORT $$EN199
$$IF199:
	   xor	al,al			; (set physical drive number to zero)  ;AC016;
;      $endif				;				       ;AC000;
$$EN199:

       mov  BOOT.EXT_PHYDRV,al		; (set physical hard drive number)     ;AC016;

       cmp  DOS_VER,0			;				       ;AN019;
;      $if  ne				; copy IOCTL stuff into boot record    ;AN019;
       JE $$IF202
					;     (no set ID available for 3.3)
	   lea	si,IOCtl_Ser_No_Low	; point to source buffer (IOCTL)       ;AN000;
	   lea	di,BOOT.EXT_BOOT_SERIAL ; point to target buffer (BOOT record) ;AN000;

	   mov	cx,IOCTL_Ser_Vol_SyS	; move serial # , Volid , System       ;AN019;
	   cld				;				       ;AN019;
	   rep	movsb			;				       ;AN019;
;      $endif				;				       ;AN019;
$$IF202:

       xor  cx,cx			; Sector 0			       ;AN019;
       mov  [packet],cx 		; set starting sector as 0	       ;AN019;
       mov  bx,offset BOOT		;				       ;AN019;
       mov  packet_buffer[0],bx 	;				       ;AN019;
       mov  word ptr [packet_sectors],1 ;				       ;AN019;
       mov  ah,1			; request a write		       ;AN019;
       call Direct_Access		;				       ;AN019;

;      $if  c				;				       ;AC000;
       JNC $$IF204
;      $endif				;				       ;AC000;
$$IF204:

       cmp  DOS_VER,0			;				       ;AN019
;      $if  e				; only do a SET if DOS 4.00
       JNE $$IF206
	   mov	bl,TargDrvNum		; Drive number			       ;AN000;
	   lea	dx,IOCtl_Buf		; point to output buffer	       ;AN000;
	   mov	ax,(GetSetMediaID shl 8) + 1 ; set volid, ser# and filetype    ;AC008;
	   INT	21h			; GetSetMediaID + 1  INT 21 <6901>     ;AC008;
;      $endif				;AN019;
$$IF206:

;  $endif				;				       ;AC000;
$$IF198:
   ret

   ENDPROC Write_Boot_Record

   BREAK <SYS - FAT_Size >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: FAT_Size
;*******************************************************************************
;
;Description:	    Determine FAT Type (12 or 16)
;
;		    NOTE: This routine is only called if the IOCtl call for
;			  Get Media Type FAILS with an extended error of
;			  'Unknown media type'.  This indicates it is a
;			  pre DOS 4.00 media (ie: it MUST be a 12 or old style
;			  16 bit FAT
;
;			  This is the same algorithm used by FORMAT
;
; Algorithm:
;
; UsedSectors = number of reserved sectors
;	 + number of FAT Sectors	( Number of FATS * Sectors Per FAT )
;	 + number of directory sectors	( 32* Root Entries / bytes Per Sector )
;
; t_clusters = ( (Total Sectors - Used Sector) / Sectors Per Cluster)
;
;   if T_Clusters <= 4086 then it a FAT12 - else - its a FAT16
;
;Called Procedures: None
;
;Input: 	    EBPB of Target media in memory
;
;Output:	    SI: points to  "FAT12    "
;			       or  "FAT16     "
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  FAT_Size
;
;	Calculate the number of directory sectors
;	Calculate and add the number of FAT sectors
;	Add in the number of boot sectors
;	subtract used sectors from total sectors
;	if <= FAT THRESHOLD then
;		set pointer to FAT12
;	else
;		set pointer to FAT12
;	endif
;
;	ret
;
;	END  FAT_Size
;
;******************-  END  OF PSEUDOCODE -**************************************

   public FAT_Size

   FAT_Size PROC NEAR

					;--------------------------
					; Calculate UsedSectors
					;---------------------------

					; Calculate the number of directory sectors

   mov	ax, deviceParameters.DP_BPB.BPB_RootEntries ;			       ;AN000;
   mov	bx, TYPE dir_entry		;				       ;AN000;
   mul	bx				;				       ;AN000;
   add	ax, deviceParameters.DP_BPB.BPB_BytesPerSector ;		       ;AN000;
   dec	ax				;				       ;AN000;
   xor	dx,dx				;				       ;AN000;
   div	deviceParameters.DP_BPB.BPB_BytesPerSector ;			       ;AN000;
   mov	cx,ax				;				       ;AN000;

					; Calculate the number of FAT sectors

   mov	ax, deviceParameters.DP_BPB.BPB_SectorsPerFAT ; 		       ;AN000;
   mul	deviceParameters.DP_BPB.BPB_NumberOfFATs ;			       ;AN000;

					; Add in the number of boot sectors

   add	ax, deviceParameters.DP_BPB.BPB_ReservedSectors ;		       ;AN000;
   add	cx,ax				;				       ;AN000;

					;--------------------------
					; Calculate t_clusters
					;--------------------------

   mov	ax, deviceParameters.DP_BPB.BPB_TotalSectors ;			       ;AN000;

   sub	ax,cx				;Get sectors in data area	       ;AN000;
   xor	dx,dx				;				       ;AN000;
   xor	bx,bx				;				       ;AN000;
   mov	bl,deviceParameters.DP_BPB.BPB_SectorsPerCluster ;		       ;AN000;
   div	bx				;Get total clusters		       ;AN000;
   cmp	ax,BIG_FAT_THRESHOLD		;Is clusters < 4086?		       ;AN000;

;  $if	be				; if less then its a FAT12	       ;AN000;
   JNBE $$IF209
       lea  si,FAT_12			;				       ;AN000;
;  $else				;				       ;AN000;
   JMP SHORT $$EN209
$$IF209:
       lea  si,FAT_16			;				       ;AN000;
;  $endif				;				       ;AN000;
$$EN209:

   clc					; leave cleanly

   return				;				       ;AN000;

   ENDPROC FAT_Size

   BREAK <SYS - Create_Serial_ID >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Create_Serial_ID
;*******************************************************************************
;
;Description:	    Create unique 32 bit serial number by getting current date
;		    and time and then scrambling it around
;
;Called Procedures: None
;
;Input: 	    None
;
;Output:	    serial number installed in Boot_Serial
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Create_Serial_ID
;
;	Get date (INT21 Get_Date + 00 <2A00>)
;	Get time (INT21 Get_Time + 00 <2C00>)
;	Boot_Serial+0 = DX reg date + DX reg date
;	Boot_Serial+2 = CX reg time + CX reg time
;	ret
;
;	END  Create_Serial_ID
;
;******************-  END  OF PSEUDOCODE -**************************************

   public Create_Serial_ID

   Create_Serial_ID PROC NEAR

   mov	ax,(Get_Date shl 8) + not_used	; Get date			       ;AN000;
   INT	21h				; Get_Date + not_used <2A00>	       ;AN000;
   mov	ax,(Get_Time shl 8) + not_used	; Get time			       ;AN000;
   INT	21h				; Get_Time + not_used <2C00>	       ;AN000;
   add	dx,dx				; Boot_Serial+0 = DX (date) + DX (date);AN000;
   add	cx,cx				; Boot_Serial+2 = CX (time) + CX (time);AN000;
   mov	IOCtl_Ser_No_Low,dx		; SERIAL # - low		       ;AN000;
   mov	IOCtl_Ser_No_Hi,cx		; SERIAL # - hi 		       ;AN000;

   ret					;				       ;AN000;

   ENDPROC Create_Serial_ID

   BREAK <SYS - Message >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Message
;*******************************************************************************
;
;Description:  Display a message
;
;Called Procedures: SYSDISPMSG, Get_DOS_Error
;
;Input: 	    (AL) message number
;		    (AH) message class
;			  = C - DS:SI points to sublist
;
;Output: no error   AX = 0
;	    error - AX = error exit code
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Message
;
;	if DOS error
;		call Get_DOS_error
;	endif
;	move message class into place
;	reset insert  (CX)
;	reset response	(DL)
;	set output handle (BX)
;	if CLASS requires insert
;		load insert required
;	if CLASS requires response
;		flush keystroke buffer
;		load response required (Dir CON in no echo)
;	endif
;	call SysDispMsg to display message
;	if error or
;		call Get_DOS_error
;		call SysDispMsg to try again
;	if not success message
;		load error exit code
;	else
;		load success exit code
;	endif
;	ret
;
;	END  Message
;
;******************-  END  OF PSEUDOCODE -**************************************

   public Message

   Message PROC NEAR

   xor	dx,dx				; reset response  (DL)		       ;AN000;
   xor	cx,cx				; reset insert	(CX)		       ;AC024;
   dec	dh				; assume CLASS is Utility	       ;AN000;

   cmp	ah,PARSE_Error			;				       ;AN000;

;  $if	be,and				; if DOS or PARSE error 	       ;AN000;
   JNBE $$IF212

   mov	dh,ah				;				       ;AN000;

;  $if	e,and				; if PARSE error		       ;AN024;
   JNE $$IF212
   cmp	al,reqd_missing 		;				       ;AC024;
;  $if	ne				; and if theres something there        ;AC024;
   JE $$IF212

       push cs				; set up for insert		       ;AN024;
       pop  [insert_ptr_seg]		;   (offset set by parse routine)      ;AN024;
       mov  cs:[si],dl			; make it an ASCIIZ string	       ;AN024;
       mov  insert_number,dl		; zero out for %0
       mov  insert_max,030h		; set length to something reasonable   ;AN024;
       inc  cx				; there's an insert                    ;AC024;
       lea  si,SUBLIST			; point to the sublist		       ;AC024;

;  $endif				;				       ;AN024;
$$IF212:


   cmp	ah,DOS_Error			;				       ;AN000;

;  $if	be				; if DOS error			       ;AC019;
   JNBE $$IF214

       call Get_DOS_error		; to find out what message to display  ;AN000;
       mov  dh,DOS_Error		; ensure message type is DOS_Error     ;AN019;

;  $endif				;				       ;AN000;
$$IF214:

   mov	bx,STDERR			; set output handle (BX)	       ;AN000;

;M010
   cmp	ax,(util_B shl 8) + done 	; "System transferred" msg?
   jne  msg20                           ;   No, jump: output to STDERR.
   mov  bx,STDOUT                       ;   Yes: output to STDOUT.
msg20:
;M010

   cmp	ah,util_C			; is it CLASS C 		       ;AN000;

;  $if	e				; CLASS C requires insert	       ;AN000;
   JNE $$IF216

       inc  cx				; load insert required		       ;AN000;

;  $endif				;				       ;AN000;
$$IF216:

   cmp	ah,util_D			; is it CLASS D 		       ;AN000;

;  $if	e				; CLASS D requires response	       ;AN000;
   JNE $$IF218

       mov  dl,DOS_CON_INP		; load response required  - con: input ;AN000;

;  $endif				;				       ;AN000;
$$IF218:

   xor	ah,ah				;				       ;AN000;


   call SysDispMsg			; to display message		       ;AN000;

;  $if	c,and				; error and...............	       ;AN000;
   JNC $$IF220

   call SysDispMsg			; to try again		 :	       ;AN000;

;  $if	c				; if reaaly bad .........:	       ;AN000;
   JNC $$IF220

       mov  ax,return_error		; load error exit code		       ;AN000;

;  $else				;				       ;AN000;
   JMP SHORT $$EN220
$$IF220:

       mov  ax,success			; load success exit code	       ;AN000;

;  $endif				;				       ;AN000;
$$EN220:

   ret					;				       ;AN000;

   ENDPROC Message


   BREAK <SYS - Get_DOS_Error >
;******************* START OF SPECIFICATIONS ***********************************
;Routine name: Get_DOS_Error
;*******************************************************************************
;
;Description:  Call DOS to obtain DOS extended error #
;
;Called Procedures: None
;
;Input: 	    None
;
;Output:	    AX = error number
;
;Change History:    Created	   5/01/87	   FG
;
;******************* END OF SPECIFICATIONS *************************************
;******************+ START OF PSEUDOCODE +**************************************
;
;	START  Get_DOS_Error
;
;	call DOS for extended error (INT21 GetExtendedError + 00 <5900>)
;	set up registers for return
;	ret
;
;	END  Get_DOS_Error
;
;******************-  END  OF PSEUDOCODE -**************************************

   public Get_DOS_Error

   Get_DOS_Error PROC NEAR

   push bx
   mov	ax,(GetExtendedError shl 8) + not_used ; call DOS for extended error   ;AN000;
   xor	bx,bx
   push es				;				       ;AN000;
   INT	21h				;    GetExtendedError + not_used <5900>;AN000;
   pop	es
   pop	bx				;				       ;AN000;
   xor	cx,cx				; reset insert	(CX)		       ;AC024;

   ret					;				       ;AN000;

   ENDPROC Get_DOS_Error

ifdef DBCS
;------------------------------------------------------------------------------
;
;	***** DBCS functions *****
;

;
;	Test if the character is DBCS Lead Byte
;
;	input:	AL = character to check
;	outpit:	ZF = 1 if DBCS Lead Byte
;

DBCSLeadByteTable	dd	0

IsDBCSLeadByte		proc	near
	push	ax
	push	si
	push	ds
	lds	si,cs:DBCSLeadByteTable
	cmp	word ptr cs:DBCSLeadByteTable+2,0
	jnz	idlb_check		; if table is already set
	push	ax
	mov	ax,6300h
	int	21h			; get DBCS lead byte table
	pop	ax
	mov	word ptr cs:DBCSLeadByteTable,si
	mov	word ptr cs:DBCSLeadByteTable+2,ds
idlb_check:
	cmp	word ptr [si],0
	jz	idlb_not		; if end of table
	cmp	al,[si]
	jb	idlb_next		; if below low value
	cmp	al,[si+1]
	jbe	idlb_yes		; if below high value
idlb_next:
	add	si,2			; do next
	jmp	short idlb_check
idlb_not:
	or	al,1			; reset ZF
	jmp	short idlb_end
idlb_yes:
	and	al,0			; set ZF
idlb_end:
	pop	ds
	pop	si
	pop	ax
	ret
IsDBCSLeadByte		endp

;
;	Check if the character position is at Tail Byte of DBCS
;
;	input:	es:bx = start address of the string
;		es:di = character position to check
;	output:	ZF = 1 if at Tail Byte
;
CheckDBCSTailByte	proc	near
	push	ax
	push	cx
	push	di
	mov	cx,di			; save character position
cdtb_check:
	cmp	di,bx
	jz	cdtb_next		; if at the top
	dec	di			; go back
	mov	al,es:[di]			; get character
	call	IsDBCSLeadByte
	jz	cdtb_check		; if DBCS lead byte do next
	inc	di			; adjust
cdtb_next:
	sub	cx,di			; if the length is odd then
	xor	cl,1			; the character position is
	test	cl,1			; at the tail byte
	pop	di
	pop	cx
	pop	ax
	ret
CheckDBCSTailByte	endp

;------------------------------------------------------------------------------
endif


   CODE ENDS

   include msgdcl.inc

   END	START

