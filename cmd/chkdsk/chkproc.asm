;******************************************************************************
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;
;  Change Log:
;
;    Date    Who   #			  Description
;  --------  ---  ---  ------------------------------------------------------
;  03/21/90  EGH  C00  Cleaned up build by removing unused variables, declaring
;		       functions properly, changing long JMPs to short JMPs,
;		       etc.
;  03/21/90  EGH  C02  Problem fixed - volume name was being displayed as a
;		       file if /V switch was used.  Fix is to not display file
;		       name if volume attribute set.  STR #2004
;
;******************************************************************************
TITLE	CHKPROC - PART1 Procedures called from chkdsk
page	,132					;

	.xlist
	include chkseg.inc
	INCLUDE CHKCHNG.INC
	INCLUDE DOSSYM.INC
	INCLUDE CHKEQU.INC
	INCLUDE CHKMACRO.INC
	include pathmac.inc
	include syscall.inc
	.list


DATA	SEGMENT PUBLIC PARA 'DATA'
	EXTRN	FIXMES_ARG:word,DIREC_ARG:word
	EXTRN	NULDMES:byte,NULNZ:byte,BADCLUS:byte,NORECDOT:byte
	EXTRN	NoRecDDot:Byte
	EXTRN	BADCHAIN:byte,NDOTMES:byte,CDDDMES:byte
	EXTRN	NORECDDOT1:byte,NORECDDOT2:byte,NORECDDOT3:byte
	EXTRN	STACKMES:byte
	EXTRN	BADDPBDIR:byte, BadSubDir:byte
	EXTRN	BADTARG_PTR:byte,BADTARG2:byte,JOINMES:byte
	EXTRN	PTRANDIR:byte,PTRANDIR2:byte
	EXTRN	CROSS_ARG:word,NOISY_ARG:word
	EXTRN	FILE_ARG1:WORD,FILE_ARG2:WORD,FILE_ARG:word
	EXTRN	DOTMES:byte,NOISY:byte,DOTENT:byte,HAVFIX:byte
	EXTRN	DOFIX:byte,DIRBUF:byte,PARSTR:byte,DDOTENT:byte
	EXTRN	NUL:byte,ERRSUB:word,SECONDPASS:byte,ALLFILE:byte
	EXTRN	HIDCNT:dword,HIDSIZ:word,FILCNT:dword,FILSIZ:word	   
	EXTRN	DIRCNT:dword							
	EXTRN	DIRSIZ:word							
	EXTRN	DIRTYFAT:byte,
	EXTRN	HECODE:byte
	EXTRN	ALLDRV:byte,FIXMFLG:byte,DIRCHAR:byte
	EXTRN	BIGFAT:byte,EOFVAL:word,BADVAL:word
	Extrn	fTrunc:BYTE							
	Extrn	dirsec:word							

	EXTRN	THISDPB:dword,DOTSNOGOOD:byte,NUL_ARG:byte,STACKLIM:word
	EXTRN	ZEROTRUNC:byte,NAMBUF:byte,SRFCBPT:word,FATMAP:word
	EXTRN	ISCROSS:byte,MCLUS:word,CSIZE:byte,SSIZE:word,fattbl:byte
	EXTRN	DSIZE:word,ARG1:word,ARG_BUF:byte,TMP_SPC:BYTE
	EXTRN	SECBUF:word
;;	EXTRN	Inv_XA_Msg:Byte,Alloc_XA_Msg:Byte
	Extrn	Data_Start_Low:Word,Data_Start_High:Word
	EXTRN	Read_Write_Relative:Byte
	EXTRN	MClus:Word,Chain_End:Word
	EXTRN	ExitStatus:byte
	EXTRN	longpath1:byte, longpath2:byte, longpath3:byte

XA_Buffer XAL	<>				;XA buffer space to read in 1st sector	
Head_Mark db	0				;Flag for MarkMap			
BClus	dw	0				;Bytes/Cluster
public cross_clus
Cross_Clus dw	0				;Cluster crosslink occurred on
Cluster_Count dw 0				;				
First_Cluster dw 0				;				
Previous_Cluster	dw	0		;				
XA_Pass db	0				;				
File_Size_High dw 0				;				
File_Size_Low dw 0				;				
Chain_Size_Low	dw	0			;				
Chain_Size_High dw	0			;				


public Head_Mark
public BClus
public Cluster_Count
public First_Cluster
public Previous_Cluster
public XA_Pass
public File_Size_High
public File_Size_Low
public Chain_Size_Low
public Chain_Size_High
DATA	ENDS


CODE	SEGMENT PUBLIC PARA 'CODE'
ASSUME	CS:DG,DS:DG,ES:DG,SS:DG
;Structures used by DIRPROC
SRCHFCB STRUC
	DB	44 DUP (?)
SRCHFCB ENDS
SFCBSIZ EQU	SIZE SRCHFCB
	EXTRN	PRINTF_CRLF:NEAR,SUBERRP:NEAR,FCB_TO_ASCZ:NEAR
	EXTRN	FIGREC:NEAR,EPRINT:NEAR
	EXTRN	DOINT26:NEAR,PROMPTYN:NEAR
	EXTRN	DOTCOMBMES:NEAR,FATAL:NEAR,MARKMAP:NEAR,GETFILSIZ:NEAR
	EXTRN	SYSTIME:NEAR, Read_Disk:Near,DoCRLF:Near
	extrn	crosschk:near
	extrn	UNPACK:near, PACK:near
	extrn	ChkDskExit:near

public DOTDOTHARDWAY, NODOT, DOEXTMES1, MESD1, CANTREC, DOTGOON, NODDOT
public DOEXTMES2, MESD2, NFIX, CANTREC2, NULLDIRERR, DOEXTMES3, DOTSBAD
public dotsbad2, DIRPROC, STACKISOK, NOPRINT, JOINERR
public NONULLDERr, DOTOK, DATTOK, DLINKOK, BADDSIZ, DSIZOK, CHKDOTDOT
public DOTDOTOK, DDATTOK, DDLINKOK, BADDDSIZ, DDSIZOK, ROOTDIR
public DODDH, DIRDONE, MOREDIR, FPROC1, NOPRINT2, HIDENFILE, NORMFILE, NEWDIR
public DPROC1, CONVDIR, DPROC2, CANTTARG, BogusDir, ASKCONV
public PRINTTRMES, CROSSLOOK, CHLP, CROSSLINK, CHAINDONE
public FIXENT2, RET20, FIXENT, GETENT, CLUSISOK
public SKIPLP, GOTCLUS, DOROOTDIR, RDRETRY, RDOK2, WANTROOT, CHECKNOFMES, ret14
public CHECKERR, get_currdirERR, ok_pri_dir, get_thiselERR, ok_pri_el
public get_THISEL, get_THISEL2, get_currdir, GET_END, ERRLOOP, LPDONE
public CHECK_SPLICE, NT_SPLC, MarkFAT, Bad_Cluster, Check_Chain_Sizes
public print_filename								
public NOTROOT									
public FIXDOT									


	pathlabl chkproc
SUBTTL	DIRPROC -- Recursive directory processing
PAGE
;**************************************************************************
; DOTDOTHARDWAY - change dir to the previous directory using '..'
;
; called by -
;
; inputs - parse string '..'
;
; outputs - new default directory
;
;NOTE
; On versions of DOS < 2.50 "cd .." would fail if there was no ".."
;    entry in the current directory. On versions >= 2.50 "cd .."
;    is handled as a string manipulation and therefore should always
;    work. On < 2.50 this routine didddled the current directory string
;    INSIDE THE DOS DATA STRUCTURES. This is no longer desirable, or
;    needed.
;**************************************************************************
procedure dotdothardway,near
	MOV	DX,OFFSET DG:PARSTR
	DOS_Call ChDir				;				
	RET
endproc dotdothardway


;**************************************************************************	
; NODOT - come here if there is no . entry in the first entry of the sub-	
;	  directory.  The . entry is a pointer to the subdirectory itself.	
;	  The entry from the search first did not find it, and the subdir is	
;	  not joined.  So, try to put a new . entry into the first slot.	
;										
; called by - nonullderr							
;										
; inputs -  SI - points to arg_buf, which is a filespec 			
;	    DI - points to tmp_spc, which is a filespec 			
;	    AX - return value from search first 				
;										
; outputs - if the /f parm was entered, tries to replace the . entry		
;	    AX - saves the return value from search first			
;										
; logic: 1. go display error messages.	Different messages are displayed,	
;	    depending on /f and /v parms.					
;										
;	2. go get the sector number and read it into ram			
;										
;	3. if the first entry is erased (begins with hex e5), then we can	
;	   fill it with the corrected . entry. Otherwise, go to #6.		
;										
;	4. So, fill entry with all the dir fields - name, ext, attr, date,	
;	   time, size, cluster #.						
;										
;	5. write it back to disk.						
;										
;	6. go check out the .. entry						
;**************************************************************************	
NODOT:						;No .				
	PUSH	AX			;save the return value from search 1st	
;display msgs				;;;;;;;;jnz	doextmes1		
	CMP	[NOISY],OFF		;was /v parm entered?			
;	$IF	Z			;display no /v msgs			
	JNZ $$IF1
	    call    suberrp							
					;;;;;;;;jmp	short mesd1		
;	$ELSE				;display /v msgs			
	JMP SHORT $$EN1
$$IF1:
DOEXTMES1:  mov     si,offset dg:dotmes ;first find out where we are		
	    call    get_currdirerr						
	    mov     dx,offset dg:ndotmes ;print dir, dot, and 'not found' msg	
	    call    eprint							
;	$ENDIF									
$$EN1:
										
;go find the sector								
MESD1:	XOR	AX,AX			;set entry number to zero		
	PUSH	BX			;save					
	PUSH	BP			;save					
	CALL	GETENT			;get the sector number			
	POP	BP			;restore bp				
	PUSH	BP			;put it back				
	CMP	BYTE PTR [DI],0E5H	;is this 1st entry erased/open? 	
;	$if	nz								
	JZ $$IF4
;cant fill . entry			 ;JNZ	  CANTREC			
CANTREC:    INC     [DOTSNOGOOD]						
	    CMP     [NOISY],OFF 		    ;				
;	    $if     nz								
	    JZ $$IF5
						 ;JZ	  DOTGOON		
		MOV	DX,OFFSET DG:NORECDOT					
		CALL	EPRINT							
;	    $endif								
$$IF5:
;C00	    jmp     dotgoon							
	    jmp     short dotgoon					   ;C00 	   
;	$endif									
$$IF4:

;get filename
fixdot: MOV	SI,OFFSET DG:DOTENT	;point to valid . entry
	MOV	CX,11			;move filename and ext
	REP	MOVSB				;Name
	PUSH	AX			;save disk number
;move attr byte
	MOV	AL,ISDIR		;hex 10
	STOSB					;Attribute
; Add in time for directory - BAS July 17/85
	ADD	DI,10
	push	dx			;save starting sector number		
	CALL	SYSTIME
	STOSW					; Time
	MOV	AX,DX
	STOSW					; Date
	MOV	AX,[BP+6]
	STOSW					;Alloc #
	XOR	AX,AX
	STOSW
	STOSW					;Size
	pop	dx			;restore starting sector number 	
	POP	AX			;
;write back to disk
	MOV	[HAVFIX],1			;Have a fix
	CMP	[DOFIX],0		; /f parm entered?
;	$if	nz								
	JZ $$IF8
					 ;JZ	  DOTGOON			
	    MOV     CX,1							
	    CALL    DOINT26							
					 ;JMP	  SHORT DOTGOON 		
;	$endif									
$$IF8:
;go check out .. entry
DOTGOON: POP	 BP
	POP	BX
	POP	AX
	MOV	SI,OFFSET DG:DIRBUF
	JMP	CHKDOTDOT			;Go look for ..
;*****************************************************************************




NODDOT	label	far				;No ..
	PUSH	AX				;Return from SRCH
	CMP	[NOISY],OFF			;				
	JNZ	DOEXTMES2
	CALL	SUBERRP
	JMP	SHORT MESD2
DOEXTMES2:
	MOV	SI,OFFSET DG:PARSTR
	CALL	get_currdirERR
	MOV	DX,OFFSET DG:NDOTMES
	CALL	EPRINT

MESD2:
	MOV	AX,1
	PUSH	BX
	PUSH	BP
	CALL	GETENT
	POP	BP
	PUSH	BP
	CMP	BYTE PTR [DI],0E5H		;Place to put it?
	JNZ	CANTREC2			;Nope
	MOV	SI,OFFSET DG:DDOTENT
	MOV	CX,11
	REP	MOVSB				;Name
	PUSH	AX
	MOV	AL,ISDIR
	STOSB					;Attribute
	ADD	DI,10
;
; Add in time for directory - BAS July 17/85
	push	dx			;save starting sector number		
	CALL	SYSTIME
	STOSW					; Time
	MOV	AX,DX
	STOSW					; Date
	MOV	AX,[BP+4]
	STOSW					;Alloc #
	XOR	AX,AX
	STOSW
	STOSW					;Size
	pop	dx			;restore starting sector number 	
	POP	AX
	MOV	[HAVFIX],1			;Got a fix
	CMP	[DOFIX],0
	JZ	NFIX				;No fix if no F, carry clear
	MOV	CX,1
	CALL	DOINT26
NFIX:
	restorereg <bp,bx,ax>							
	MOV	SI,OFFSET DG:DIRBUF
	JMP	far ptr ROOTDIR 			;Process files

CANTREC2:
	restorereg <bp,bx,ax>							
	CMP	[NOISY],OFF			;				
	JZ	DOTSBAD2
	MOV	DX,OFFSET DG:NORECDDOT
;C00	JMP	DOTSBAD
	JMP	SHORT DOTSBAD						   ;C00

NULLDIRERR label far	    ;dir is empty
	CMP	[NOISY],OFF			;				
	JNZ	DOEXTMES3
	CALL	SUBERRP
	JMP	SHORT DOTSBAD2
DOEXTMES3:
	MOV	SI,OFFSET DG:NUL
	CALL	get_currdirERR
	MOV	DX,OFFSET DG:NULDMES
DOTSBAD:					;Can't recover
	mov	[file_arg2],offset dg:badtarg2
	inc	byte ptr [nul_arg]
	MOV	fTrunc,TRUE
	CALL	EPRINT
dotsbad2:
	CALL	DOTDOTHARDWAY
	INC	[DOTSNOGOOD]
	MOV	SP,BP				;Pop local vars
	POP	BP				;Restore frame
	RET	4				;Pop args




PAGE
;***************************************************************************
; DIRPROC - recursive tree walker
;
; called by - main-routine in chkdsk1.sal
;
; inputs    - ax=0
;	    - two words of 0 on the stack
;
;Recursive tree walker
;dirproc(self,parent)
;****************************************************************************
DIRPROC:
    MOV     [DOTSNOGOOD],0   ;Init to dots OK - set . or .. error flag to false
    MOV     [ERRSUB],0			    ;No subdir errors yet
    PUSH    BP				    ;Save frame pointer - 0
    MOV     BP,SP			    ;ffe2 - 2c = ffb6
    SUB     SP,SFCBSIZ			    ;Only local var

; are we at stack overflow ?
    CMP     SP,[STACKLIM]		    ; ffb6 vs. 5943		    
;   $IF     NA
    JA $$IF10
;;;;;;;;JA	STACKISOK
	MOV	BX, StackMsgNo		;Out of stack /* M002 */
	JMP	FATAL
;   $ENDIF
$$IF10:

STACKISOK:
;print the files as they are found
    CMP     [NOISY],off 	    ; off= 0				    
;   $IF     NZ				;if not noisy, dont print filenames
    JZ $$IF12
;;;;;;;;JZ	NOPRINT
	CMP	[SECONDPASS],False		;				
;	$IF	Z		      ;only print on the first pass
	JNZ $$IF13
;;;;;;;;;;;;JNZ     NOPRINT			    ;Don't do it again on second pass
	    MOV     SI,OFFSET DG:NUL
	    CALL    get_CURRDIR
	    mov     dx,offset dg:DIREC_arg	    ;Tell user where we are
	    CALL    PRINTf_crlf
;	$ENDIF
$$IF13:
;   $ENDIF
$$IF12:

; initialize search fcb
NOPRINT:
    MOV     SI,OFFSET DG:ALLFILE    ;extended fcb
    MOV     DI,SP
    PUSH    DI
    MOV     CX,SFCBSIZ		    ;move 44dec bytes
    REP     MOVSB		    ;from allfile (ds:si) to es:di
; find this file
    POP     DX			    ; from push bp
    MOV     BX,DX			    ;BX points to SRCH FCB
    DOS_Call Dir_Search_First		;search for any file		    
;
    CMP     WORD PTR [BP+6],0	 ;attribute byte- root will = zero
;   $if     z
    JNZ $$IF16
	jmp  far ptr rootdir	 ;yes, we are at the root
;   $endif
$$IF16:
	OR	AL,AL		 ;check return code from search first
	JZ	NONULLDERR
	CALL	CHECK_SPLICE			; See if dir is spliced
;	$if	c
	JNC $$IF18
;;;;;;;;;;;;JC	    nulldirerr			    ; Not spliced, error
	    jmp     nulldirerr
;	$endif
$$IF18:
JOINERR:
	MOV	SI,OFFSET DG:NUL
	CALL	get_currdir
	mov	fTrunc,TRUE
	mov	dx,offset dg:joinmes		;				
	call	Printf_Crlf			;				
	mov	dx,offset dg:badtarg2		;				
	call	Printf_CRLF			;				
	CALL	DOTDOTHARDWAY
	MOV	SP,BP				;Pop local vars
	POP	BP				;Restore frame
	RET	4				;Pop args



NONULLDERR:
	MOV	SI,OFFSET DG:DIRBUF + DIRNAM
	MOV	DI,OFFSET DG:DOTENT
	MOV	CX,11
	REP	CMPSB
	JZ	DOTOK				;Got a . as first entry
	push	ax			;save return code from search first	
	CALL	CHECK_SPLICE			; See if dir is spliced
;	$IF	C			;carry means no join on this dir	
	JNC $$IF20
	    pop ax			;restore return code			 
	    jmp nodot			;goto no . entry code			
;	$ELSE				;no carry means dir is joined		
	JMP SHORT $$EN20
$$IF20:
	    pop ax			;restore return code			 
	    jmp joinerr 		;goto join error code			
;	$ENDIF				;no carry means dir is joined		
$$EN20:
;;;;;;;;JNC	JOINERR 			; spliced, stop 		
;;;;;;;;JMP	NODOT				;No .				

DOTOK:
	MOV	SI,OFFSET DG:DIRBUF
	MOV	AL,[SI.DIRATT]
	TEST	AL,ISDIR
	JNZ	DATTOK
	PUSH	SI				;. not a dir?
	MOV	SI,OFFSET DG:DOTMES
						;MOV	 DX,OFFSET DG:BADATT
	mov	dx,offset dg:norecddot2 	;				
	CALL	DOTCOMBMES
	POP	SI
	OR	[SI.DIRATT],ISDIR
	CALL	FIXENT				;Fix it
DATTOK:
	MOV	AX,[SI.DIRCLUS]
	CMP	AX,[BP+6]			;. link = MYSELF?
	JZ	DLINKOK
	PUSH	SI				;Link messed up
	MOV	SI,OFFSET DG:DOTMES
						;MOV	 DX,OFFSET DG:CLUSBAD
	mov	dx,offset dg:norecddot1 	;				
	CALL	DOTCOMBMES
	POP	SI
	MOV	AX,[BP+6]
	MOV	[SI.DIRCLUS],AX
	CALL	FIXENT				;Fix it
DLINKOK:
	MOV	AX,WORD PTR [SI.DIRESIZ]
	OR	AX,AX
	JNZ	BADDSIZ
	MOV	AX,WORD PTR [SI.DIRESIZ+2]
	OR	AX,AX
	JZ	DSIZOK
BADDSIZ:					;Size should be zero
	PUSH	SI
	MOV	SI,OFFSET DG:DOTMES
						;MOV	 DX,OFFSET DG:BADSIZM
	mov	dx,offset dg:norecddot3 	;				
	CALL	DOTCOMBMES
	POP	SI
	XOR	AX,AX
	MOV	WORD PTR [SI.DIRESIZ],AX
	MOV	WORD PTR [SI.DIRESIZ+2],AX
	CALL	FIXENT				;Fix it
DSIZOK: 					;Get next (should be ..)
	MOV	DX,BX
	DOS_Call Dir_Search_Next		;				
CHKDOTDOT:					;Come here after . failure
	OR	AL,AL
	JZ	DOTDOTOK
	 JMP	NODDOT				;No ..
DOTDOTOK:
	MOV	SI,OFFSET DG:DIRBUF + DIRNAM
	MOV	DI,OFFSET DG:DDOTENT
	MOV	CX,11
	REP	CMPSB
;	$if	nz
	JZ $$IF23
	    jmp     noddot
;;; ;;;;;;;;JNZ     NODDOT			    ;No ..
;	$endif
$$IF23:
	MOV	SI,OFFSET DG:DIRBUF
	MOV	AL,[SI.DIRATT]
	TEST	AL,ISDIR
	JNZ	DDATTOK 			;.. must be a dir
	PUSH	SI
	MOV	SI,OFFSET DG:PARSTR
						;MOV	 DX,OFFSET DG:BADATT
	mov	dx,offset dg:norecddot2 	;				
	CALL	DOTCOMBMES
	POP	SI
	OR	[SI.DIRATT],ISDIR
	CALL	FIXENT				;Fix it
DDATTOK:
	PUSH	SI
	MOV	AX,[SI.DIRCLUS]
	CMP	AX,[BP+4]			;.. link must be PARENT
	JZ	DDLINKOK
	MOV	SI,OFFSET DG:PARSTR
						;MOV	 DX,OFFSET DG:CLUSBAD
	mov	dx,offset dg:norecddot1 	;				
	CALL	DOTCOMBMES
	POP	SI
	MOV	AX,[BP+4]
	MOV	[SI.DIRCLUS],AX
	CALL	FIXENT				;Fix it
DDLINKOK:
	MOV	AX,WORD PTR [SI.DIRESIZ]
	OR	AX,AX
	JNZ	BADDDSIZ
	MOV	AX,WORD PTR [SI.DIRESIZ+2]
	OR	AX,AX
;	$if	z
	JNZ $$IF25
;C00	    jmp     DDSIZOK
	    jmp     short DDSIZOK					   ;C00
;	$endif
$$IF25:
BADDDSIZ:					;.. size should be 0
	PUSH	SI
	MOV	SI,OFFSET DG:PARSTR
						;MOV	 DX,OFFSET DG:BADSIZM
	mov	dx,offset dg:norecddot3 	;				
	CALL	DOTCOMBMES
	POP	SI
	XOR	AX,AX
	MOV	WORD PTR [SI.DIRESIZ],AX
	MOV	WORD PTR [SI.DIRESIZ+2],AX
	CALL	FIXENT				;Fix it


;***************************************************************************
; DDSIZOK - search for the next file in this directory
;***************************************************************************
DDSIZOK label far
	MOV	DX,BX				;search for Next entry
	DOS_Call Dir_Search_Next		;func=12			

ROOTDIR label far	;come here after search first .. failure also
	OR	AL,AL			; was a matching filename found?
	JZ	MOREDIR     ;zero = yes 	;More to go
	CMP	WORD PTR [BP+6],0 ;nz=no	;Am I the root?
	JZ	DIRDONE 			;Yes, no chdir
	MOV	DX,OFFSET DG:PARSTR
	DOS_Call ChDir				;				
	JNC	DIRDONE 			;Worked

; NOTE************************************************
;   On DOS >= 2.50 "cd .." should ALWAYS work since it is
;   a string manipulation. Should NEVER get to here.

	CMP	[NOISY],OFF			;				
	JZ	DODDH
	MOV	SI,OFFSET DG:NUL
	CALL	get_currdirERR
	MOV	DX,OFFSET DG:CDDDMES
	CALL	EPRINT
DODDH:
	CALL	DOTDOTHARDWAY			;Try again
DIRDONE:
	MOV	SP,BP				;Pop local vars
	POP	BP				;Restore frame
	RET	4				;Pop args



;*****************************************************************
; found at least one file in this subdir!
;*****************************************************************
MOREDIR:
	MOV	SI,OFFSET DG:DIRBUF  ;point to where ext fcb of found file is
	TEST	[SI.DIRATT],ISDIR    ;attr 010h = sub-directory
	JNZ	NEWDIR				;Is a new directory?
	CMP	[SECONDPASS],False		;no, same dir			
	JZ	FPROC1				;2nd pass here
	TEST	[SI.DIRATT],VOLIDA   		;attr=08
						;Don't chase chains on labels
	jnz	ddsizok
	CALL	CROSSLOOK			;Check for cross links
	JMP	DDSIZOK 			;Next

FPROC1:
	CMP	[NOISY],OFF			;				
	JZ	NoPrint2			;print filenames?
	TEST	[SI.DIRATT],VOLIDA		;Q: attr=08?			 ;C02
	JNZ	NoPrint2			; N: regular file		 ;C02
	call	print_filename

NOPRINT2:
	mov	Cluster_Count,0 		;No clusters for vol label	
	mov	cx,0				;setup cx for 0 size vol labels 
	TEST	[SI.DIRATT],VOLIDA   ;attr=08	;Don't chase chains on labels   
	jnz	ddsizok 			; continue if regular file
	MOV	AL,81H				;Head of file
	mov	di,word ptr [si].DIRESIZ+0	;Get file size
	mov	File_Size_Low,di		;
	mov	di,word ptr [si].DIRESIZ+2	;
	mov	File_Size_High,di		;
	mov	di,[si].DirClus 		;First cluster of file		
	CALL	MARKFAT
	MOV	CX,Cluster_Count		;Get number of clusters
;;;;;;;;PUSH	CX				;Save them
;;;;;;;;CALL	Check_Extended_Attributes	;See if XA exist, and handle	
;;;;;;;;POP	CX				;Get File length clusters
;;;;;;;;ADD	CX,Cluster_Count		;Add in XA clusters
	TEST	[SI.DIRATT],HIDDN		;
	JZ	NORMFILE			;
hidenfile:
	add	word ptr hidcnt,1	;found another hidden file	       
	adc	word ptr hidcnt+2,0	;add high word if > 64k files	     
	add	hidsiz,cx		;it was this many bytes 	     
	JMP	ddsizok 			;Next
NORMFILE:
	add	word ptr filcnt,1	;inc file counter		     
	adc	word ptr filcnt+2,0	;add high word if >64k files	     
	add	filsiz,cx		;add in size of file		     
	JMP	ddsizok 			;Next


;***************************************************************************
; NEWDIR - come here whenever you find another directory entry
; inputs: SI - points to directory entry
;***************************************************************************
NEWDIR:
	CMP	[SECONDPASS],False	;are we on 2nd pass?
	JZ	DPROC1			;zero means no - skip next part
	CALL	CROSSLOOK		;2nd pass - Check for cross links
	JMP	SHORT DPROC2		;goto dproc2
DPROC1: 				;1st pass
	MOV	AL,82H			;Head of dir
	mov	di,[si].DirClus 	;get 1st clus num from dir entry
	mov	File_Size_Low,0 	;Set to zero, shouldn't
	mov	File_Size_High,0	; be looked at for dir
	CALL	MARKFAT
	add	word ptr dircnt,1	;add 1 to the dir counter
	adc	word ptr dircnt+2,0	;add 1 to the high word if carry
	MOV	CX,Cluster_Count	;Add count of clusters in files
	CMP	[ZEROTRUNC],0		;did we modify the file size?
	JZ	DPROC2			;Dir not truncated
CONVDIR:				;yes, dir size truncated
	AND	[SI.DIRATT],NOT ISDIR	;Turn into file
	CALL	FIXENT
	JMP	ddsizok 		;Next
DPROC2:
	add	dirsiz,cx		;add in siz of clusters
; put 4 words on the stack - for next call to dirproc?
	PUSH	[ERRSUB]		;save the fcb ptr from prev dir
	PUSH	BX			;Save my srch FCB pointer
	MOV	AX,[SI].DirCLus 	;get 1st cluster number
	PUSH	AX			; Give him his own first clus pointer
	PUSH	[BP+6]			; His PARENT is me
;copy fcb name to msg string name
	ADD	SI,DIRNAM		;point to name, +08
	MOV	DI,OFFSET DG:NAMBUF
	savereg <di,ax> 		;copy to msg string
	CALL	FCB_TO_ASCZ
	restorereg <ax,di>
;check out validity of dir
	OR	AX,AX			;does the 1st clus in dir point to zero?
	JZ	BogusDir		; no, it is bogus
	mov	dx,di
	DOS_Call ChDir			; chdir to it
	JC	CANTTARG		; carry means bad dir
; go check out new dir
	CALL	DIRPROC
	POP	BX			;Get my SRCH FCB pointer back
	POP	[ERRSUB]		;restore from prev dir
	CMP	[DOTSNOGOOD],0
	JNZ	ASKCONV
	JMP	ddsizok 		;Next
;newdir error routines
CANTTARG:
;cant chdir
	ADD	SP,8			; Clean stack
	mov	SI,dx			; Pointer to bad DIR
	CALL	get_currdirERR		; returns with di pointing PAST end \0
	sub	di, offset dg:arg_buf
	cmp	di, 68			; 64 char len pathlen + C:\ + \0
	jae	PathTooLong
	MOV	DX,OFFSET DG:BADTarg_PTR
	mov	fTrunc,TRUE
	call	printf_crlf
	JMP	ddsizok 			;Next
BogusDir:
;bad dir entry
	ADD	SP,8				; clean off stack
	MOV	SI,DX				; pointer to bad dir
	CALL	get_currdirERR			; output message with dir
	MOV	DX,OFFSET DG:BadSubDir		; real error message
	CALL	EPRINT				; to stderr...
ASKCONV:
	CMP	[SECONDPASS],False		;
	jnz	ptm_0

	cmp	[NOISY],on			; /v entered ?
	jne	printtrmes

	MOV	DX,OFFSET DG:PTRANDIR		;unrecoverable error in directory
	call	printf_crlf			;display msg

PRINTTRMES:
	cmp	DoFix,ON			; are we fixing errors?
	jne	ptm_0				; jump if not

	MOV	DX,OFFSET DG:PTRANDIR2		;either case - "convert dir to file?"
	CALL	PROMPTYN			;Ask user what to do
	jz	ptm_1

ptm_0:
	jmp    ddsizok

ptm_1:
	PUSH	BP
	PUSH	BX
	MOV	AX,[BX+THISENT] 		;Entry number
	CALL	GETENT				;Get the entry
	MOV	SI,DI
	MOV	DI,OFFSET DG:DIRBUF
	PUSH	DI
	ADD	DI,DIRNAM
	MOV	CX,32
	REP	MOVSB				;Transfer entry to DIRBUF
	POP	SI
	PUSH	SI
	MOV	SI,[SI.DIRCLUS] 		;First cluster
	CALL	GETFILSIZ
	POP	SI
	POP	BX
	POP	BP
	MOV	WORD PTR [SI.DIRESIZ],AX	;Fix entry
	MOV	WORD PTR [SI.DIRESIZ+2],DX
	mov	[DOTSNOGOOD],0			; since this dir is converted
						; to a file, our work is over;
						; the parent dir is so far OK
	JMP	CONVDIR 			; convert directory

	; Come here if chdir fails and dir path > 64 characters long.  We don't
	; deal with these well (files down lower appear as lost clusters), so
	; tell the user what's wrong and bail out.  FYI, MS-DOS can't easily
	; create such long paths, but other systems (like Windows/NT) can.

PathTooLong:
	mov	dx, offset dg:longpath1 ; Display long friendly error msg
	call	printf_crlf
	mov	dx, offset dg:longpath2
	call	printf_crlf
	mov	dx, offset dg:longpath3
	call	printf_crlf
	jmp	ChkDskExit		; exit right now

;*****************************************************************************
;end of newdir
;*****************************************************************************



SUBTTL	fat-Look routines
PAGE
;*****************************************************************************
; CROSSLOOK - look at the fat, check for cross linked files
; called by -
;****************************************************************************
CROSSLOOK:
;Same as MRKFAT only simpler for pass 2
	MOV	[SRFCBPT],BX				   
	MOV	BX,SI					   
	MOV	SI,[BX.DIRCLUS] 			   
	CALL	CROSSCHK				   
	JNZ	CROSSLINK				   
;;;;;;;;mov	XA_Pass,False				   

CHLP:
	PUSH	BX					   
	CALL	UNPACK					   
	POP	BX					   
	XCHG	SI,DI					   
	CMP	SI,[EOFVAL]				   
	jae	chaindone				   
;;;;;;;;JAE	Check_XA_Cross				   
	CALL	CROSSCHK				   
	JZ	CHLP					   
	JMP SHORT CROSSLINK				   
							   
Check_XA_Cross: 			;					

;;;;;;;;mov	SI,[BX.DIR_XA]		;See if extended attribute 
;;;;;;;;cmp	si,0			;Is there?		   
;;;;;;;;je	ChainDoneJ		;No if zero		   
;	CALL	CROSSCHK		;Yes, see if crosslinked   
;	JNZ	CROSSLINKJ		;NZ means yes		   
;A_Cross_Loop:				;			   
;	PUSH	BX			;			   
;	CALL	UNPACK			;Get next cluster	   
;	POP	BX			;			   
;	XCHG	SI,DI			;			   
;	CMP	SI,[EOFVAL]		;Reach the end? 	   
;	JAE	ChainDoneJ	       ;Leave if so		   
;	CALL	CROSSCHK		;See if crosslink	   
;	JZ	XA_Cross_Loop		;Go check next cluster if not		
;	jmp	CrossLink		;Go handle crosslink	   
;

;NOCLUSTERSJ: JMP NOCLUSTERS


 CHASELOOP:							   
	 PUSH	 BX						   
	 CALL	 UNPACK 					   
	 POP	 BX						   
	 INC	 CX						   
	 XCHG	 SI,DI						   
	 CMP	 SI,[EOFVAL]					   
	 JAE	 CHAINDONE					   
	 CMP	 SI,2						   
	 JB	 MRKBAD 					   
	 CMP	 SI,[MCLUS]					   
	 JBE	 MRKOK						   
 MRKBAD:					 ;Bad cluster # in chain
	 PUSH	 CX						   
	 PUSH	 DI						   
	 CALL	 get_THISELERR					   
	 MOV	 DX,OFFSET dg:BADCHAIN				   
	 CALL	 EPRINT 					   
	 POP	 SI						   
	 MOV	 DX,0FFFH			 ;Insert EOF	   
	 CMP	 [BIGFAT],0					   
	 JZ	 FAT12_1					   
	 MOV	 DX,0FFFFH					   
 FAT12_1:							   
	 PUSH	 BX						   
	 CALL	 PACK						   
	 POP	 BX						   
	 POP	 CX						   
	 JMP SHORT CHAINDONE					   

 MRKOK:
	 CALL	 MARKMAP					   
	 JZ	 CHASELOOP					   
Public	CrossLink
CROSSLINK:					;File is cross linked
	INC	[ISCROSS]					       
	CMP	[SECONDPASS],False		;		       
	JZ	CHAINDONE			;Crosslinks only on second pass 
	mov	[cross_clus],si 		;Cluster number    
	CALL	get_THISEL					   
	Message File_Arg			;Print file out    
	MOV	DX,OFFSET DG:CROSS_arg		;Print message out 
	CALL	PRINTf_crlf					   
Public	ChainDone
CHAINDONE:
	 TEST	 [BX.DIRATT],ISDIR
	 JNZ	 NOSIZE 			 ;Don't size dirs
	 CMP	 [ISCROSS],0
	 JNZ	 NOSIZE 			 ;Don't size cross linked files
	 CMP	 [SECONDPASS],False		 ;
	 JNZ	 NOSIZE 			 ;Don't size on pass 2  (CX garbage)
	 MOV	 AL,[CSIZE]
	 XOR	 AH,AH
	 MUL	 [SSIZE]
	 PUSH	 AX				 ;Size in bytes of one alloc unit
	 MUL	 CX
	 MOV	 DI,DX				 ;Save allocation size
	 MOV	 SI,AX
	 SUB	 AX,WORD PTR [BX.DIRESIZ]
	 SBB	 DX,WORD PTR [BX.DIRESIZ+2]
	 JC	 BADFSIZ			 ;Size to big
	 OR	 DX,DX
	 JNZ	 BADFSIZ			 ;Size to small
	 POP	 DX
	 CMP	 AX,DX
	 JB	 NOSIZE 			 ;Size within one Alloc unit
	 PUSH	 DX				 ;Size too small
 PUBLIC  BadFSiz
 BADFSIZ:
	 POP	 DX
	 PUSH	 CX				 ;Save size of file
	 MOV	 WORD PTR [BX.DIRESIZ],SI
	 MOV	 WORD PTR [BX.DIRESIZ+2],DI
	 CALL	 FIXENT2			 ;Fix it
	 CALL	 get_THISELERR
	 MOV	 DX,OFFSET DG:BADCLUS
	 CALL	 EPRINT
	 POP	 CX				 ;Restore size of file
 NOSIZE:
	MOV	SI,BX
	MOV	BX,[SRFCBPT]
	RET

 NOCLUSTERS:
						 ;File is zero length
	 OR	 SI,SI
	 JZ	 CHKSIZ 			 ;Firclus is OK, Check size
	 MOV	 DX,OFFSET DG:NULNZ
 ADJUST:
	 PUSH	 DX
	 CALL	 get_THISELERR
	 POP	 DX
	 CALL	 EPRINT
	 XOR	 SI,SI
	 MOV	 [BX.DIRCLUS],SI		 ;Set it to 0
	 MOV	 WORD PTR [BX.DIRESIZ],SI	 ;Set size too
	 MOV	 WORD PTR [BX.DIRESIZ+2],SI
	 CALL	 FIXENT2			 ;Fix it
	 INC	 [ZEROTRUNC]			 ;Indicate truncation
	 JMP	 CHAINDONE

 PUBLIC  ChkSiz
 CHKSIZ:
	 MOV	 DX,OFFSET DG:BADCLUS
	 CMP	 WORD PTR [BX.DIRESIZ],0
	 JNZ	 ADJUST 			 ;Size wrong
	 CMP	 WORD PTR [BX.DIRESIZ+2],0
	 JNZ	 ADJUST 			 ;Size wrong
	 JMP	 CHAINDONE			 ;Size OK


SUBTTL	Routines for manipulating dir entries
PAGE

FIXENT2:
;Same as FIXENT only [SRFCBPT] points to the search FCB, BX points to the entry
	savereg <si,bx,cx>
	MOV	SI,BX
	MOV	BX,[SRFCBPT]
	CALL	FIXENT
	restorereg <cx,bx,si>
RET20:	RET

FIXENT:
;BX Points to search FCB
;SI Points to Entry to fix
	MOV	[HAVFIX],1			;Indicate a fix
	CMP	[DOFIX],0		;did the user enter /f flag?
	JZ	fixret			;zero means no - dont fix it
	savereg <bp,bx,si,si>
	MOV	AX,[BX+THISENT] 		;Entry number
	CALL	GETENT
	POP	SI				;Entry pointer
	ADD	SI,DIRNAM			;Point to start of entry
	MOV	CX,32
	REP	MOVSB
	INC	CL
	CALL	DOINT26
	restorereg <si,bx,bp>
fixret: RET



;*****************************************************************************	
; GETENT - calculate, and read into ram, the sector of the directory entry	
;	   that is invalid.  This entry can be in either the root directory,	
;	   or in a sub-directory.  If it is in the root, it can be in the first 
;	   sector of the root dir, or in a subsequent sector.  If it is in a	
;	   subdirectory, it can be in the first cluster of the subdir, or in	
;	   any subsequent cluster.  It can also be in the first sector of the	
;	   cluster, or in any of the following sectors within that cluster.	
;										
; WARNING!! NOTE!! --> this procedure has a limit on the input value of 64k	
;		       entries.  If the disk fails on an entry in a subdir	
;		       which has an invalid entry past this value, then the	
;		       calling procedure will probably wrap on this word value, 
;		       causing getent to calc the wrong sector, and then	
;		       corrupting the disk.  Not likely, but poss.		
;										
; called by - nodot/mesd1	 - no . entry found  (always subdir)		
;	    - noddot/mesd2	 - no .. entry found (always subdir)		
;	    - askconv/printtrmes - convert dir to file (can be in root) 	
;	    - makfillp		 - find root entry in which to place lost clus	
;										
; inputs - AX - desired entry num (in curr dir, reffed off BP)			
;		0=.   1=..   2=first entry					
;	   DX - number of lost clusters
;	   BP - ptr to extended fcb for this dir				
;	   BP+6 - 1st cluster number of this dir				
;										
; output - AX - contains number of the disk to use for int26			
;	   DI - points to entry in subdir in ram				
;	   DX - low sector number of the dir					
;	   BX - ram offset of the sector					
;	   Read_Write_Relative.Start_Sector_Hi - hi sector number of the dir	
;										
; Regs abused - all of 'em !! (ok, well, maybe not bp...)                       
;										
;logic: 1. make sure there will not be a problem with the cluster number. This	
;	   should not be a problem, since if the cluster number is invalid, it	
;	   should have been flagged by previous routines.			
;										
;	2. calc clus-num & offset						
;	   Entries * bytes/entry / BPS --> number of sectors from the beg of	
;	   the dir.  There are 16 entries per sector (starting at zero).  The	
;	   bytes/entry and bytes/sector are condensed, giving a div by 16,	
;	   instead of "* 32 / 512".  Now we have the first cluster (0-fff7),	
;	   the sector-offset (0-fff), and the entry-offset (0-f).		
;										
;	   forumla: entry (0-ffff)  /  16 = sector-offset (0-fff)  ax		
;					  = entry-offset  (0-f)    dx		
;										
;      3. if we are in the root directory, then we have the correct sector	
;	  number, so just add it to the starting sector number of the		
;	  directory.								
;										
;      4. otherwise, we are in a subdirectory.	Here, we need to get the	
;	  cluster-offset, since the sector-offset can be more than 1 cluster	
;	  in length.  So, divide the sectors by (secs/clus) to get cluster-	
;	  offset.  This value is now a power of 2, from 2 up to 16.		
;										
;	   / sectors/cluster (2-16)   = cluster offset AL			
;				      = sector	offset AH			
;										
;      5. If AL > 0, then we have to walk the fat chain to find the cluster	
;	  where this sector is.  Fortunately, we have the starting cluster	
;	  number (BX), UNPACK will find the next cluster number, and we have	
;	  the number of clusters to jump (AL).	So, move the appropriate	
;	  into the regs, and loop until completed.  Now BX has the correct	
;	  cluster number.							
;										
;      6. Now we need to translate the cluster and sector numbers into an	
;	  absolute, double word, sector number.  FIGREC will do this.		
;										
;      7. Now, from either root dir, or from subdir, we have the absolute	
;	  sector, so set up the regs, and call READ_DISK to read it into ram.	
;	  Now DX contains the sector number (low), and BX points to the 	
;	  sector in ram.							
;										
;      8. Finally, get the entry-offset that we had stored on the stack, and	
;	  translate it into a byte-offset by multpying it times the number of	
;	  bytes per entry (32).  Now DI points to the entry in ram.		
;*****************************************************************************	
GETENT: 									
	mov	bx,[bp+6]		;Get 1st cluster of subdir		
;double check for invalid cluster						
	cmp	bx,[eofval]		;Last entry in cluster? 		
;	$IF	NB								
	JB $$IF39
	    mov     bx,baddpbdirMsg    ;This should never happen /* M002 */	
	    jmp     fatal	    ;Danger, warning Phil Robins
;	$ENDIF									
$$IF39:
										
CLUSISOK:									
;calc cluster number and offset 						
	mov	cx,16			;32 bytes/entry  /  512 bytes/sec	
	xor	dx,dx			;zero out hi word for divide		
	div	cx    ;NOW- bx=first clus, ax=sec-offset, dx=entry-offset	
				    ;NOTE: ax can be > 1 cluster		
;are we at the root?								
	or	bx,bx			;cluster zero?				
;	$IF	Z			;yes, then we are in root dir		
	JNZ $$IF41
	    ;;;;;;;;jz	    wantroot		    ;Cluster 0 means root dir	
WANTROOT:   push    dx			;restored as di- ptr to invalid entry	
	    mov     dx,ax		;get sector offset			
	    add     dx,[dirsec] 	;add in first sector of dir		
	    mov     Read_Write_Relative.Start_Sector_High,0  ;save hi value	
	    ;;;;;;;;;;;JMP     DOROOTDIR ;now ready for int25			
										
;	$ELSE		;not in root dir					
	JMP SHORT $$EN41
$$IF41:
NOTROOT:    div     csize		;divide by sectors/cluster (2-16)	
			     ;AL=# cluster-offset (QUO), AH= sector-offset (REM)
	    mov     cl,al		;get cluster offset from al		
	    xor     ch,ch		;zero out hi byte to make word value	
	    or	    cx,cx    ;do we have more than one cluster worth to go yet? 
;	    $IF     NZ	     ;yes - we have to walk the chain to find it	
	    JZ $$IF43
	    ;;;;;;;;JCXZ    GOTCLUS		    ;jump if cx reg = zero	
		mov	si,bx		    ;move the cluster num for input	
SKIPLP: 	call	unpack		    ;find the next cluster number	
		xchg	si,di		    ;move it into input position	
		loop	skiplp		    ;do for number of cluster-offset	
		mov	bx,si		    ;now we have the cluster number	
;	    $ENDIF								
$$IF43:
										
;calculate the sector from the cluster & sec-offset				
GOTCLUS:    push    dx		     ;restored as di -> entry offset		
	    call    figrec	     ;Convert to sector # - ax=low, dx=hi	
;	$ENDIF	;are we in root dir?						
$$EN41:
										
DOROOTDIR:									
	mov	bx,[secbuf]		;get offset of ram area 		
	mov	al,[alldrv]		;get drive number			
	dec	al			;adjust for int25			
RDRETRY: mov	 cx,1			 ;read 1 sector 			
	call	Read_Disk		;do it					
	jnc	rdok2			;was it good?				
;Need to handle 'Fail' option of critical error here				
	JZ	RDRETRY 							
										
RDOK2:	pop	ax			;get byte-offset into sector		
	mov	cl,5		    ;value of 32= bytes per entry		
	shl	ax,cl		    ;mul entry offset to get byte offset	
	add	ax,bx			;add in offset of dir in ram		
	mov	di,ax								
	mov	al,[alldrv]		;get drive number			
	dec	al			;adjust for int26			
	RET				;di now points to offending entry	
;*****************************************************************************	




CHECKNOFMES:
	MOV	AL,1
	XCHG	AL,[FIXMFLG]
	OR	AL,AL
	JNZ	RET14				;Don't print it more than once
	CMP	[DOFIX],0
	JNZ	RET14				;Don't print it if F switch specified
	mov	dx,offset dg:FIXMES_arg
	CALL	PRINTf_crlf
	call	DoCRLF				;				
ret14:	RET

CHECKERR:
	mov	[ExitStatus], Bad_Exit		;Error, set exit errorlevel
	CALL	CHECKNOFMES
	CMP	[SECONDPASS],False		;				
	RET

get_currdirERR:
	CALL	CHECKERR
	jz	ok_pri_dir
	mov	byte ptr [arg_buf],0
	ret
ok_pri_dir:
	CALL	get_currdir
	ret

get_thiselERR:
	CALL	CHECKERR
	jz	ok_pri_el
	mov	byte ptr [arg_buf],0
ok_pri_el:
	CALL	get_thisel
	RET

get_THISEL:
	MOV	SI,BX
	ADD	SI,DIRNAM
;*****************************************************************************
; called by: checkfiles
; inputs:	AX - number of fragments
;		SI
;*****************************************************************************
get_THISEL2:
	MOV	DI,OFFSET DG:NAMBUF
	PUSH	DI
	CALL	FCB_TO_ASCZ
	POP	SI
get_currdir:
	PUSH	SI
; get drive letter prefix (c:\)
	mov	di,offset dg:arg_buf
	MOV	al,[ALLDRV]
	ADD	al,'@'
	stosb
	MOV	al,[DRVCHAR]
	stosb
	mov	al,[DIRCHAR]
	stosb
	MOV	SI,DI
; get the name of the current directory, and put it into es:di
	MOV	DL,[ALLDRV]
	DOS_Call Current_Dir			;				
GET_END:
;find the end of the string - it will be hex zero
	LODSB
	OR	AL,AL
	JNZ	GET_END
;
	DEC	SI				; Point at NUL
	MOV	DI,SI
	POP	SI			;point to begin of string
	CMP	BYTE PTR [SI],0
	JZ	LPDONE				;If tail string NUL, no '/'
; move '\' for between path and filename
	MOV	al,[DIRCHAR]
	CMP	BYTE PTR [DI - 1],AL
	JZ	ERRLOOP 			; Don't double '/' if root
	stosb
ERRLOOP:
; move filename from ds:si to es:di until find hex zero
	LODSB
	OR	AL,AL
	JZ	LPDONE
	stosb
	JMP	ERRLOOP
LPDONE:
; finish off string with hex zero for asciiz
	mov	al,0
	stosb
	RET

CHECK_SPLICE:
; Carry set if current directory is NOT spliced (joined) onto.
; Carry clear if current directory is spliced (joined) onto.

	MOV	SI,OFFSET DG:NUL
	CALL	get_currdir			; Build ASCIZ text of current dir
						;  at arg_buf
	mov	si,offset dg:arg_buf
	mov	di,offset dg:TMP_SPC
	DOS_Call xNameTrans			;				
	JC	NT_SPLC 			; Say NOT spliced if error
	CMP	WORD PTR [TMP_SPC+1],"\" SHL 8 OR ":"
	JNZ	NT_SPLC
	CMP	BYTE PTR [TMP_SPC+3],0
	JNZ	NT_SPLC
	MOV	AL,BYTE PTR [arg_buf]		; Source drive letter
	CMP	AL,BYTE PTR [TMP_SPC]		; Different from dest if spliced
	JZ	NT_SPLC 			; Drive letter didn't change
	CLC
	RET

NT_SPLC:
	STC
	RET


;*****************************************************************************
;Routine name: MarkFAT
;*****************************************************************************
;
;Description: Trace the fat chain for a single file, marking entries in FATMap,
;	      and handling errors -
;	      (crosslink, truncation, allocation error, invalid cluster entry)
;
; called by :	moredir
;		newdir
;
;Called Procedures: Unpack
;		    Bad_Cluster
;		    MarkMap
;		    Check_Chain_Sizes
;		    ThisEl
;		    Eprint
;
;Change History: Created	5/10/87 	MT
;
;Input: BX = pointer to search FCB
;	AL is head mark with app type =81h
;	SI = points to dir entry
;	DI = cluster entry of file or XA in directory
;	File_Size_Low/High = bytes length directory or XA structure says
;				the data area is
;	SecondPass = TRUE/FALSE
;	XA_Pass = TRUE/FALSE
;	EOFVal = 0FF8h/0FFF8h
;
;Output:
;	 ZEROTRUNC is non zero if the file was trimmed to zero length
;	 ISCROSS is non zero if the file is cross linked
;	 BX,SI preserved
;	 Cluster_Count = number of clusters in chain
;	 carry flag is set if the clusters are ok
;	 fatmap entries - 81h = head of file,
;			  01h = used cluster
;
;Psuedocode
;----------
;
;	ZeroTrunc,IsCross = FALSE, SRFCBPT = BX, Cluster_Count = 0
;	Get file cluster entry (CALL Unpack)
;	IF cluster < 2 or > maximum cluster (MClus)
;	   Go handle invalid cluster (CALL Bad_Cluster)
;	ELSE
;	   SEARCH
;	      Go mark cluster in FATMap (CALL MarkMAP)
;	      Turn off head bit on FATMap marker
;	   EXITIF Crosslink
;	      IsCross = TRUE
;	      IF SecondPass = FALSE
;		 Setup filename for message (CALL ThisEl)
;		 Display crosslink message (Call Eprint)
;	      ENDIF
;	   ORELSE (no crosslink)
;	      Get next cluster (CALL Unpack)
;	      IF Cluster >= EOFVAL [(0/F)FF8h]
;		 Verify file sizes (CALL Check_Chain_Sizes)
;		 clc  (Force loop to end)
;	      ELSE
;		 IF cluster < 2 or > maximum cluster (MClus)
;		    Go handle invalid cluster (CALL Bad_Cluster)
;		    clc  (Force loop to end)
;		 ELSE
;		     stc  (Force loop to keep goining
;		 ENDIF
;	      ENDIF
;	   ENDLOOP clc
;	   ENDSRCH
;	ENDIF
;	ret
;*****************************************************************************
Procedure MarkFAT				;			       
	push	si				
	push	bx				
	mov	Head_Mark,al			;Save flag to put in map	
	mov	ZeroTrunc,False 		;Init values			;     ;
	mov	IsCross,False			;				;     ;
	mov	Cluster_Count,0 		;Init count of clusters 	;     ;
	mov	SrFCBPt,bx			;Pointer to search FCB		;     ;
	mov	First_Cluster,di		;				
	mov	Previous_Cluster,di		;Init pointer
	cmp	di,2				;Cluster < 2?			
;	$IF	B,OR				;    or 			
	JB $$LL46
	cmp	di,MClus			;Cluster > total clusters?	
;	$IF	A				;				
	JNA $$IF46
$$LL46:
	    cmp word ptr [si].dirclus,0 ;if both cluster and size = 0,		
;	    $IF NE,OR			;then its not an error, 		
	    JNE $$LL47
	    cmp word ptr [si].diresiz,0 ;and dont print msg			
;	    $IF NE								
	    JE $$IF47
$$LL47:
		call	Bad_Cluster		     ;Yes, go indicate bad stuff     
;	    $ENDIF
$$IF47:
;	$ELSE					;Cluster in valid range 	
	JMP SHORT $$EN46
$$IF46:
;	   $SEARCH				;Chase the cluster chain	
$$DO50:
	      mov     al,Head_Mark		;Get flag for map 01		
	      call    MarkMap			;Mark the cluster (SI)		
	      push    ax			;Save head mark 		
	      lahf				;Save CY status
	      and     Head_Mark,Head_Mask	;Turn off head bit of map flag	
	      sahf				;Get CY flags back
	      pop     ax			;Get haed mark back		
;	   $EXITIF C				;Quit if crosslink		
	   JNC $$IF50
	      mov     IsCross,True		;Set crosslink flag		
	      cmp     SecondPass,True		;Handle crosslink 2nd pass only 
;	      $IF     E 			;This is first pass		
	      JNE $$IF52
		 mov	 Cross_Clus,di		;Put cluster in message 	
		 push	 bx			;Get dir pointer into bx	
		 push	 si			;				
		 mov	 bx,si			; for the call			
		 call	 Get_ThisELErr		;				
		 mov	 dx,offset DG:Cross_arg ;Specify error message		
		 call	 EPrint 		;Go print file and error	
		 pop	 si			;				
		 pop	 bx			;				
		 Message Cross_Arg		;				
;	      $ENDIF				;				
$$IF52:
;	   $ORELSE				;No crosslink found		
	   JMP SHORT $$SR50
$$IF50:
	      push    si			;Save dir pointer
	      mov     si,di			;Provide current cluster
	      mov     Previous_Cluster,di	;Save current cluster
	      call    UnPack			;Get next cluster entry (di)	
	      inc     Cluster_Count		;Got a cluster			
	      pop     si			;Get dir pointer back
	      cmp     di,EOFVal 		;Is it the last clus in file?	
;	      $IF     AE			;Yes - good chain so far	
	      JNAE $$IF55
		 call	 Check_Chain_Sizes	;Go verify file sizes		
		 clc				;Clear CY to force exit 	
;	      $ELSE				;Not end of chain
	      JMP SHORT $$EN55
$$IF55:
		 cmp	 di,2			;Cluster < 2?			
;		 $IF	 B,OR			;    or 			
		 JB $$LL57
		 cmp	 di,MClus		;Cluster > total clusters?	
;		 $IF	 A			;Yep				
		 JNA $$IF57
$$LL57:
		    call    Bad_Cluster 	;Yes, go indicate bad stuff	
		    clc 			;Clear CY to force loop exit	
;		 $ELSE				;No, more clusters to go	
		 JMP SHORT $$EN57
$$IF57:
		    stc 			;Set CY to keep going		
;		 $ENDIF 			;				
$$EN57:
;	      $ENDIF				;
$$EN55:
;	   $ENDLOOP NC				;Exit if done with chain	
	   JC $$DO50
;	   $ENDSRCH				;End of chain chase loop       
$$SR50:
;	$ENDIF					;
$$EN46:
	pop	bx				;Restore registers		
	pop	si				;				
	ret					;				
MarkFAT endp					;				

;*****************************************************************************
;Routine name: Bad_Cluster
;*****************************************************************************
;
;description: IF first cluster =0, truncate file or XA to zero length.
;	      If bad cluster elsewhere, put in EOFVal.
;
;Called Procedures: Get_ThisElErr
;		    Eprint
;		       FixENT2
;
;Change History: Created	5/10/87 	MT
;
;Input: First_Cluster
;	   Chain_End
;	   XA_PASS = TRUE/FALSE
;	   DI = Cluster entry
;	   SI = dir block pointer
;	   First_Cluster = first cluster or extended XA
;	   Previous_Cluster = last good cluster number
;
;Output: ZeroTrunc = TRUE/FALSE
;
;Psuedocode
;----------
;
;	Setup filename for any messages (Call Get_ThisELErr)
;	IF cluster = First_Cluster
;	   IF XA_PASS = FALSE
;		 Zero out file length in DIR
;		 Setup message (CALL Get_ThisElErr)
;		 Display message (Call Eprint)
;	   ELSE (XA pass)
;	      Zero out XA pointer
;	      Setup message (CALL Get_ThisElErr)
;	      Display message (Call Eprint)
;	   ENDIF
;	   Write out corrected directory (CALL FixENT2)
;	ELSE (cluster other than first in chain)
;	   IF XA_Pass = TRUE
;	     Display Bad XA cluster message
;	   ELSE (!XA_Pass)
;	     Display bad file chain message
;	   ENDIF
;	   Move EOF into bad cluster (CALL PACK - Chain_End)
;	ENDIF
;	ret
;*****************************************************************************
Procedure Bad_Cluster				;				
	savereg <si,di,dx,si,di,bx>		;Preserve registers		
	mov	bx,si				; for the call			
	call	Get_ThisElErr			;Setup message			
	restorereg <bx,di,si>
	cmp	di,First_Cluster	;does 1st clus point to itself? 	;Need to change the directory	
;	$IF	E,OR			;yes					; pointer if the dir cluster or 
	JE $$LL64
	push	di			;if not, try this next test		; XA is bad, or the last good	
	mov	di,Previous_Cluster	;get prev cluster			; entry was the dir cluster or	
	cmp	di,First_Cluster	;does prev clus = 1st clus?		; XA cluster.			
	pop	di			;means the 1st cluster is bad
;	$IF	E			;yes	       ;			       
	JNE $$IF64
$$LL64:
	    cmp word ptr [si].dirclus,0 ;is cluster num already 0?		
;	    $IF NE			;no, its bad				
	    JE $$IF65
		mov	dx,offset DG:NulNZ	  ;1st cluster number is invalid
		call	EPrint			  ;Go print file and error	
		mov	word ptr [si].dirclus,0   ;set cluster number to 0	
		mov	zerotrunc,true	;modified the file size 		
;	    $ENDIF			;already set to 0, dont print err msg	
$$IF65:
	    mov     word ptr [si].DIRESIZ,0   ;set file size to 0	      ;Kill the file size	      
	    mov     word ptr [si].DIRESIZ+2,0				      ;Kill the file size	    
	    mov  bx,si				 ;Get pointer to directory	 
	    call    FixEnt2			 ;Write out updated directory	 
;	$ELSE					;Not first cluster in chain	
	JMP SHORT $$EN64
$$IF64:
	      mov     dx,offset dg:Badchain	;Tell user file and error	
	      call    EPrint							
	   mov	   dx,Chain_End 		;Terminate chain at bad spot	
	   mov	   si,Previous_Cluster		;Change the last good cluster	
	   call    Pack 			;Go fix it			
;	$ENDIF					;				
$$EN64:
	restorereg <dx,di,si>
	ret					;
Bad_Cluster endp				;				

;*****************************************************************************
;Routine name: Check_Chain_Sizes
;*****************************************************************************
;
;description: See if length of chain as listed in dir or XA matches up
;	      with the number of clusters allocated. Don't check if crosslink
;	      error, or chasing directory chain.
;
;Called Procedures: FixEnt
;		    Bad_Chain_Size
;
;
;Change History: Created	5/10/87 	MT
;
;Input: CSIZE = sectors per cluster
;	SSIZE = bytes per sector
;	Cluster_Count = number of clusters in chain
;	File_Size_Low/High = bytes dir or XA says is in chain
;	SI = Pointer to Dir entry
;
;Output: Cluster_Count = Size of chain in clusters
;	 SI = Pointer to dir entry
;	 BX = SRFCBPT
;
;Psuedocode
;----------
;
;	IF !Directory attribute,AND
;	IF !Crosslinked (ISCROSS = FALSE),AND
;	IF !Second pass (SecondPass = FALSE)
;	   Compute bytes/cluster
;	   Compute bytes/chain
;	   IF size > File_Size_High/Low
;	      Fix the size (CALL Bad_Chain_Size)
;	      ELSE
;		 Subtract file size from chain length
;		 IF Difference in Chain_Length and Size >= bytes/cluster
;		    Fix the size (CALL Bad_Chain_Size)
;		 ENDIF
;	      ENDIF
;	   ENDIF
;	ENDIF
;	CX = Cluster_Count  (kept for compatibility with old code)
;	BX = SRFCPT (kept for compatibility with old code)
;	ret
;*****************************************************************************
Procedure Check_Chain_Sizes			
	push	si				;				
	push	ax				;				
	test	[si].DirAtt,Dir_Attribute	;Is this a directory?		
;	$IF	Z,AND				;No				
	JNZ $$IF69
	cmp	IsCross,False			; and,is it crosslinked?	
;	$IF	E,AND				;No				
	JNE $$IF69
	cmp	SecondPass,False		;and, is this the first pass?	
;	$IF	E				;Yes,				
	JNE $$IF69
	   xor	   ax,ax			;AX =0				
	   mov	   ax,SSize			;Get (bytes/sector) *		
	   mov	   cl,CSize			; (Sectors/cluster)		
	   mul	   cx				;AX=Bytes/cluster  (< 64k)	
	   mov	   BClus,ax			;Save Bytes/cluster		 
	   mov	   cx,Cluster_Count		;Number of clusters in chain	
	   mul	   cx				;DX:AX = bytes/chain		
	   mov	   Chain_Size_Low,ax		;Save allocation size in bytes	
	   mov	   Chain_Size_High,dx		;				
	   cmp	   dx,File_Size_High		;See if file size if greater	
;	   $IF	   E,AND			; than chain length - if	
	   JNE $$IF70
	   cmp	   ax,File_Size_Low		; so, than there is an		
;	   $IF	   B				; allocation error.		
	   JNB $$IF70
	      call    Bad_Chain_Size		;Fix it!			
;	   $ELSE				;Chain larger than file 	
	   JMP SHORT $$EN70
$$IF70:
	      cmp     dx,File_Size_High 	;See if high part lower 	
;	      $IF     B 			;Chain < filsize if so		
	      JNB $$IF72
		 call	 Bad_Chain_Size 	;Fix it!			
;	      $ELSE				;Chain > filesize		
	      JMP SHORT $$EN72
$$IF72:
		 mov	 cx,File_Size_Low	;See if within 1 cluster	
		 mov	 bx,File_Size_High	;				
		 sub	 ax,cx			;Subtract file size from	
		 sbb	 dx,bx			; the chain size		
		 cmp	 dx,0			;See if within 1 cluster	
;		 $IF	 NE,OR			;Not if high size set,or	
		 JNE $$LL74
		 cmp	 ax,BClus		;Within (bytes/cluster -1)?	
;		 $IF	 AE			;Nope, allocation error 	
		 JNAE $$IF74
$$LL74:
		    call    Bad_Chain_Size	;Go fix the chain		
;		 $ENDIF 			;				
$$IF74:
;	      $ENDIF				;				
$$EN72:
;	   $ENDIF				;				
$$EN70:
;	$ENDIF					;				
$$IF69:
	mov	bx,SrFCBPt			; Needed for compat		
	pop	ax				;Restore used regs		
	pop	si				;SI = Dir pointer		
	ret					;				;     ;
check_chain_sizes endp				;				



Procedure print_filename			
	    PUSH    BX
	    MOV     BX,SI
	    CALL    get_THISEL
	    mov     dx,offset dg:noisy_arg
	    call    printf_crlf
	    MOV     SI,BX
	    POP     BX
	    return
print_filename	  endp				;				

;these procedures were for the extended attribute support, which was removed	

;*****************************************************************************
;Routine name: Bad_Chain_Size
;*****************************************************************************
;
;Description: adjust
;	      filesize to allocation length.
;
;Called Procedures: Truncate_XA
;		    FixEnt2
;		    Get_ThisElErr
;		    Eprint
;
;Change History: Created	5/11/87 	MT
;
;Input: XA_Pass = TRUE/FALSE
;	Chain_Size_High/Low
;	SI = Dir pointer
;	Chain_Size_Low/High = length in bytes of allocation chain
;
;Output: None
;
;Psuedocode
;----------
;
;	IF XA_Pass
;	   Delete XA chain (CALL Truncate_XA)
;	ELSE
;	   Set directory entry to length = Total allocation size
;	   Go write out (CALL FixEnt2)
;	   Setup message (CALL Get_Thiselerr)
;	   Display it (Call Eprint)
;	ENDIF
;	ret
;*****************************************************************************
Procedure Bad_Chain_Size			;				
	push	es
	push	ax				;Save register			

	push	ds		 ;make es point to dg
	pop	es
;;;;;; cmp     XA_Pass,True		       ;Are we handling XA's?          
;;;;;;;;$IF	E				;Yes				
;;;;;;;;;;;call    Truncate_XA			;Go truncate the chain		
;;;;;;;;$ELSE					;Normal file chain		
	   mov	   ax,Chain_Size_Low		;Get length of allocation	
	   mov	   dx,Chain_Size_High		; chain for filesize		
	   mov	   word ptr [si].DirESiz,ax	;Put it in the directory	
	   mov	   word ptr [si+2].DirESiz,dx	;   "  "       "  "		
	   push    bx				;				
	   push    si				;				
	   mov	   bx,si			;Get pointer to directory	
	   call    FixENT2			;Write dir to disk		
	   call    Get_ThisElErr		;Setup message			
	   mov	   dx,offset DG:BadClus 	;Specify error message		
	   call    EPrint			;Go print file and error	
	   pop	   si				;				
	   pop	   bx				;				
;;;;;;;;$ENDIF					;				
	pop	ax				;Restore registers		
	pop	es
	ret					;

Bad_Chain_Size endp				;				

;*****************************************************************************
;Routine name: Truncate_XA
;*****************************************************************************
;
;Description: If /F entered, than truncate XA chain and remove pointer.
;	      If XA allocation error, than deallocate all of XA chain.
;
;Called Procedures: Get_ThisElErr
;		    Eprint
;		    MarkMap
;		    Unpack
;		    Pack
;
;Change History: Created	5/11/87 	MT
;
;Input: First_Cluster
;	   Chain_End
;	   SI = directory entry pointer
;
;
;Output: FATMap entries for XA chain zero'd out
;
;Psuedocode
;----------
;
;	Set XA pointer in dir to 0
;	Write it out (CALL FixEnt2)
;	Setup message (Call get_ThisElErr
;	Display message (Call Eprint)
;	Get first cluster number (First_Cluster)
;	DO
;	   Get first cluster entry (Call Unpack)
;	   Go mark cluster in FATMap with "Open" (CALL MarkMAP)
;	   Set cluster entry with 0000 (Call Pack)
;	ENDDO cluster value >= EOFVal
;	ret
;*****************************************************************************

;rocedure Truncate_XA				;				
;
;	push	si			     ;Save dir pointer			
;	push	bx			     ;					
;	push	si			     ;					
;	mov	bx,si			     ;Get directory pointer		
;	call	Get_ThisEl		     ;Setup message			
;	mov	dx,offset DG:Alloc_XA_Msg    ;Specify error message		
;	call	EPrint			     ;Go print file and error		
;	pop	si			     ;					
;	mov	word ptr [si].DIR_XA,No_Ext_Attrib ;Erase XA pointer		
;	call	FixENT2 		     ;Write dir entry out		
;	pop	bx			     ;					
;	mov	si,First_Cluster	     ;Get first cluster 		
;	$DO				     ;Chase and erase XA chain		
;	   call    Unpack		     ;Get next cluster			
;	   push    di			     ;Save it- DI next, SI current	
;	   mov	   al,No_Entry		     ;Free entry in map 		
;	   call    MarkMap		     ; "  "    "  "			
;	   mov	   dx,No_Entry		     ;Free up cluster in Fat		
;	   call    Pack 		     ; "  "    "  "			
;	   pop	   si			     ;Get back next cluster		
;	   cmp	   si,[EOFVal]		     ;Reached end of chain?		
;	$ENDDO	   AE			     ;Keep looping if not		
;	pop	si			     ;Restore Dir pointer		
;	ret				     ;					
;
;runcate_XA endp			     ;					

;*****************************************************************************
;Routine name: Check_Extended_Attributes
;*****************************************************************************
;
;Description: Get the first cluster of XA chain, if it is zero, than erase
;	      extended attribute pointer (/F only). Otherwise, map the
;	      cluster in the FATMAP. If crosslink found on first cluster,
;	      no more processing is done. If value other than EOF mark
;	      found in first cluster
;
;Called Procedures: Load_XA
;		    Fix_Bad_XA
;		    Check_XA_Structure
;		    MarkFAT
;
;Change History: Created	5/10/87 	MT
;
;Input: SI = pointer to directory entry
;
;Output: FATMap marked with XA_Cluster for each XA cluster found
;	    XA_PASS = NO
;
;Psuedocode
;----------
;
;	IF (XA exists for file)
;	   XA_PASS = YES
;	   DI = XA entry cluster in dir
;	   Load in first sector of XA (CALL Load_XA)
;	   IF !error
;	      File_Size_Low/High = length of XA's in bytes
;	      AL = chain head mark (XA_Chain)
;	      Trace chain and map it (CALL MarkFAT)
;	   ELSE
;	      call Bad_Cluster
;	   ENDIF
;	ENDIF
;	ret
;*****************************************************************************

;rocedure Check_Extended_Attributes		;				
;
;	push	ax				;Save register			
;	push	cx				;				
;	push	dx				;				
;	push	di				;				
;	mov	ax,[si].DIR_XA			;Get first cluster of XA's      
;	cmp	ax,No_Ext_Attrib		;Are there extended attrib's    
;	$IF	NE				;Quit if no			
;	   mov	   di,ax			;Pointer to current cluster	
;	   mov	   First_Cluster,di		;Remember first cluster 	
;	   mov	   XA_Pass,Yes			;Indicate processing XA's       
;	   call    Load_XA			;Go load sector 		
;	   $IF	   NC				;CY means load error		
;	      mov     ax,XA_Buffer.XAL_TSIZE	;Get bytes in XA chain		
;	      mov     File_Size_High,0		;Save it			
;	      mov     File_Size_Low,ax		;				
;	      mov     al,XA_Chain		;Set up mark for map		
;	      call    MarkFAT			;Go map out chain		
;	   $ELSE				;Error on read of XA		
;	      call    Bad_Cluster		;Delete extended attribs	
;;	   $ENDIF				;				
;	$ENDIF
;	pop	di				;Restore registers
;	pop	dx				;				
;	pop	cx				;				
;	pop	ax				;				
;	ret					;				
;
;heck_Extended_Attributes endp			;				


;*****************************************************************************
;Routine name: Load_XA
;*****************************************************************************
;
;description: Read in the first XA cluster
;
;Called Procedures: Read_Disk
;
;
;Change History: Created	5/13/87 	MT
;
;Input: AX has start cluster of XA chain
;	SI = dir pointer
;Output: CY if read failed
;
;Psuedocode
;----------
;
;	Get start of data area
;	Get start cluster
;	Compute XA location from starting cluster
;	Read it in (CALL Read_Disk)
;	IF error
;	   stc
;	ENDIF  (NC if didn't take IF)
;	ret
;*****************************************************************************

;rocedure Load_XA				;				
;
;	push	si				;Save used registers		
;	push	cx				;				
;	push	dx				;				
;	push	bx				;				
;	sub	ax,2				;Make cluster 0 based		
;	mov	cl,CSize			;Get sectors/cluster		
;	mul	cl				;Offset sec in data area	
;	add	ax,Data_Start_Low		;Get actual sector in partition 
;	adc	dx,Data_Start_High		;   "  "    "  "		
;	mov	Read_Write_Relative.Start_Sector_High,dx ;Setup high sector addr
;	mov	bx,offset dg:XA_Buffer		;Read into buffer		
;	mov	cx,1				;Get just first sector		
;	mov	dx,ax				;Get logical sector low 	
;	mov	al,AllDrv			;Get drive number 1=A,2=B	
;	dec	al				;Make 0 based drive 0=A 	
;	call	Read_Disk			;Read in sector 		
;	$IF	C				;Problem?			
;	   stc					;				
;	$ENDIF					;				
;	pop	bx				;Restore registers		
;	pop	dx				;				
;	pop	cx				;				
;	pop	si				;				
;	ret					;				
;
;oad_XA endp					;				

;****************************************************************************
;WARNING!!! this must be the last label in the code section
;	    any changes to chkdsk.arf must take into account this area.
;	    it is used for reading things from disk into memory, such as dir
Public	CHKPRMT_End
Chkprmt_End label byte
;****************************************************************************
	pathlabl chkproc
CODE	ENDS
	END

