;==================================================================
; CHKDSK2.ASM
;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
;
; CHANGE HISTORY:
; M003   NSM    1/30/91         Always report correct FREE mem at the end of 
;                               chkdsk inspite of where chkdsk was loaded
;==================================================================

TITLE   CHKDSK - MS-DOS Disk consistancy checker
page    ,132                                    ;

	.xlist
	include chkseg.inc
	INCLUDE CHKCHNG.INC
	INCLUDE DOSSYM.INC
	INCLUDE CHKEQU.INC
	INCLUDE CHKMACRO.INC
	include pathmac.inc
	include version.inc    ; needs to see MSVER on pass 1
	.list

SUBTTL  Initialized Data
PAGE
CONST   SEGMENT PUBLIC PARA 'DATA'
	EXTRN  BADVER:byte,BADDRV_ARG:word,INVPATH_ARG:word
	EXTRN  FILE_ARG:word
	EXTRN  BADCD_ARG:word,BADSUBDIR:byte
	EXTRN  BADDRVM:byte
	EXTRN  BADIDBYT:byte
	EXTRN  OPNERR_ARG:word,NOEXT_ARG:word,EXTENT_ARG:word
	EXTRN  IDMES_ARG:WORD
	EXTRN  FILE_ARG1:WORD,FILE_ARG2:WORD
	EXTRN  badrw_num:word,BADRW_STR:WORD,BLOCK_NUM:WORD
	EXTRN  BADSW_ARG:WORD,DSKSPC:WORD
	EXTRN  HIDMES:WORD,DIRMES:WORD,FILEMES:WORD,ORPHMES2:WORD
	EXTRN  ORPHMES3:WORD,BADSPC:WORD,FRESPC:WORD
	EXTRN  TOTMEM:WORD,FREMEM:WORD,REPORT_ARG:WORD,CRLF_ARG:WORD
	EXTRN  RARG1:dWORD,RARG3:dWORD,ORPHCNT:dWORD                ;an049;bgb
	EXTRN  SubstErr:BYTE
	extrn  tot_bytes_lo:word, tot_bytes_hi:word

	extrn   SWITCHAR:byte,TCHAR:byte,HECODE:byte,CONBUF:byte
	extrn   DOTMES:byte,NOISY:byte,HAVFIX:byte
	extrn   DOFIX:byte,DIRBUF:near,PARSTR:byte
	extrn   NUL:byte,ERRSUB:word,ALLFILE:byte
	extrn   ORPHFCB:byte,ORPHEXT:byte,HIDCNT:dword
	extrn   HIDSIZ:word,FILCNT:dword,FILSIZ:word,DIRCNT:dword                ;an049;bgb
	extrn   DIRSIZ:word,CROSSCNT:dword,BADSIZ:word                       ;an049;bgb
	extrn   ORPHSIZ:word                                                ;an049;bgb
	extrn   LCLUS:word                                                  ;an049;bgb
	extrn   USERDIR:byte,FRAGMENT:byte
	extrn   ALLDRV:byte,FIXMFLG:byte,DIRCHAR:byte
	extrn   EOFVAL:word                                                  ;an050;bgb
	extrn   Idmes2:Byte,Idmes1:Byte,idmes3:byte                             ;an017;bgb
CONST   ENDS

SUBTTL  Un-initialized Data
PAGE
DATA    SEGMENT PUBLIC PARA 'DATA'
	extrn   THISDPB:dword,DOTSNOGOOD:byte,NUL_ARG:byte
	extrn   NAMBUF:byte,SRFCBPT:word
	extrn   ISCROSS:byte,MCLUS:word,CSIZE:byte,SSIZE:word
	extrn   DSIZE:word,ARG1:word,ARG_BUF:byte,ERRCNT:byte
	extrn   USERDEV:byte,HARDCH:dword,CONTCH:dword
	extrn   mem_size:word                                                   ;an055;bgb
	extrn   top_of_mem:word         ; M003
	extrn   max_free:word           ; M003
	extrn   psp_segment:word                                                ;an030;bgb
	extrn   write_fault:byte                                                ;an045;bgb
DATA    ENDS

CODE    SEGMENT PUBLIC PARA 'CODE'
ASSUME  CS:DG,DS:DG,ES:DG,SS:DG
	EXTRN   INT_23:NEAR
	EXTRN   PROMPTYN:NEAR,GET_CURRDIRERR:NEAR,GET_CURRDIR:NEAR
	EXTRN   FINDCHAIN:NEAR,CHECKERR:NEAR
	EXTRN   Write_Disk:Near, multiply_32_bits:near

	PUBLIC  PRINTF_CRLF,DOCRLF,SUBERRP,FCB_TO_ASCZ,EPRINT
	PUBLIC  DOINT26,DOTCOMBMES,REPORT
	public  ramcarv


;EPRINT:
;        CALL    CHECKERR
;        JNZ     RET14
;        cmp     byte ptr [nul_arg],0
;        jnz     hav_eprint_arg
;        mov     [file_arg2],offset dg:nul
;hav_eprint_arg:
;        mov     [file_arg1],dx
;       ;mov     dx,offset dg:file_arg
;        mov     dx,file_arg                     ;Get offset of message          ;AC000;
;        call    printf_crlf
;        mov     byte ptr [nul_arg],0
;RET14:  ret


EPrint:
	call    CheckErr                        ;See if we should display msg
	pathlabl chkdsk2
;       $IF     Z                               ;Yes if Z set                   ;AC000;
	JNZ $$IF1
	push    dx                              ;Save message                   ;AC000;
	Message File_Arg                        ;Put out file in question       ;AC000;
	pop     dx                              ;Get back message               ;AC000;
	call    Printf_CRLF                     ;Print it
	cmp     byte ptr [nul_arg],0            ;Is there a second message?
;       $IF     NZ                              ;Yes if not nul                 ;AC000;
	JZ $$IF2
	mov     dx,File_Arg2                    ;Display it                     ;AN000;
	call    Printf_CRLF                     ;                               ;AN000;
;       $ENDIF                                  ;                               ;AC000;
$$IF2:
	mov     byte ptr [nul_arg],0            ;Re-init this field
;       $ENDIF
$$IF1:
	ret                                     ;

DOTCOMBMES:
	CMP     [NOISY],0
	JZ      SUBERRP
	mov     [file_arg2],dx
	CALL    get_currdirERR
       ;MOV     DX,OFFSET DG:CENTRY             ;Centry got split into 3 msg's
       ;inc     byte ptr [nul_arg]              ;
	CALL    EPRINT
	RET

SUBERRP:
	MOV     AL,1                    ;found a subdir error
	XCHG    AL,BYTE PTR [ERRSUB]    ;set error flag and get old flag
	CMP     AL,0                    ;were any errors found before?
;       $if     z                       ;no errors found yet
	JNZ $$IF5
					 ;JNZ     RET32
	    MOV     SI,OFFSET DG:NUL       ;display error msgs
	    CALL    get_currdirERR
	    MOV     DX,OFFSET DG:BADSUBdir
	    CALL    EPRINT
;       $endif
$$IF5:
RET32:  RET

;****************************************************************************
; called by:  get_thisel2
; inputs:   DS:SI - pointer to file name
;****************************************************************************
FCB_TO_ASCZ:                            ;Convert DS:SI to ASCIZ ES:DI
	PUSH    CX
;move filename from ds:si to es:di
	MOV     CX,8                    ; Pack the name
	REP     MOVSB                   ; Move all of it
main_kill_tail:
; delete trailing spaces in name
	CMP     BYTE PTR ES:[DI-1]," "  ;was the last char in name a space?
	JNZ     find_check_dot
	DEC     DI                      ; Back up over trailing space
	INC     CX
	CMP     CX,8
	JB      main_kill_tail
find_check_dot:
; ???
	CMP     WORD PTR [SI],(" " SHL 8) OR " "
	JNZ     got_ext                 ; Some chars in extension
	CMP     BYTE PTR [SI+2]," "
	JZ      find_done               ; No extension
got_ext:
; move period for extension
	MOV     AL,"."
	STOSB
; move 3 byte extension
	MOV     CX,3
	REP     MOVSB
ext_kill_tail:
;delete trailing blanks
	CMP     BYTE PTR ES:[DI-1]," "  ;
	JNZ     find_done               ;
	DEC     DI                      ; Back up over trailing space
	JMP     ext_kill_tail           ;
find_done:                              ;
; put hex zero at the end
	XOR     AL,AL                   ;
	STOSB                           ; NUL terminate
	POP     CX
	RET


DOINT26:
;       PUSH    CX                      ;reg saves are handled in write_disk    ;ac048;bgb;an045;bgb
;       PUSH    DX                      ;reg saves are handled in write_disk    ;ac048;bgb;an045;bgb
;       PUSH    BX                      ;reg saves are handled in write_disk    ;ac048;bgb;an045;bgb
	call    Write_Disk              ;                                       ;an045;bgb;AC000;
;       POP     BX                      ;reg saves are handled in write_disk    ;ac048;bgb;an045;bgb
;       POP     DX                      ;reg saves are handled in write_disk    ;ac048;bgb;an045;bgb
;       POP     CX                      ;reg saves are handled in write_disk    ;ac048;bgb;an045;bgb
;       JNC     RET23                                                           ;ac048;bgb;an045;bgb
       ;MOV     SI,OFFSET DG:WRITING                                            ;ac048;bgb;an045;bgb
       ;CALL    DSKERR                                                          ;ac048;bgb;an045;bgb
;      $IF      C                                                               ;ac048;bgb;an045;bgb
       JNC $$IF7
	   mov     dx,offset dg:write_fault                                     ;ac048;bgb;an045;bgb
	   invoke  printf_crlf                                                  ;ac048;bgb;an045;bgb
;      $ENDIF                                                                   ;ac048;bgb;an045;bgb
$$IF7:
;                                                                               ;ac048;bgb;ac048;bgb;an045;bgb
;Need to handle 'Fail' option of critical error here.                           ;ac048;bgb;an045;bgb
;                                                                               ;ac048;bgb;an045;bgb
										;ac048;bgb;an045;bgb
;       JZ      DOINT26                                                         ;ac048;bgb;an045;bgb
RET23:  RET                                                                     ;ac048;bgb;an045;bgb



;**************************************
; Prints all reporting data
;**************************************

REPORT:
;total disk space
    mov     bx,offset dg:dskspc
    mov     dx,tot_bytes_hi             ;total bytes   in disk          ;AN006;bgb
    mov     ax,tot_bytes_lo             ;total bytes   in disk          ;AN006;bgb
    xor     si,si                       ;no file count
    xor     di,di                       ;no file count
    call    Report_Mes_2                ;                               ;AN006;bgb
;hidden files
    mov     ax,hidsiz                   ;get cluster count              ;an049;bgb
    or      ax,ax                       ;are there any hidden files?
;   $IF     NZ                          ;yes                                   ;AC000;
    JZ $$IF9
	mov     si,word ptr hidcnt      ;si=low file count                  ;an049;bgb
	mov     di,word ptr hidcnt+2    ;di=hi file count                   ;an049;bgb
	mov     bx,offset dg:hidmes     ;bx=msg ;
	call    report_mes_1                    ;
;   $ENDIF                                  ;                               ;AC000;
$$IF9:
;space in subdirectories
    mov     ax,dirsiz                   ;get cluster count
    or      ax,ax                       ;Are there any directories?    ;an049;bgb
;   $IF     NZ                          ;yes                                   ;AC000;
    JZ $$IF11
	mov     si,word ptr dircnt      ;si=low file count                  ;an049;bgb
	mov     di,word ptr dircnt+2    ;di=hi file count                   ;an049;bgb
	mov     bx,offset dg:dirmes     ;bx=msg
	call    report_mes_1                                                ;an049;bgb
;   $ENDIF                                  ;                               ;AC000;
$$IF11:
;user files
    mov     ax,filsiz                   ;get cluster count
    or      ax,ax                       ;Are there any user files?    ;an049;bgb
;   $IF     NZ                          ;yes                                   ;AC000;
    JZ $$IF13
	mov     si,word ptr filcnt      ;si=lo file count                   ;an049;bgb
	mov     di,word ptr filcnt+2    ;di=hi file count                   ;an049;bgb
	mov     bx,offset dg:filemes    ;bx=msg
	call    report_mes_1
;   $ENDIF                                  ;                               ;AC000;
$$IF13:
;chains of lost clusters
    mov     ax,orphsiz                  ;get cluster count
    or      ax,ax                       ;Are there any lost clusters?   ;an049;bgb
;   $IF     NZ                          ;yes                                   ;AC000;
    JZ $$IF15
	mov     si,word ptr orphcnt     ;si=lo file count
	mov     di,word ptr orphcnt+2   ;di=hi file count
	cmp     dofix,0                 ;/F entered?
;       $IF     Z                       ;no                             ;AC000;
	JNZ $$IF16
	    mov     bx,offset dg:orphmes3 ;bytes would be recovered
;       $else
	JMP SHORT $$EN16
$$IF16:
	    mov     bx,offset dg:orphmes2 ;bytes were   recovered
;       $ENDIF                                  ;                               ;AC000;
$$EN16:
	call    report_mes_1
;   $ENDIF                                  ;                               ;AC000;
$$IF15:
;clusters of bad spots
    mov     ax,badsiz                   ;get cluster count
    or      ax,ax                       ;Are there any bad spots on disk?
;   $IF     NZ                          ;if low word > zero, then yes          ;AC000;
    JZ $$IF20
	xor     si,si                   ;no files to count
	xor     di,di                   ;no files to count
	mov     bx,offset dg:badspc             ;Issue report
	call    report_mes_1
;   $ENDIF                                  ;                               ;AC000;
$$IF20:
;bytes on disk left - free space

;;;BEGIN CHANGES FOR DOS 6 oct 22 Scottq
;;;
;;;instead of using the dos dpb value for total number of clusters,
;;;we take the total space (retrieved via int 21h function 36)
;;;and divide by cluster size.  If someone has hooked the size function,
;;;(dblspace) to return a different total space from the dpb, then
;;;the free space returned will still be correct.

	;;; get bytes per cluster into cx...
	xor     ah,ah                           ;                               
	mov     cx,SSize                        ;Bytes/sector *                 
	mov     al,CSize                        ; Sectors/cluster               
	mul     cx                              ; = Bytes/Cluster in AX         
	mov     cx,ax
	;;; get total bytes into dx:ax
	mov     dx,tot_bytes_hi             ;total bytes   in disk          
	mov     ax,tot_bytes_lo             ;total bytes   in disk          
	;;; get total clusters into ax 
	div     cx

	; replaced with above
	;;;mov     ax,[dsize]              ;get total disk clusters
;;;END CHANGES FOR DOS 6 oct 22 Scottq

	sub     ax,[dirsiz]             ; - dirs
	sub     ax,[filsiz]             ; - files
	sub     ax,[hidsiz]             ; - hidden files
	sub     ax,[badsiz]             ; - bad spots
	sub     ax,[orphsiz]            ; - lost clusters recovered
	sub     ax,[lclus]              ; - lost clusters not recovered
	xor     si,si
	xor     di,di
	mov     bx,offset dg:frespc
	call    report_mes_1            ;Free space is whats left
	call    docrlf                          ;                               ;AN000;
;size of each allocation unit
	xor     dx,dx                           ;Figure out cluster size        ;AN000;
	xor     ah,ah                           ;                               ;AN000;
	mov     cx,SSize                        ;Bytes/sector *                 ;AN000;
	mov     al,CSize                        ; Sectors/cluster               ;AN000;
	mul     cx                              ; = Bytes/Cluster in AX         ;AN000;
	mov     bx,offset dg:idmes2             ;Allocation size message        ;AN000;
	xor     si,si
	xor     di,di
	call    Report_Mes_2                    ;                               ;AN000;
;total clusters
	mov     ax,Mclus                        ;Allocation units available     ;AN000;
	dec     ax                              ;MCLUS is # clusters+1
	xor     dx,dx                           ;                               ;AN000;
	mov     bx,offset dg:idmes1             ;                               ;AN000;
	xor     si,si
	xor     di,di
	call    Report_Mes_2                    ;                               ;AN000;
;;;;;;;;call    docrlf                  ;                                       ;an017;bgb
;avail clusters                                                                 ;an017;bgb
public avail_clus
avail_clus:
	mov     ax,[dsize]              ;total clusters on disk                 ;an017;bgb
	sub     ax,[dirsiz]             ; - clusters in subdirs                 ;an017;bgb
	sub     ax,[filsiz]             ; - user files                          ;an017;bgb
	sub     ax,[hidsiz]             ; - hidden files                        ;an017;bgb
	sub     ax,[badsiz]             ; - bad spots                           ;an017;bgb
	sub     ax,[orphsiz]            ; - lost clusters recovered                                        ;an017;bgb
	sub     ax,[lclus]              ; - lost clusters not recovered         ;an017;bgb
	mov     bx,offset dg:idmes3             ;                               ;an017;bgb
	xor     dx,dx                           ;                               ;AN017;bgb
	xor     si,si
	xor     di,di
	call    Report_Mes_2            ;dont convert to bytes!                 ;an017;bgb
	call    docrlf                                                          ;an017;bgb

; get the size of conv mem  by calling INT 12

	int     12h                     ; query for total mem in system
	mov     dx,64                   ; convert to total no of paras
	mul     dx                      ; dx:ax =size of mem IN PARAS
	mov     [mem_size],ax           ;

;dcl Jan 8, 87 Compensate for RAM Carving - Start
;/* M001 BEGIN */
; Add size of XBDA only if it is located contig. to conv.mem reported by
; INT 12; else it is probably moved by EMM386 to somewhere within conv. mem
; and so it is already counted by INT 12 figure. 

ramcarv:
if      MSVER                           ; MSKK03 07/14/89
	xor     bx,bx
else
	push    es
	xor     bx,bx   
	mov     ah,0c1h                 ; return Ext'd Bios Data Seg Address    
	int     15h     
;       $IF     NC                      ; ram carving exists if no carry
	JC $$IF22
	   mov     ax,es                ; see whether XBDA is just at the end
	   cmp     ax,[mem_size]        ; of conv mem. reported by int 12
	   jne  $$IF22                  ; if not at end, don't add size of XBDA
	   xor     ax,ax                ; zero out ax
	   mov     al,byte ptr es:[0]   ; pointer to # of 1k blocks of RAM Carve
	   mov     dx,64                ; convert 1k blocks to paras    
	   mul     dx                                           
	   mov     bx,ax                   ; save value in BX
;       $ENDIF                                  ;       
$$IF22:
	pop     es                              
endif
;/* M001 END */

;dcl Jan 8, 87 Compensate for RAM Carving - End

	mov     ax,[mem_size]        ;Find out about memory
	add     ax,bx                   ; dcl Jan 8, 87 Compensate for RAM Carving
	mov     dx,16                   ;Mul to convert kbytes to bytes
	mul     dx
	mov     bx,offset dg:totmem
	call    report_mes_2

; /* M003 BEGIN */
; report free space
; see where we are loaded ; if we are loaded in conv. memory, then take the
; top_of_mem (from ds:2) and report (top_of_mem -our psp) as the free size.
; if we are loaded in UMBs,  report the max_free value as the FREE mem value
	mov     ax,[max_free]
	or      ax,ax                   ; are we loaded in UMB ?
	jnz     rep_free_mem            ; yes, and so we have the right figure
					; in ax

; we are loaded in conv. mem.
; calculate the free size as (top_of_mem - our psp_seg ptr + 1) 

	mov     ax,[top_of_mem]         
	sub     ax,psp_segment          
	push	ax			; Put amount free on TOS

;
; if our environment space is right above our PSP (just lower), then the
; actual largest-executable-size needs to encompass that space as well.  So
; check our environment pointer to see if CHKDSK's PSP is the next block--if
; so, add out environment space to max_free
;

	push	ds			; Store DS before we go change it...

	mov     ax,psp_segment          ; Now grab our PSP...
	push	ax			;    put it on the stack above AmtFree
	mov	ds,ax			; And stick it in DS for reference

	mov	dx,ds:[2ch]		; DX = pointer to our environment space
	dec	dx			; Back up one, to its MCB

	mov	ds,dx			; Now we want to work with what's here:
	mov	bx,ds:[3]		; BX = size of environment
	inc	bx			;      (add 1 paragraph for the header)
	mov	ax,bx			; AX = size of environment too
	add	ax,dx			; DX = location of environment,-1
	inc	ax			; +1 for MCB of next block

;
; At this point, AX is the address of CHKDSK's PSP if it is loaded in the
; adjacent MCB, and CHKDSK's actual PSP is at the top of the stack.  BX is
; the size of the environment, and will be added to the amount free--so if
; CHKDSK isn't in the next MCB, we have to zero it.
;

	pop	dx			; DX = CHKDSK's PSP, AX = calculated
	cmp	ax, dx			; Are they the same?
	jz	AddEnvSpace		; Yes--leave BX == size of environment

	xor	bx, bx			; No?  Then clear BX.

AddEnvSpace:
	pop	ds			; Restore DS as it was earlier...

	pop	ax			; Restore the amount free
	add	ax, bx			; Account for size of environment

; ax = free mem size in conv. mem

rep_free_mem:
; /* M003 END */
	mov     dx,16
	mul     dx
	mov     bx,offset dg:fremem
	call    report_mes_2
	ret

;*************************************************************
;
;  Print the message specified by the control string.
;
; REPORT_MES_1
; On entry:
;     BX contains the address of the control string.
;     AX contains a cluster count for the %ld argument in the control string.
;     CX contains a word count for the %d argument in the control string
;        or is meaningless.
;
; REPORT_MES_2
; On entry:
;     BX contains the address of the control string.
;     AX,DX contain a long integer.
;     CX contains a word count for the %d argument in the control string
;        or is meaningless.
;
;*************************************************************

;*****************************************************************************
; REPORT_MES_1 -  Print the report messages.  Display the file count and
;                translate the number of clusters into the number of bytes.
;
; WARNING!! NOTE!! -->
;
; called by - PROCEDURE NAME
;
; inputs: AX - cluster count (1-ffff)
;         BX - offset of control string
;         CX -
;         DX - high word of cluster count is zero'd out here.
;         SP -
;         BP -
;         SI - low word of file count
;         DI - hi  word of file count
;         DS - segment of control string
;         ES -
;
; output: DISPLAY OF DATA TO SCREEN
;
; Regs abused - ALL
;
;logic: 1. zero out hi word of cluster count, and multiply by sectors per
;          cluster.  This gives number of sectors, which is a double word.
;
;       2. multiply by bytes per sector to give number of bytes.
;
;       3. place values in diplay fields, and call the msg. retriever.
;
;*****************************************************************************
report_mes_1:
	push    bx                      ;save it
	xor     dx,dx                   ;zero out hi word for multiply
	mov     cl,csize                ;get sectors per cluster
	xor     ch,ch                   ;zero out hi byte of word
	mul     cx                      ;Convert cluster count to sector count ax/dx

	mov     bx,dx                   ;bx:ax is number to be mult;bgb
	mov     cx,ssize                ;cx is number to mult with  ;bgb
	call    multiply_32_bits                       ;bgb
	mov     dx,bx                   ;move hi value to dx
	pop     bx                      ;retore pointer;bgb

report_mes_2:
	mov     word ptr rarg1,ax       ;Lo word of bytes in ax
	mov     word ptr rarg1+2,dx     ;Hi word of bytes in dx
	mov     word ptr rarg3,si       ;lo word of file count in si
	mov     word ptr rarg3+2,di     ;hi word of file count in di
	mov     report_arg,bx           ;Store the offset of the ctrl string
	mov     dx,bx                   ;dx has ptr to msg for disp_interface  ;AC000;
	call    printf_crlf             ;print msg, then carraige return
	ret

PRINTF_CRLF:
	call    display_interface   ;                                       ;AC000;
DOCRLF: mov     dx,offset dg:crlf_arg
	call    Display_Interface       ;Replace old printf call with SysDispMsg;AN000;
	ret                             ;                                       ;AN000;


	pathlabl chkdsk2
CODE    ENDS
	END
