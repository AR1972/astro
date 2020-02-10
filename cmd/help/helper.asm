        page    ,132
        title   Helper routines for HELP

; get screen height - return current height of display.  Use ANSI to
; report height if it is present, otherwise reach into ROM BIOS data
; area.  Code taken from MORE.COM
;
; make upper - convert character to upper case using the DOS uppercase
; table.  Code taken from FIND.

        .xlist
        include rombios.inc
        include version.inc
        .list

; miscellaneous equates for detecting screen size

ANSI_GET	EQU    440CH					     
GET_SUBFUNC	EQU    037FH					     
LINESPERPAGE	EQU    24		; default screen size
TEXT_MODE	EQU    1				       
DATA_LENGTH	EQU    14				       
STDERR          EQU    2                ; Standard error handle

ifdef JAPAN
LINELENGTH	EQU	80		; default line length
endif

;								     
ANSI_STR    STRUC						     
	    DB	  0						     
	    DB	  0						     
	    DW	  DATA_LENGTH					     
	    DW	  ?						     
D_MODE	    DB	  ?						     
	    DB	  ?						     
	    DW	  ?						     
	    DW	  ?						     
	    DW	  ?						     
SCR_COLS    DW	  ?						     
SCR_ROWS    DW	  ?						     
ANSI_STR    ENDS						     
        

        .MODEL SMALL
        .DATA
MAXROW	DB	25
MAXCOL	DB	80

ANSI_BUF ANSI_STR <>			;buffer for IOCTL call


        .CODE

; get height of screen from device driver or ROM BIOS
; returns height in rows in AL, width in columns in AH

        public  _get_screen_height
_get_screen_height      proc    near

        push    bp                      ; standard prelude
        mov     bp,sp
        push    si
        push    di

ifdef JAPAN

	mov	MAXROW,LINESPERPAGE 	; assume 24 rows
	mov	MAXCOL,LINELENGTH	; assume 80 columns

else

	push	ds
	mov	ax,ROMBIOS_DATA 	  ;Get ROM Data segment
	mov	ds,ax
	Assume	DS:ROMBIOS_DATA

	mov	al,CRT_Rows		  ;Get max rows
	pop	ds
	Assume	DS:Dgroup

	or	al,al			  ;If zero specified
	jnz	@F
	mov	al,LINESPERPAGE 	  ;assume 24 rows

@@:
	mov	maxrow,al		  ;set lines per page from ROM BIOS
endif

;
; Check if ANSI is loaded, and if so, use it to find screen dimensions
;

	MOV	AX,ANSI_GET		  ;prepare for device characteristics..
	MOV	BX,STDERR		  ;request.
	MOV	CX,GET_SUBFUNC		  ;get subfunction..
	LEA	DX,ANSI_BUF		  ;point to buffer.
	INT	21H
	jc	no_ansi

;
; ANSI is around and get tell us the screen dimensions
;

	LEA	DI,ANSI_BUF
	cmp	[di].d_mode, TEXT_MODE	  ;if we are in a text mode then..
	jne	no_ansi 		  ;default already initialized
	MOV	AX,[DI].SCR_ROWS	  ;store the screen length...else..
	MOV	MAXROW,AL		  ;default (25) is assumed.

ifdef JAPAN
	mov	ax,[di].SCR_COLS
        mov     ah,al                     ; return cols in AH                           
no_ansi:

else

no_ansi:
	MOV	AH,0FH
	INT	10H                       ; returns cols in AH
        
endif
        mov     al,maxrow                 ; get the max rows      
        
        pop     di
        pop     si
        pop     bp
        ret

_get_screen_height      endp


; routines to get DOS upper case table, and to map chars to upper
; case in a language independent way.

        .DATA

GET_UPPER_TABLE equ     6502h           ; DOS get upper case table call    
ucasetab_bias   dw      0               ; value of (256-table size)
uppercase_table db      5 dup (?)       ; buffer for upper case table
                                        ; used of DOS Function 65h,
                                        ; and to store real table address


        .CODE


; Get pointer to upper case table and store as local data
; Must call this before calling _make_upper below

        public  _get_ucase_tab
_get_ucase_tab  proc    near
        push    bp                      ; Standard prologue
        mov     bp,sp
        push    si
        push    di

        push    es
        mov     ax,GET_UPPER_TABLE
        mov     bx,-1                   ; current code page
        mov     cx,5                    ; size of info for upper case table
        mov     dx,bx                   ; current country id
        push    ds
        pop     es

        mov     di,offset uppercase_table ; point at buffer for table
        int     21h
                                        ; assume no error
        inc     di                      ; point at start of table
        les     di,[di]                 ; load table address

        assume  es:nothing

        mov     bx,es:[di]              ; load table size
        mov     ax,256                  ; compute table size bias
        sub     ax,bx
        mov     ucasetab_bias,ax        ; save it for later
        add     di,2                    ; point at translation table
        mov     word ptr uppercase_table,di   ; save tranlation table address
        mov     word ptr uppercase_table+2,es
        pop     es                                        
        pop     di
        pop     si
        pop     bp
        ret

_get_ucase_tab  endp


; convert character to upper case using DOS uppercase table.
; NOTE: program must have called _get_ucase_tab before calling this.
;
;       Entry: char in AL
;       Exit : modified char in AL

        public  _make_upper
_make_upper     proc    near

        cmp     al,80h                  ; if AL is extended char, get mapping
        jb      not_al_high             ; AL not extended char
        jmp     short use_table         ; get mapping

not_al_high:
        cmp     al,'a'                  ; make sure it is lower case
        jb      not_al_lower            ; jump if no remapping needed
        cmp     al,'z'
        ja      not_al_lower
        and     al,0DFh                 ; remap normal character

not_al_lower:
        ret

use_table:                              ; do lookup in DOS ucase table
        push    bx
        push    di
        push    es

        les     di,dword ptr uppercase_table      ; get the table

        assume  es:nothing

        mov     bl,al
        xor     bh,bh
        sub     bx,ucasetab_bias        ; get the index
        mov     al,es:[di+bx]           ; get the char

        pop     es
        pop     di
        pop     bx
        ret

_make_upper     endp


        end
