;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

; ZZZZZZ Are there bugs with offset calculations?? Is DGROUP the right
; assume for DS? I am unable to access variable DoQuickCompare from 'C'
; The offset is off by 64 bytes!!

?WIN = 0                ;Not windows;
?PLM = 1                ;DO use pl/m
include cmacros.inc

USA_CODE        EQU             001

sBegin data
      EXTRN gDescendingOrder: WORD

       CollStruc struc
			CollInfoId      db  ?
			CollInfoPtr     dd  ?
       CollStruc ends

       CountryBuffer struc
			Date_Format     db  2 dup(?)
			Currency_Symbol db  5 dup(?)
			Thousands_Sep   db  2 dup(?)
			Decimal_Sep     db  2 dup(?)
			Date_Sep        db  2 dup(?)
			Time_Sep        db  2 dup(?)
			Currency_Pos    db  ?
			Num_Decimals    db  ?
			Time_Format     db  ?
			Case_Mapping    dd  ?
			Data_Sep        db  2 dup(2)
			Reserved        db 10 dup(?)
			;;;;size of structure must be at least 34 bytes!!
       CountryBuffer ends

       CollInfo         CollStruc <>
       EXTRN NationData:  BYTE

	   DoQuickCompare       db       0 ; Will be set to 1, if USA or
					   ; no collating table avail.
	   PUBLIC       DoQuickCompare
sEnd data

sBegin code

    assumes cs, code
    assumes ds, DGROUP

;;;; This function gets the International data into global NationData
;;;; Effects:
;;;;    Fills in NationData global
;;;;   DoQuickCompare set to 1, if country is USA, else unchanged.
;
; BOOL GetInternationalData(void)
;
cProc  GetInternationalData, PUBLIC,    <si,di,ds,es>
cBegin  GetInternationalData
	lea     dx, NationData          ; DS:DX points to NationBuffer
	mov ax,3800h                    ; Get Current Country info
	int 21h                         ; If success, Carry clear and BX = country code

	mov ax,0                        ;
	cmc                             ;
	rcl ax,1                        ; value return to C-caller (1 on success, else 0)

	;; Now set the DoQuickCompare field
	cmp     bx, USA_CODE            ; Is current country USA?
	jne     GIDNotQuick

GIDQuick:
	mov     DoQuickCompare, 1       ; QuickCompare is possible

	;;; DoQuickCompare remains 0 if country is not USA.

GIDNotQuick:
cEnd  GetInternationalData

;;;; This function sets up the character collating table for sorting
;;;; Effects:
;;;;   CollInfoId set to 6 if collating table is found, else 0.
;;;;   CollInfoPtr points to collating table found.
;;;;   DoQuickCompare set to 1, if no collating table avail, else unchanged.
;
; void SetCollatingTable(void)
;
cProc  SetCollatingTable, PUBLIC,  <si,di,ds,es>
cBegin  SetCollatingTable
	assumes ds, DGROUP

	lea     di, CollInfo    
	push    ds                              
	pop     es                      ; ES:DI points to CollInfo buffer

	mov     ax, 6506h               ; DOS function get extended country Info
					; subfunc get pointer to collating sequence table
	
	mov     bx, -1                  ; BX = code page of interest (active CON = -1)
	mov     cx, 5                   ; CX = length of buffer to receive info
	mov     dx, bx                  ; DX = country ID (default = -1)

	int     21h
	jnc     SCTRet                  ; Success, CollInfoId = 6 is set by DOS.

;       The call failed. Set CollInfoInd = 0 to signal no collating table.

	mov     CollInfo.CollInfoId, 0
	mov     DoQuickCompare, 1       ; QuickCompare can be done as no coll table avail

SCTRet:
cEnd  SetCollatingTable

; Invoked from a c-routine. Returns TRUE, if we can use a non-collating compare
; That is whether a straight ASCII compare can be done.
cProc  FDoQuickCompare, PUBLIC, <si,di,ds,es>
cBegin  FDoQuickCompare
	assumes ds, DGROUP
	xor     ah, ah
	mov     al, DoQuickCompare
cEnd  FDoQuickCompare


;;;; This function compares two strings using the collating table if avail.
;;;; ENTRY      DS:SI = ptr to string1
;;;;            ES:DI = ptr to string2
;;;;            CX = length of string to be compared
;;;;
;;;; EXIT       Depending on >, <, ==, the processor flags are set.
;;;;            direction flag cleared.
;;;;            destroys AX, BX, CX, DX, DI, SI
str_cmp proc
	cld                             ; for lodsb, repe instructions to inc addresses

	push    bp                      ; preserve BP
	mov             bp,     ds      ; save string1's seg as we need DS for collating stuff
	mov             ax, DGROUP
	mov             ds, ax
	assumes ds,DGROUP

	lds     bx, CollInfo.CollInfoPtr  ; DS:BX = ptr to collating table
	assumes ds,nothing

	mov             dx, ds:[bx]     ; DX = collating table length
	add             bx, 2           ; DS:BX = ptr to collating values

	dec             dx              ; dl holds length of table - 1
SC_nextchar:
	mov             al, es:[di]     ; AL = AX = char from string2
	inc             di                      ; ES:DI = ptr to next char in string2

;;; BUGBUG -- maybe I should avoid these collating length compares in the loop?
	cmp             al,dl
	ja              @F              ; in the table?
SC_xlat1:
	xlat
@@:
	mov             ah, al          ; store char in ah

	push            ds              ; save DS
	mov             ds, bp          ; DS = seg address of string1
	lodsb                           ; AL = DS:[SI++] = char from string1
	pop             ds                      ; restore DS = seg of collating table

	cmp             al, dl          ; compare char to length of collating table
	ja              @F
SC_xlat2:
	xlat
@@:
	cmp             al, ah          ; compare char from string1 with that from string2

ifdef DBCS
	loope   @f
	jmp     short SC_not_same       ; if not same
@@:
	mov     al,es:[di-1]
	call    IsDBCSLeadByte
	jnz     SC_nextchar             ; if this is not lead byte
	mov     ah,es:[di]              ; get tail byte
	inc     di
	push    ds
	mov     ds,bp
	lodsb
	pop     ds
	cmp     al,ah                   ; compare tail byte
	loope   SC_nextchar     ; loop until unequal or no more left
SC_not_same:
else

	loope   SC_nextchar     ; loop until unequal or no more left
endif

	mov             ds, bp          ; DS = value on entry to this routine

	pop             bp                      ; restore BP
	ret
str_cmp endp

;;;; This function compares the names of files for sorting using collating table
;;;; The extension is always at offset 8.
;;;; The short names/extensions are assumed to be padded by NULL/same chars.
;;;; If the names are the same, compare extensions also.
;;;; Returns -1 if one < two
;;;;          0 if equal
;;;;          1 if one > two
;
; int pascal name_cmp(char far* string_one,char far* string_two)
;
cProc  name_cmp, PUBLIC,  <si,di,ds,es>
parmW string_one_segment
parmW string_one_offset
parmW string_two_segment
parmW string_two_offset
cBegin  name_cmp
	
    mov di,string_two_offset
    mov es,string_two_segment

    mov si,string_one_offset
    mov ds,string_one_segment
    assumes ds,nothing

    mov cx,8
    call str_cmp                        ; sets the >, <, == flags

    ja short nc_1_is_bigger

    
    jne short nc_1_is_lesser; if we get here, one is <= two

    ;;; here we compare the extensions as names match
    mov di,string_two_offset
    add di,8                            ; add 8 to get to extension

    mov si,string_one_offset
    add si,8                            ; add 8 to get to extension

    mov cx,3
    call str_cmp                        ; sets the >, <, == flags

    mov ax, 0                           ; set ret status to mean equal strings

    ja short nc_1_is_bigger
    je short NameC_end

nc_1_is_lesser:
    mov ax,-1
    jmp short NameC_Toggle

nc_1_is_bigger:
    mov ax,1

NameC_Toggle:

    ; Based on the sort order exchange the values -1 and 1. Note that the
    ; flag "gDescendingOrder" will have a value of FFFE or 0!

    mov bx, DGROUP
    mov ds, bx
    assumes ds, DGROUP

    xor ax, gDescendingOrder

NameC_end:
cEnd  name_cmp


;;;; This function compares the names of files for sorting, with bias
;;;; on the extension name using collating table!!
;;;; The extension is always at offset 8.
;;;; Short names/extensions are assumed to be padded by NULL/same chars.
;;;; If the extensions are the same, compare names also.
;;;; The name is either null terminated or 8 characters long and begins
;;;; at offset 0.
;;;; Returns -1 if one < two
;;;;          0 if equal
;;;;          1 if one > two

;
; int pascal ext_cmp(char far* string_one,char far* string_two)
;
cProc  ext_cmp,  PUBLIC,  <si,di,ds,es>
parmW string_one_segment
parmW string_one_offset
parmW string_two_segment
parmW string_two_offset
cBegin  ext_cmp
	
	
    mov di,string_two_offset
    mov es,string_two_segment

    add di,8                            ; add 8 to get to extension

    mov si,string_one_offset
    mov ds,string_one_segment
    assumes ds,nothing
    
    add si,8                            ; add 8 to get to extension

    mov cx,3
    call str_cmp                        ; sets the >, <, == flags

    ;;; ax is zero now
    ja short ec_1_is_bigger
    jne short ec_1_is_lesser

    ;;; extensions are the same -- so compare names.
    mov di,string_two_offset
    
    mov si,string_one_offset

    mov cx,8
    call str_cmp                        ; sets the >, <, == flags

    mov ax, 0                           ; set ret status to mean equal strings

    ja short ec_1_is_bigger

    ;;; if we get here, one is <= two
    je short ExtC_end

ec_1_is_lesser:
    mov ax,-1
    jmp short ExtC_Toggle

ec_1_is_bigger:
    mov ax,1

ExtC_Toggle:

    ; Based on the sort order exchange the values -1 and 1. Note that the
    ; flag "gDescendingOrder" will have a value of FFFE or 0!

    mov bx, DGROUP
    mov ds, bx
    assumes ds, DGROUP

    xor ax, gDescendingOrder

ExtC_end:
cEnd  ext_cmp

;;;; function similar to name_cmp but with direct ASCII values!!
cProc  quick_name_cmp, PUBLIC,  <si,di,ds,es>
parmW string_one_segment
parmW string_one_offset
parmW string_two_segment
parmW string_two_offset
cBegin  quick_name_cmp

    assumes ds, DGROUP
    mov bx, gDescendingOrder    ; store the ascending/descending order flag
				; in 'bx'
    cld

    mov di,string_two_offset
    mov es,string_two_segment

    mov si,string_one_offset
    mov ds,string_one_segment
    assumes     ds,nothing

    mov cx, 11          ; size of name+extension
    xor ax, ax          ; mark return value as 0 - will be used later

    repe cmpsb

    ;;; ax is zero now
    je  short QNameC_end

    ;;; if we get here, one is <> two!
    ja short qnc_1_is_bigger

qnc_1_is_lesser:
    dec ax              ; ax = -1       
    dec ax              ; ax = -2
			; Flow thru now makes ax = -1 for return value

qnc_1_is_bigger:
    inc ax

    ;;; Based on the Sort Order -- whether we want to sort by ascending
    ;;; order or descending order we want to return 1, 0, -1 instead of
    ;;; the respective values of -1, 0, 1 for less, equal and greater resp.
      
    xor ax, bx          ; xor with sort order flag to xchg -1 and 1.
    
QNameC_end:
cEnd  quick_name_cmp

;;;; function similar to ext_cmp but with direct ASCII values!!
cProc  quick_ext_cmp,  PUBLIC,  <si,di,ds,es>
parmW string_one_segment
parmW string_one_offset
parmW string_two_segment
parmW string_two_offset
cBegin  quick_ext_cmp

    assumes ds, DGROUP
    mov bx, gDescendingOrder    ; store the ascending/descending order flag
				; in 'bx'
    cld

    mov di, string_two_offset
    mov es, string_two_segment
    ;;; add 8 to get to extension
    add di, 8

    mov si, string_one_offset
    mov ds, string_one_segment
    assumes ds, nothing
    ;;; add 8 to get to extension
    add si, 8

    mov cx, 3

    xor ax, ax                  ; mark return value as 0 - will be used later

    repe cmpsb

    ja  short qec_1_is_bigger

    jne short qec_1_is_lesser

    ;;; extensions are the same -- so compare names.
    mov di, string_two_offset

    mov si, string_one_offset

    mov cx, 8

    repe cmpsb

    ;;; ax is zero now
    je short QExtC_end

    ;;; if we get here, string one is <> string two
    ja short qec_1_is_bigger

qec_1_is_lesser:
    dec ax              ; ax = -1       
    dec ax              ; ax = -2
			; Flow thru now makes ax = -1 for return value

qec_1_is_bigger:
	inc ax

    ;;; Based on the Sort Order -- whether we want to sort by ascending
    ;;; order or descending order we want to return 1, 0, -1 instead of
    ;;; the respective values of -1, 0, 1 for less, equal and greater resp.
	
    xor ax, bx          ; xor with sort order flag to xchg -1 and 1.

QExtC_end:
cEnd  quick_ext_cmp


; THIS FUNCTION IS NOT USED
; int pascal strnzcmp(char far* string_one,char far* string_two, word)
;
;cProc  strnzcmp, PUBLIC , <si,di,ds,es>
;parmW string_one_segment
;parmW string_one_offset
;parmW string_two_segment
;parmW string_two_offset
;parmW len
;cBegin  strnzcmp
;    mov es,string_two_segment
;    mov di,string_two_offset
;    cld
;    xor ax,ax
;    mov bx,len
;    mov cx,bx
;    repne scasb ;;; find length of string 2 but don't look for EOS beyond len
;
;    sub bx,cx
;    mov cx,bx
;    sub di,bx
;    mov ds,string_one_segment
;    mov si,string_one_offset
;    repe cmpsb
;
;    ;;; ax is zero now
;    ja short str_one_is_bigger
;    ;;; if we get here, one is <= two
;    je StrC_end
;
;str_one_is_lesser:
;        mov ax,-1
;        jmp short StrC_end
;
;str_one_is_bigger:
;    mov ax,1
;
;StrC_end:
;cEnd  strnzcmp

ifdef DBCS
;
;       Test if the character is DBCS Lead Byte
;
;       input:  AL = character to check
;       outpit: ZF = 1 if DBCS Lead Byte
;

DBCSLeadByteTable       dd      0

IsDBCSLeadByte          proc    near
	push    ax
	push    si
	push    ds
	lds     si,cs:DBCSLeadByteTable
	cmp     word ptr cs:DBCSLeadByteTable+2,0
	jnz     idlb_check              ; if table is already set
	push    ax
	mov     ax,6300h
	int     21h                     ; get DBCS lead byte table
	pop     ax
	mov     word ptr cs:DBCSLeadByteTable,si
	mov     word ptr cs:DBCSLeadByteTable+2,ds
idlb_check:
	cmp     word ptr [si],0
	jz      idlb_not                ; if end of table
	cmp     al,[si]
	jb      idlb_next               ; if below low value
	cmp     al,[si+1]
	jbe     idlb_yes                ; if below high value
idlb_next:
	add     si,2                    ; do next
	jmp     short idlb_check
idlb_not:
	or      al,1                    ; reset ZF
	jmp     short idlb_end
idlb_yes:
	and     al,0                    ; set ZF
idlb_end:
	pop     ds
	pop     si
	pop     ax
	ret
IsDBCSLeadByte          endp
endif

sEnd   code


end
