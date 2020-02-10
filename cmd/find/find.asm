;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
        page    ,132
      title   DOS FIND  Utility
; 0
;********************************************************************
;*
;*   UTILITY NAME:         find.exe
;*
;*   SOURCE FILE NAME:     find.asm
;*
;*   STATUS:               Find utility, DOS Version 4.0
;*
;*   SYNTAX (Command line)
;*
;*         FIND [/V][/C][/N][/I] "string" [[d:][path]filename[.ext]...]
;*
;*         where:
;*
;*           /V - Display all lines NOT containing the string
;*           /C - Display only a count of lines containing string
;*           /N - Display number of line containing string
;*           /I - Ignore case   
;*
;*   UTILITY FUNCTION:
;*
;*     Searches the specified file(s) looking for the string the user
;*     entered from the command line.  If file name(s) are specifeied,
;*     those names are displayed, and if the string is found, then the
;*     entire line containing that string will be displayed.  Optional
;*     parameters modify that behavior and are described above.  String
;*     arguments have to be enclosed in double quotes.  (Two double quotes
;*     if a double quote is to be included).  Only one string argument is
;*     presently allowed.  The maximum line size is determined by buffer
;*     size.  Bigger lines will bomb the program.  If no file name is given
;*     then it will asssume the input is coming from the standard Input.
;*     No errors are reported when reading from standard Input.
;*
;*
;*   EXIT:
;*    The program returns errorlevel:
;*      0 - OK, and some matches
;*      1 - OK, but no matches
;*      2 - Some Error
;*
;*
;*   Revision History:
;*
;*    V1.1    8/23/82         M.A.U.  (Microsoft)
;*
;*    V1.2    9/22/82         M.A.U.  (Microsoft)
;*              Added the -c and -n options
;*
;*            9/23/82         M.A.U.  (Microsoft)
;*              Added DOS version number control
;*
;*            10/07/82  Rev.2         M.A.U.  (Microsoft)
;*              Changed quote for double quotes, and added
;*            file name printing
;*
;*            10/20/82  Rev.3         M.A.U.  (Microsoft)
;*              Modified IBM name to FIND, and changed the text
;*            of some messages.
;*
;*            10/25/82  Rev.4         M.A.U.  (Microsoft)
;*              Changed name to FIND and all messages to the
;*            IBM form.
;*
;*            10/27/82  Rev.5         M.A.U.  (Microsoft)
;*              Made the correct exit on version check in case
;*            of a 1.x DOS.
;*
;*            11/4/82 Rev. 5          A.R. Reynolds  (Microsoft)
;*               Messages moved to external module
;*
;*            11/10/82  Rev. 6        M.A. U.  (Microsoft)
;*              Corrected problem with line numbers, and a problem
;*            with seeking for 0 chars.
;*
;*            03/30/83  Rev. 7        M.A. U.  (Microsoft)
;*              Added patch area for bug fixing.
;*
;*            04/14/83  Rev. 8        M.A. U.  (Microsoft)
;*              Made changes for Kanji characters. (ugh!)
;*
;*            12/17/84  Rev. 9        Zibo  (Microsoft)
;*              Fix boundary case for buffer containing exact line
;*
;*    V4.0 :  6/29/87                 Russ W (IBM)
;*           Lines commented with ;AN000;
;*              Add support for IBM Parse service routines
;*              Add support for IBM Message Retriever Service Routines
;*              Add support for Code Page File Tags
;*              Made PROCs out of all labels that were targets of a call (not commented with AN000)
;*              Removed patch area for "bug fixing"
;*
;*    V4.0 :  9/15/87                 Bill L, (IBM)
;*           ;AN001; = DCR 201, changes to extended attributes support
;*           ;AN002; = PTM 1090
;*           ;AN003; = DCR 191
;*           ;AN004; = PTM 1630
;*           ;AN005; = PTM 1643, PTM 1675, PTM 1754
;*           ;AN006; = DBCS support
;*           ;AN007; = Optimizations to save disk space on ship diskettes
;*
;*    V5.0 :  8/9/90
;*	      M01	SA	Modified parser parameter block used by pre-parse
;*				so that switches within strings are not recognized.
;*				Modified pre-parse routine so that it continues
;*				parsing if a string or file-spec is identified.
;*	
;*            M002      MD      Improved segment usage, added international
;*                              upper casing, fixed /I bug.  Not all changes
;*                              marked.
;*
;*    V6.0 : 10/19/92
;*                      MD      Got return code working
;*
;**********************************************************************

;--------------------------
;-      MACRO DEFINITIONS
;--------------------------
BREAK   MACRO   subtitle
        SUBTTL  subtitle
        PAGE
ENDM


;---------------------------;
;-      INCLUDE FILES       ;
;---------------------------;
.xlist                      ;
.xcref                      ;
        INCLUDE SYSCALL.INC ;
        INCLUDE sysmsg.inc  ;   ;AN000; Include message equates and MACROS
        INCLUDE find.inc    ;   ;AN000; Include find equates and MACROS
.list                       ;
.cref                       ;
;---------------------------;

MSG_UTILNAME <FIND>             ;AN000;

;--------------------------
;-      EQUATES
;--------------------------
FALSE           equ     0
TRUE            equ     NOT FALSE

CR              equ     0dh             ;A Carriage Return
LF              equ     0ah             ;A Line Feed
quote_char      equ     22h             ;A double quote character


buffer_size     equ     4096            ;file buffer size
st_buf_size     equ     128             ;string arg. buffer size
fname_buf_size  equ     64              ;file name buffer size


;----- DOS EQUATES -----;
STDIN   equ     0                       ;AN000; Handle
STDOUT  equ     1                       ;AN000; Handle
STDERR  equ     2                       ;AN000; Handle

ERROR_ACCESS_DENIED     equ     5       ;AN000; Int 021h error return

;------------------------
;-      MESSAGE EQUATES
;------------------------

msg_file_not_found      equ     2       ;AN000; File not found %s
msg_access_denied       equ     5       ;AN000; Access denied %s
msg_read_error          equ    30       ;AN000; Read error in %s
msg_inv_num_parm        equ     2       ;AN000; Invalid number of parameters
msg_inv_parm            equ    10       ;AN000; Invalid Parameter %s
msg_required_missing    equ     2       ;AN005; Required parameter missing
msg_find                equ     4       ;AN000; FIND:
msg_code_page_mismatch  equ    37       ;AN005; Code Page mismatch
msg_switch              equ     3       ;AN005; Invalid switch

msg_opt1		equ	10      ; these three messages comprise the
msg_opt2		equ	11      ; explanation of the options allowed
msg_opt3		equ 	12      ; for find.      leaf, april 1990

MSG_OPTIONS_FIRST	equ	300	; first options help msg #
MSG_OPTIONS_LAST	equ	310	;  and last

;-----------------------
;--     Parser equates
;-----------------------
 
FarSW   equ     0                       ;AN000;
DateSW  equ     0                       ;AN000;
TimeSW  equ     0                       ;AN000;
FileSW  equ     1                       ;AN000;
CAPSW   equ     1                       ;AN000;
CmpxSW  equ     0                       ;AN000;
DrvSW   equ     0                       ;AN000;
QusSW   equ     1                       ;AN000;
NumSW   equ     0                       ;AN000;
KeySW   equ     0                       ;AN000;
SwSW    equ     1                       ;AN000;
Val1SW  equ     0                       ;AN000;
Val2SW  equ     0                       ;AN000;
Val3SW  equ     0                       ;AN000;

;------------------------
; SUBLIST Equates
;------------------------
Left_Align            equ     0      ;AN000; 00xxxxxx
Right_Align           equ     80h    ;AN000; 10xxxxxx

Char_Field_Char       equ     0      ;AN000; a0000000
Char_Field_ASCIIZ     equ     10h    ;AN000; a0010000

Unsgn_Bin_Byte        equ     11h    ;AN000; a0010001 - Unsigned Binary to Decimal character
Unsgn_Bin_Word        equ     21h    ;AN000; a0100001
Unsgn_Bin_DWord       equ     31h    ;AN000; a0110001

Sgn_Bin_Byte          equ     12h    ;AN000; a0010010 - Signed Binary to Decimal character
Sgn_Bin_Word          equ     22h    ;AN000; a0100010
Sgn_Bin_DWord         equ     32h    ;AN000; a0110010

Bin_Hex_Byte          equ     13h    ;AN000; a0010011 - Unsigned Binary to Hexidecimal character
Bin_Hex_Word          equ     23h    ;AN000; a0100011
Bin_Hex_DWord         equ     33h    ;AN000; a0110011

;---------------------------------------
;-------------- CODE SEGMENT -----------
;---------------------------------------

code    segment public
            assume   cs:code
            assume   ds:nothing
            assume   es:nothing
            assume   ss:stack

        jmp start

;
;--------------------
.xlist
.xcref
include version.inc
INCLUDE parse.asm
.list
.cref
;--------------------

        EXTRN   heading:byte,heading_len:byte

;-----------------------
;----- Misc  Data ------
bufferDB db     6 dup(0)        ;AN006;
dbcs_off dw     0               ;AN006;
dbcs_seg dw     0               ;AN006;
dbcs_len dw     0               ;AN006;

ccolon  db      ": "
n1_buf  db      "["
n2_buf  db      8 dup(0)                ;buffer for number conversion

; possible values of return code
MATCH_EXIT      equ     0               ; match found, no errors
NO_MATCH_EXIT   equ     1               ; no match found, no errors
ERROR_EXIT      equ     2               ; error encountered, search incomplete

errlevel        db      NO_MATCH_EXIT   ; assume no error, no match

GET_UPPER_TABLE equ     6502h           ; DOS get upper case table call    
ucasetab_bias   dw      0               ; value of (256-table size)
uppercase_table db      5 dup (?)       ; buffer for upper case table
                                        ; used of DOS Function 65h,
                                        ; and to store real table address

; command line switch flags

switches	db	0	; holds switches specified on command line

LINE_NUMS	equ	1	; /n - print line numbers with matched lines
NOT_MATCHED	equ	2	; /v - print lines not containing match
LINE_COUNT	equ	4	; /c - print count of matching lines only
NO_CASE 	equ	8	; /i - case insensitive search
HELP		equ	10h	; /? - display help message


;----- LINE COUNTERS
mtch_cntr dw    0                       ;matched lines counter
line_cntr dw    0                       ;line counter

;-------------------------------------------
;-      MESSAGE RETRIEVER SUBSTITUTION LIST
;-------------------------------------------

MSG_SERVICES <MSGDATA>          ;AN000;

sublist label dword             ;AN000;
sl_size db      11              ;AN000; SUBLIST Size, in bytes
sl_res  db      0               ;AN000; reserved
sl_ptr_o dw     ?               ;AN000; Offset  PTR to data item
sl_ptr_s dw     ?               ;AN000; Segment PTR to data item
sl_n    db      0               ;AN000; n of %n
sl_flag db      ?               ;AN000; Data-Type flags
sl_maxw db      0               ;AN000; Max width
sl_minw db      0               ;AN000; Min width
sl_pad  db      ' '             ;AN000; Pad character


parm         db ?               ;AN000; Save area for invalid parm

;******************************************************************************
;*                                               PARSER DATA STRUCTURES FOLLOW
;******************************************************************************

parms   label   byte            ;AN000;
        dw      parmsx          ;AN000; POINTER TO PARMS STRUCURE
        db      1               ;AN000; DELIMITER LIST FOLLOWS
        db      1               ;AN000; NUMBER OF ADDITIONAL DELIMITERS
        db      ";"             ;AN000; ADDITIONAL DELIMITER

parms1  label   byte            ;AN005;
        dw      parmsx1         ;AN005; POINTER TO PARMS STRUCURE
        db      1               ;AN005; DELIMITER LIST FOLLOWS
        db      1               ;AN005; NUMBER OF ADDITIONAL DELIMITERS
        db      ";"             ;AN005; ADDITIONAL DELIMITER

;------------------------------
;- STRUCTURE TO DEFINE FIND SYNTAX REQUIREMENTS
;------------------------------
parmsx  label   word            ;AN000;
        db      1,2             ;AN000; THERE ARE BETWEEN 1 AND 2 POSITIONAL PARMS
        dw      pos1            ;AN000; POINTER TO POSITIONAL DEFINITION AREA
        dw      pos2            ;AN000; POINTER TO POSITIONAL DEFINITION AREA
        db      1               ;AN000; THERE IS 1 SWITCH DEF AREA FOR "/V, /C, AND /N"
        dw      sw1             ;AN000; POINTER TO FIRST SWITCH DEFINITION AREA
        dw      0               ;AN000; THERE ARE NO KEYWORDS IN FIND SYNTAX

parmsx1 label   word            ;AN000;
        db      0,2             ;AN000; THERE ARE BETWEEN 0 AND 2 POSITIONAL PARMS
					;M01;allow minimum of 0 so that command line
					;M01;errors are caught by main parse (after pre-parse)
        dw      pos1            ;AN000; POINTER TO POSITIONAL DEFINITION AREA
        dw      pos2            ;AN000; POINTER TO POSITIONAL DEFINITION AREA
        db      1               ;AN000; THERE IS 1 SWITCH DEF AREA FOR "/V, /C, AND /N"
        dw      sw1             ;AN000; POINTER TO FIRST SWITCH DEFINITION AREA
        dw      0               ;AN000; THERE ARE NO KEYWORDS IN FIND SYNTAX
;M01;end

        ;------------------------------
        ;- STRUCTURE TO DEFINE POSITIONAL PARM
        ;------------------------------
pos1    label   word            ;AN000;
        dw      0080h           ;AN000; QUOTED STRING, REQUIRED
        dw      0000h           ;AN000; NO CAPITALIZE
        dw      ret_buff        ;AN000; PLACE RESULT IN RET_BUFF
        dw      novals          ;AN000; NO VALUE LIST
        db      0               ;AN000; NO KEYWORDS

        ;------------------------------
        ;- STRUCTURE TO DEFINE POSITIONAL PARM
        ;------------------------------
pos2    label   word            ;AN000;
        dw      0203h           ;AN000; FILE NAME, OPTIONAL, REPEATS ALLOWED
        dw      0001h           ;AN000; CAPITALIZE BY FILE TABLE
        dw      ret_buff        ;AN000; PLACE RESULT IN RET_BUFF
        dw      novals          ;AN000; NO VALUE LIST
        db      0               ;AN000; NO KEYWORDS


        ;------------------------------
        ;- STRUCTURE TO DEFINE THE SWITCHES
        ;------------------------------
	public	sw1
	public	n_swch

sw1     label   word            ;AN000;
        dw      0               ;AN000; NO MATCH FLAGS
        dw      2               ;AN005; capitalize
        dw      ret_buff        ;AN000; PLACE RESULT IN RET_BUFF
        dw      novals          ;AN000; NO VALUE LIST
	db	5		;AN000; Five SWITCHES IN FOLLOWING LIST
n_swch  db      "/N",0          ;AN000;
v_swch  db      "/V",0          ;AN000;
c_swch  db      "/C",0          ;AN000;
i_swch	db	"/I",0
?_swch  db      "/?",0


        ;------------------------------
        ;- VALUE LIST DEFINITION FOR NO VALUES
        ;------------------------------
novals  label   word            ;AN000;
        db      0               ;AN000;  VALUE LIST


        ;------------------------------
        ;- RETURN BUFFER FOR POSITIONAL PARAMETERS
        ;------------------------------
ret_buff        label   word    ;AN000;
rb_type         db      ?       ;AN000; TYPE RETURNED
rb_item_tag     db      ?       ;AN000; SPACE FOR ITEM TAG
rb_synonym      dw      ?       ;AN000; ES:rb_synonym points to synonym
rb_value_lo     dw      ?       ;AN000; SPACE FOR VALUE
rb_value_hi     dw      ?       ;AN000; SPACE FOR VALUE




did_file        db      FALSE   ;AN004;  if true then already processed a file
got_eol         db      FALSE   ;AN004;  if false then possibly more filenames on command line
got_filename    db      FALSE   ;AN004;  if true then parser found a filename on command line
got_srch_str    db      FALSE   ;AN000;  if true then parser found search string on command line
ordinal         dw      0       ;AN000; parser ordinal
crlf            db      CR,LF   ;AN000;


;
;************************************************************
;*
;*   SUBROUTINE NAME:      main
;*
;*   SUBROUTINE FUNCTION:
;*         Process the command line.  If there are no errors, then open
;*         the specified files, search for string, display it to the
;*         standard output device.
;*
;*   INPUT: Command line (described in program header)
;*
;*   OUTPUT:
;*         Files will be opened and read in.  Regardless of the command
;*         line parameters entered by the user, output will be written
;*         to the standard output device handle 1.
;*
;*   NORMAL EXIT:
;*         File(s) opened (if not STDIN), read successfully, and closed.
;*         Display requested information.
;*
;*   ERROR CONDITIONS:
;*         Incorrect DOS version
;*         Invalid number of parameters
;*         Syntax error
;*         Access denied
;*         File not found
;*         Invalid Parameter
;*         Read error in
;*
;*   INTERNAL REFERENCES:
;*         bin2asc
;*         clr_cntrs
;*         is_prefix
;*         next_kchar
;*         print_count
;*         prout
;*         prt_err
;*         prt_err_2
;*         prt_file_name
;*         prt_lcntr
;*
;**************************************************************************

        MSG_SERVICES <FIND.ctl,FIND.cla,FIND.cl1,FIND.cl2>      ;AN000;
        MSG_SERVICES <DISPLAYmsg,LOADmsg,CHARmsg,NOCHECKSTDIN>  ;AN003; Make retriever services available
START:

        mov     ax,cs                   ;load ES to the right area,
        mov     es,ax                   ;
        mov     ds,ax                   ;

        assume  ds:code, es:code

        call    sysloadmsg              ;AN000; Preload messages, Check DOS Version.
        jnc     Set_for_parse           ;AN000; If no error, parse command line

        call    prt_find                ;AN005;
        call    sysdispmsg              ;AN005;

        mov     errlevel,ERROR_EXIT     ; report error occurred
        call    terminate               ; does not return

;-----------------------------------
;- DOS version is ok. Parse cmd line
;-----------------------------------
Set_for_parse:
        call    get_dbcs_vector                         ;AN006; ;Get DOS dbcs table vector
;
        mov     ah,GetCurrentPSP                        ;AN000; Get PSP address, returned in BX
        int     021h                                    ;AN000;
        mov     ds,bx                                   ;AN000; Put PSP Seg in DS

        assume  ds:nothing

        mov     si,081h                                 ;AN000; Offset of command line in PSP
        xor     cx,cx                                   ;AN000; Number of args processed so far = 0
        mov     cs:ordinal,cx                           ;AN000; init parser ordinal

;--------------------------------------
; See if there was nothing entered
;--------------------------------------
        cmp     byte ptr ds:080h,0                      ;AN000; Check length of command line,
        jne     p_parse                                 ;AN005; Go process the parameters
        mov     ax,msg_inv_num_parm                     ;AN000; No parms, too bad!
        mov     dh,2                                    ;AN005; message class
        call    display_and_die                         ;AN000; Tell the unfortunate user
p_parse:
        mov     cs:got_filename,FALSE        ;AN004; input file default is STDIN

        push    cs                      ;A0005; ensure es is correct
        pop     es                      ;AN005;

        assume  es:code

        call    clr_cntrs               ;AN005; set all counters to zero
        mov     cx,ordinal              ;AN005; init parser ordinal
        call    pre_parse               ;AN005;
	test	switches,HELP
	jz	parser
	jmp	foo

PARSER:
        push    cs                      ;A0000; ensure es is correct
        pop     es

        assume  es:code

        call    clr_cntrs               ; set all counters to zero
        mov     cx,ordinal              ;AN000; init parser ordinal
        call    parse                   ;AN000; Parse command line

        push    si                      ;AN000; Save ptr to remaining command line
        push    ds

        push    cs                      ;Load new DS with CS
        pop     ds

        assume  ds:code

        mov     ordinal,cx              ;AN000; Save parser ordinal

;---------------------
; get filespec size
;---------------------
        mov     file_name_buf,di        ;save buffer offset from parser
        xor     bx,bx                   ;AN000;indicate no save again
        call    get_length              ;AN000;get filespec length
        mov     file_name_len,ax        ;save the name length

; Get pointer to upper case table if necessary - M002

        test    switches,NO_CASE        ; case ignore active?
        jz      open_file

        push    es
        mov     ax,GET_UPPER_TABLE
        mov     bx,-1                   ; current code page
        mov     cx,5                    ; size of info for upper case table
        mov     dx,bx                   ; current country id
        push    cs
        pop     es

        assume  es:code

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

;---------------------
;-      OPEN FILE FOR READING
;---------------------                  ; M002 end
open_file:

        cmp     got_filename,TRUE       ;AN004; using STDIN
        je      o_cont                  ;AN004; no, open the file
        xor     ax,ax                   ;AN007; ;AN004; file handle (ax) = STDIN
	jmp	short say_name		;AN007; ;AN004; skip open of file
o_cont:                                 ;AN004;
        mov     dx,file_name_buf        ;AC000;addrss. of the file name
openit:
        mov     ah,open
        mov     al,0                    ;file open for reading
        int     021h                    ;call the DOS
	jc	do_open_error		;AN000;

;---------------------
;-      PRINT FILE NAME
;---------------------
say_name:
        push    ax                      ;save file handle
        cmp     got_filename,FALSE      ;AN004; using STDIN
        je      xx1                     ;AN004; yes, don't print a filename
        mov     dx,offset heading
        mov     cl,heading_len
        xor     ch,ch
        call    prout

        mov     dx,file_name_buf     
        mov     cx,file_name_len
        call    prout

	test	switches,LINE_COUNT   	;count only flag set?
	jnz	xx1

        mov     dx,offset crlf
        mov     cx,2
        call    prout
xx1:
        pop     ax

;---------------------
;-      Fill Buffer for Matching
;---------------------
fill:
        mov     bx,ax                   ;retrieve handle
refill:
        mov     dx,offset buffer        ;data buffer addrss.
        mov     cx,buffer_size
        mov     ah,read
        int     021h
        jnc     no_read_error           ;if carry then read error
        jmp     read_error

no_read_error:
        or      ax,ax                   ;if ax=0 then all done
        jnz     Truncate

DoNullRead:
	test	switches,LINE_COUNT	;count only flag set?
	jz	sj2
        call    print_count

sj2:
        and     bx,bx                  ;Using STD IN?
        jnz     regular
        jmp     foo                     ;if so: all done, exit

regular:
        mov     ah,close                ;otherwise close the file
        int     021h
        jmp     scan_rest               ;get another file

do_open_error:
        jmp     open_error              ;AN000;
;---------------------------
; We have read in an entire buffer.  Scan for a ^Z and terminate the buffer
; there.  Change only CX
;---------------------------
Truncate:
        push    di
        push    cx
        push    es
        mov     di,dx
        mov     cx,ax
        mov     ax,ds
        mov     es,ax

        assume  es:code

        mov     al,1Ah
        CLD
        repnz   scasb
;---------------------------
; If zero is set, the the previous character is a ^Z.  If it is reset then
; the previous character is the end of buffer.  With ^Z, we back up over the
; char.
;---------------------------
        jnz     chop
        dec     di
chop:
        mov     ax,di
        sub     ax,dx                   ; get true length of buffer
        pop     es

        assume  es:nothing

        pop     cx
        pop     di
        or      ax,ax
        jz      DoNullRead

;---------------------------
;----- MATCH ROUTINE
;---------------------------
;Note: If input is being taken from a file the stack contains
; (from top to bottom):
;       - Pointer to the next command in the command line
;       - Pointer to the program segment prefix (to be loaded into
;         DS to access the command line.
; if the input is from the standard input then NONE of it will be
; in the stack.
;---------------------------

go_match:
        push    bx                      ;save the file handle
        mov     bp,offset buffer        ;ptr to first line of file
;---------------------------
; At this point we must check to make sure there is AT LEAST one LF in the
;  buffer. If there is not, then we must insert one at the end so we
;  don't get stuck trying to get one complete line in the buffer when
;  we can't cause the buffer ain't big enough.
;---------------------------
        push    ax                      ; Save true buffer size
        mov     cx,ax                   ; scan whole buffer
        mov     al,LF                   ; for a LF
        mov     di,bp                   ; start of buffer
        repnz   scasb
        pop     ax                      ; recover buffer size
        mov     di,ax                   ;displacement from beg of buffer
        jnz     last_line               ; No line feeds, must insert one
;---------------------------
; Check to see if we reached EOF (return from READ less than buffer_size).
;  If EOF we must make sure we end with a CRLF pair.
;---------------------------
        cmp     ax,buffer_size-1        ;last line of the file?
        jg      no_last_line            ;nope
last_line:                              ;if yes, add a CRLF just in case
        mov     bx,bp
        cmp     byte ptr[bx+di-1],LF    ;finished with a LF?
        je      no_last_line            ;yes, it's an OK line.
        mov     byte ptr[bx+di],CR      ;put a CR at the end of the data
        inc     di
        mov     byte ptr[bx+di],LF      ;put a LF ...
        inc     di

no_last_line:
        push    di                      ;save the # of chars. in the buffer
        push    bp
        mov     dx,st_length            ;length of the string arg.
        dec     dx                      ;adjust for later use
        jmp     short try_again

more_stuff_o:
        jmp     more_stuff

;----- SCAN LINES IN THE BUFFER FOR A MATCH -------------------------;
;Note: at this point the stack contains (from top to bottom):
;       - Stuff mentioned before
;       - File Handle
;       - Number of chars. left in the buffer from the next line.
;       - Addrs. of the next line in the buffer.
;
; plus, DX has the adjusted length of the string argument.
;
; We are about to begin scanning a line.  We start by determining if there is
; a complete line in the buffer.  If so, we scan for the char.  If NOT, we go
; and grab new info.
;---------------------------
	public	try_again
try_again:
        pop     bp                      ;addrs. of next line in the buffer
        mov     di,bp                   ;points to beg. of a line
        pop     cx                      ;get # of chars left in the buffer
        mov     bx,cx                   ;save in case a non-complete line
        mov     al,LF                   ;search for a Line Feed
        jcxz    more_stuff_o            ;no chars left in buffer
        repnz   scasb
        jnz     more_stuff_o            ;no full line left in buffer
        push    cx                      ;save chars left in buffer
        push    di                      ;points to beg. of next line
        mov     cx,di
        sub     cx,bp                   ;length of the current line
        mov     bx,cx                   ;save in case it has a match
        dec     cx                      ;Discount the LF we found
        cmp     byte ptr ES:[DI-2],CR   ; Is there a CR to discount too?
        jnz     NO_SECOND_DEC           ; No there is not.
        dec     cx                      ;CR character discounted
NO_SECOND_DEC:
        inc     line_cntr               ;increment line counter
        jcxz    try_again_opt           ;if line empty go to next line
        mov     di,bp                   ;pointer to the beg. of current line
another_char:
;---------------------------
; On entry:
;       BX      line length
;       CX      adjusted line length
;       DX      adjusted string argument length
;       DI      points to beg. of line
;---------------------------
        push    dx                      ;save for next line
lop:
        pop     dx
        push    dx
        inc     dx                      ;different algorithm!
        mov     si,offset st_buffer     ;pointer to beg. of string argument

comp_next_char:
        push    di
        mov     di,si
        call    is_prefix               ;check for a prefix char
        pop     di
        jnc     nopre
        lodsw
        cmp     cx,1                    ; Can not compare a two byte char
        jbe     try_again_opt1          ; if there is only one available
        cmp     ax,word ptr [di]
        jz      kmatch1
        jmp     short back_up           ;AN007;

nopre:
        lodsb
        cmp     al,byte ptr [di]
        jz      kmatch
	test	switches,NO_CASE	; case insensitive search?
	jz	back_up 		; back up now if not
        mov     ah,[di]                 ; get other char in AH
        call    check_case              ; check case of both chars
	cmp	al,ah           	; compare again
	jz	kmatch

back_up:
        pop     ax                      ; Original length of comp string
        push    ax
        inc     ax
;---------------------------
    ; Our match failed IN THE MIDDLE of the string (partial match). We need
    ;  to back up in the line to the NEXT char after the one which matched
    ;  the first char of the search string and try again. The amount to
    ;  back up to where we started is ax-dx (the result MAY be 0, this is OK).
    ;  we then need to skip ONE char in the line.
;---------------------------
        sub     ax,dx                   ; AX = AX-DX
        sub     di,ax                   ; Do the back up.
        add     cx,ax                   ; restore count too!
        call    next_kchar              ;no match, advance di to next kanji
        jc      try_again_opt1          ;not enough chars left in line
        jmp     short lop               ;try another char in line

try_again_opt1:
        pop     dx
        jmp     short try_again_opt     ;AN007;


kmatch1:
        dec     dx                      ;last char had prefix so it was
                                        ; long.
kmatch:
        dec     dx
        jz      a_matchk                ; no chars left: a match!
        call    next_kchar
        jc      try_again_opt1
        jmp     comp_next_char          ; loop if chars left in arg.

a_matchk:
        mov     errlevel,MATCH_EXIT     ; match found, set return code
        pop     dx
	test	switches,NOT_MATCHED    ;is flag set?
	jz	prt_line		;no, print the line
        jmp     try_again

;---------------------------
;-      NO MATCH: CHECK FOR THE v OPTION
;---------------------------
try_again_opt:
	test	switches,NOT_MATCHED    ;is flag set?
	jnz	prt_line
	jmp	try_again		;no goto next line

;---------------------------
;-      PRINT THE LINE WITH THE MATCH
;Note: at this point the stack contains (top to bottom)
;       - Stuff mentioned before
;
; plus, BP points to begginig of the current line, BX has the length
;of the current line including the CRLF, and DX the adjusted length of
;the string argument.
;---------------------------

prt_line:
	test	switches,LINE_COUNT	;is count only flag set?
	jz	no_line_count
        inc     mtch_cntr               ;yes, increment counter
        jmp     try_again

no_line_count:
        push    dx                      ;save the adjusted string arg. length
	test	switches,LINE_NUMS	;is line number flag set?
	jz	no_line_nums
        call    prt_lcntr

no_line_nums:
        mov     dx,bp
        mov     cx,bx
        call    prout
        pop     dx                      ;restore
        jmp     try_again

;----- READ MORE TEXT LINES INTO THE BUFFER -------------------------;
; The scanning routines have detected that the buffer does not
;contain a full line any more. More lines have to be read into the
;buffer. But first perform a seek on the file in order to re-read
;the non-complete line into the begining of the buffer.
; Uppon entry BP contains points to the begining of the non-complete
;line, and BX has the number of characters left in the buffer.
; The Stack contains (top to bottom):
;       - Pointer to the next command in the command line
;       - Pointer to the program segment prefix (to be loaded into
;         DS to access the command line).
;       - File handle.

more_stuff:
        mov     dx,bx                   ;get chars left in buffer
        pop     bx                      ;get the handle
        or      dx,dx                   ;are there 0 left?
        jz      no_seek                 ;yes, do not seek
        neg     dx                      ;form two's complement
        mov     cx,-1
        mov     al,1                    ;seek from the current position
        mov     ah,lseek                ;seek on file
        int     021h
        jc      read_error
no_seek:
        jmp     refill                  ;no errors: refill the buffer
read_error:
        and     bx,bx                   ;AN007; ;Using STD IN?
        je      foo                     ;if so: all done, exit
        mov     ah,close                ;close the file
        int     021h
;---------------
;------ Set message number and go display it

        mov     ax,msg_read_error       ;AN000; Read error message
        jmp     short r_error           ;AN007;

;---------------------
;-      PRINT ERRORS
;---------------------
open_error:
        cmp     ax,ERROR_ACCESS_DENIED  ;AN000;
        jnz     DoNorm

        mov     ax,msg_access_denied    ;AN000; Message for Access Denied
        jmp     short r_error           ;AN007; ;AN000; Do the rest

DoNorm:                                 ;AN000;
        mov     ax,msg_file_not_found   ;AN000; Message for File Not Found

r_error:
        call    prt_find                ;AN005;
        mov     sl_ptr_s,ds          ;AN000; Save segment of subst text
        mov     cx,file_name_buf     ;AN000;
        mov     sl_ptr_o,cx          ;AN000; Save offset  of subst text
        mov     sl_flag,left_align+char_field_ASCIIZ ;AN000; Type of insertion text
        mov     bx,STDERR               ;AN000; Sent to STD OUT
        mov     cx,1                    ;AN000; One substitution string
        mov     dh,1                    ;AN000; Its a utility message

        call    display_msg             ;AN000; Display rror message
        mov     errlevel,ERROR_EXIT     ; signal exiting with error

;---------------------
;-      SCAN THE REST OF THE COMMAND LINE
;---------------------
scan_rest:
        pop     ds                      ;restore pointer to comm. line

        assume  ds:nothing

        pop     si                      ;restore pointer to next comm.
        mov     did_file,TRUE           ; tell parser we did a file, so if it doesn't find another, ok!
        cmp     got_eol,TRUE            ; Check if nothing left on command line
        je      foo                     ; no, nothing left on command line, exit
        jmp     parser

foo:
        call    terminate               ; terminate


;--------------------------
;       Clear Counters
;--------------------------
        assume  ds:nothing

clr_cntrs proc  near
        mov     byte ptr cs:mtch_cntr,0
        mov     byte ptr cs:line_cntr,0
        ret
clr_cntrs endp


;--------------------------
;       Print Count of Matched lines
;       Modifies: AX,CX,DX and DI
;--------------------------
        assume  ds:code

print_count     proc    near
        push    bx                      ;save handle
        and     bx,bx                   ;AN007; ;using STDIN?
        jz      sj3                     ;if so do not print file name

        mov     dx,offset ccolon
        mov     cx,2
        call    prout                   ;print colon
sj3:
        mov     ax,mtch_cntr
        mov     di,offset n2_buf        ;buffer for characters
        call    bin2asc                 ;convert to ascii
        mov     dx,offset n2_buf
        call    prout                   ;print the number
        mov     dx,offset crlf
        mov     cx,2
        call    prout                   ;print an end of line
        pop     bx
        ret
print_count     endp

;--------------------------
;       Print relative line number

;       Modifies: AX,CX and DI
;--------------------------
        assume  ds:code

prt_lcntr       proc    near
        push    bx
        push    dx
        mov     ax,line_cntr
        mov     di,offset n2_buf
        call    bin2asc
        mov     byte ptr[di],"]"
        inc     cx
        inc     cx
        mov     dx,offset n1_buf
        call    prout
        pop     dx
        pop     bx
        ret
prt_lcntr endp

;--------------------------
;       Print string to STDOUT
;--------------------------
        assume  ds:code

prout   proc    near
        mov     bx,STDOUT
        mov     ah,write
        int     021h
        ret
prout   endp

;--------------------------
;       Binary to Ascii conversion routine
; Entry:
;       AX      Binary number
;       DI      Points to one past the last char in the
;             result buffer.
; Exit:
;       Result in the buffer MSD first
;       CX      Digit count
; Modifies:
;       AX,BX,CX,DX and DI
;--------------------------
        assume  ds:code

bin2asc proc    near
        mov     bx,0ah
        xor     cx,cx
go_div:
        inc     cx
        cmp     ax,bx
        jb      div_done
        xor     dx,dx
        div     bx
        add     dl,'0'          ;convert to ASCII
        push    dx
        jmp     short go_div

div_done:
        add     al,'0'
        push    ax
        mov     bx,cx
deposit:
        pop     ax
        stosb
        loop    deposit
        mov     cx,bx
        ret
bin2asc endp

;--------------------------
;       Compares chars in AH and AL regardless of case
;               if either char is lower case, convert to upper and
;               compare.  On entry, we've already established the
;               chars don't compare as they are, so we need to check
;               if they compare after case mapping
;
;       entry:
;               AL,AH   have the characters to compare
;       exit:
;               AL,AH   converted to upper case
;       modifies:
;               AX
;--------------------------
;M002 modified this routine

        assume  ds:code

check_case proc	near

        cmp     al,80h                  ; if AL is extended char, get mapping
        jb      not_al_high             ; AL not extended char
        call    use_table               ; get mapping

not_al_high:
        xchg    ah,al
        cmp     al,80h
        jb      not_ah_high
        call    use_table

not_ah_high:
        cmp     al,'a'
        jb      not_al_lower
        cmp     al,'z'
        ja      not_al_lower
        and     al,0DFh

not_al_lower:
        xchg    ah,al
        cmp     al,'a'
        jb      not_ah_lower
        cmp     al,'z'
        ja      not_ah_lower
        and     al,0DFh

not_ah_lower:
        ret

use_table:
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

check_case endp



;--------------------------
;       ADVANCE POINTER TO NEXT KANJI CHARACTER
; entry:        DI  points to a Kanji string
;               CX  length in bytes of the string
; exit:         DI  points to next Kanji char
;               CX  has number of bytes left
; modifies:     AX
;--------------------------
        assume  ds:code

next_kchar      proc    near
        jcxz    no_kleft
        call    is_prefix
        jnc     no_p
        inc     di
        dec     cx
        jcxz    no_kleft                ; for insurance
no_p:
        inc     di
        dec     cx
        clc
        ret

no_kleft:
        stc
        ret
next_kchar      endp

;--------------------------
;       Get DOS dbcs table vector
; entry:  none
; exit:   none
; modifies: none
;--------------------------
get_dbcs_vector proc near             ;AN006;
        push es                       ;AN006;
        push di                       ;AN006;
        push ax                       ;AN006;
        push bx                       ;AN006;
        push cx                       ;AN006;
        push dx                       ;AN006;
;
        mov  ax,cs                    ;AN006; ;segment of return buffer
        mov  es,ax                    ;AN006;

        assume  es:code

        mov  di,offset bufferDB       ;AN006; ;offset of return buffer
        mov  ah,65h                   ;AN006; ;get extended country info
        mov  al,07h                   ;AN006; ;get DBCS environment table
        mov  bx,0ffffh                ;AN006; ;use active code page
        mov  cx,5                     ;AN006; ;number of bytes returned
        mov  dx,0ffffh                ;AN006; ;default country ID
        int  21h                      ;AN006; ;DOS function call,vector returned
                                      ;AN006; ; in ES:DI
        assume  es:nothing

        inc  di                       ;AN006; ;skip over id byte returned
        mov  ax,word ptr es:[di]      ;AN006; ;get offset of DBCS table
        mov  cs:dbcs_off,ax           ;AN006; ;save it
;
        add  di,2                     ;AN006; ;skip over offset to get segment
        mov  bx,word ptr es:[di]      ;AN006; ;get segment of DBCS table
        mov  cs:dbcs_seg,bx           ;AN006; ;save it
;
        mov  di,ax                    ;AN006; ;Point to DBCS table to get length
        mov  es,bx                    ;AN006;
        mov  ax,word ptr es:[di]      ;AN006;
        mov  cs:dbcs_len,ax           ;AN006;
        add  cs:dbcs_off,2            ;AN006; ;change offset to point to table
;
        pop  dx                       ;AN006;
        pop  cx                       ;AN006;
        pop  bx                       ;AN006;
        pop  ax                       ;AN006;
        pop  di                       ;AN006;
        pop  es                       ;AN006;
;
        ret                           ;AN006;
get_dbcs_vector endp                  ;AN006;


;--------------------------
;       FIND OUT IS THE BYTE IS A KANJI PREFIX
; entry:  DI    points to a kanji string
; exit:   Carry set if it is a kanji prefix
; modifies:     AX
;--------------------------
        assume  ds:code

is_prefix proc near                   ;AN006;
        push    es
        push    si
        push    ax
;
        mov     si,dbcs_off        ;ES:SI -> DOS dbcs table
        mov     ax,dbcs_seg
        mov     es,ax
;
        assume  es:nothing

        mov     al,byte ptr [di]   ;get first byte of string
;
; Two consecutive 00 bytes signifies end of table
;

is_loop:
        cmp  word ptr es:[si],00h     ;Check for two consecutive 00 bytes
        jne  is_next1                 ;no, continue
        clc                           ;clear carry - byte is not lead byte of db char
        jmp  short is_exit            ;AN007; ;yes, found them, quit

;
; Check if byte is within range values of DOS dbcs table
;

is_next1:
        cmp  al,byte ptr es:[si]      ;is byte >= first byte in range?
        jae  is_next2                 ;yes, continue
        jmp  short is_again           ;AN007; ;no, loop again

is_next2:
        cmp  al,byte ptr es:[si+1]    ;is byte <= last byte in range?
        jbe  is_found                 ;yes, found a lead byte of db char

is_again:
        add  si,2                     ;no, increment ptr to next range
        jmp  is_loop

is_found:
        stc                           ;byte is lead byte of db char, set carry

is_exit:
        pop  ax
        pop  si
        pop  es
;
        ret
is_prefix       endp


;
;---------------------
;- Terminate process
;---------------------
terminate       proc    near          ;AN000;
        mov     ah,exit               ;AN000; Terminate function call
        mov     al,cs:errlevel        ;AN000; Errorlevel placed in AL
        int     021h                  ;AN000; Terminate
        ret                           ;AN000; Meaningless return
terminate       endp                  ;AN000;


;
;************************************************************
;*
;*   SUBROUTINE NAME:      display_msg
;*
;*   SUBROUTINE FUNCTION:
;*         Display the requested message to the specified handle
;*
;*   INPUT:
;*      1)   AX = Number of the message to be displayed.
;*      2)   BX = Handle to be written to.
;*      3)   DH = Code indicating message class
;*
;*   OUTPUT:
;*      The message corresponding to the requested msg number will
;*      be written to the requested handle.
;*
;*   NORMAL EXIT:
;*      Message will be successfully written to requested handle.
;*
;*   ERROR EXIT:
;*      None.  Note that theoretically an error can be returned from
;*      SYSDISPMSG, but there is nothing that the application can do.
;*
;*   INTERNAL REFERENCES:
;*      System Display Message service routines
;*
;*   EXTERNAL REFERENCES:
;*      None
;*
;************************************************************
display_msg     proc    near            ;AN000;
        push    ds                      ;AN000; Save DS
        push    cs                      ;AN000; Substitution list segment
        pop     ds                      ;AN000;

        assume  ds:code

        mov     si,offset sublist       ;AN000; Substitution list offset
      ; mov     dh,-1                   ;AN000; Message class
                                        ; 1=DOS Extended error
                                        ; 2=DOS Parse error
                                        ; -1=Utility message
        mov     dl,0                    ;AN000;  DOS INT 21H function number to use for input
                                        ; 00H=No input, 01H=Keyboard input,
                                        ; 07H=Direct Console Input Without Echo,
                                        ; 08H=Console Input Without Echo, 0AH=Buffered Keyboard Input
        call    SYSDISPMSG              ;AN000; AX=Extended key value if wait for key
      ; jnc     disp_done               ;AN000; If CARRY SET then registers will contain extended error information
                                        ;       AX - Extended error Number
                                        ;       BH - Error Class
                                        ;       BL - Suggested action
                                        ;       CH - Locus
disp_done:                              ;AN000;
        pop     ds                      ;AN000; Restore DS

        assume  ds:nothing

        ret                             ;AN000;
display_msg     ENDP                    ;AN000;

        PAGE
;************************************************************
;*
;*   SUBROUTINE NAME:      parse
;*
;*   SUBROUTINE FUNCTION:
;*      Call the DOS PARSE Service Routines to process the command
;*      line. Search for valid switches (/N, /V, /I, and /C) and take
;*      appropriate action for each.  Extract the search string.
;*
;*   INPUT:        DS:SI points to string to parse
;*                 ES:DI parser parms
;*
;*   OUTPUT:       ES:DI points to filespec for text search
;*
;*   NORMAL EXIT:
;*
;*      If /V, /C, or /N entered, set appropriate flag.
;*      Save the search string.
;*
;*   ERROR EXIT:
;*
;*      If user enters any invalid parameter or switch, then this
;*      routine will display an error message and terminate with
;*      errorlevel 2.
;*
;************************************************************
EOL             equ    -1       ;AN000; Indicator for End-Of-Line
NOERROR         equ     0       ;AN000; Return Indicator for No Errors
SYNTAX          equ     9       ;AN000; Syntax error from parser

SWITCH          equ     3       ;AN000;
FILESPEC        equ     5       ;AN000;
QUOTED_STRING   equ     9       ;AN000;

parse   proc    near                                    ;AN000;
;--------------------------------------
; address of command line in DS:SI
;--------------------------------------
;------------------------------------------
;- Look for the search string and switches
;------------------------------------------
parse_loop:                                             ;
        mov     di,offset parms                         ; Address of parse control block at ES:DI
        xor     dx,dx                                   ; Reserved
        call    sysparse                                ; Parse parm at DS:SI
        cmp     ax,EOL                                  ; Are we at End Of Line ??
        jne     p_next                                  ; No eol found
        mov     cs:got_eol,TRUE                         ; no more filenames to get!
        cmp     cs:did_file,TRUE                        ; did we do a file already ?
	jne	end_parse       			; Yes, done here
	jmp	terminate                               ; otherwise finished

p_next:                                                 ; continue
        and     ax,ax                                   ; ;AN000; Was there an error?
        je      CONT2                                   ; No, continue processing

        mov     dh,2                                    ; Its a PARSE message
        call    display_and_die                         ;
CONT2:                                                  ; Something valid was entered
        cmp     cs:rb_type,QUOTED_STRING                ; Is it a quoted string ?
        je      its_a_quoted_string                     ; Yes, go process it
        cmp     cs:rb_type,FILESPEC                     ; Is it a filespec?
        jne     cont3                                   ;
        mov     di,cs:rb_value_lo                       ;  Look for another
        mov     cs:got_filename,TRUE                    ;  got a filename
        jmp     short end_parse                         ;  Look for another
cont3:
        cmp     cs:rb_type,SWITCH                       ; Is it a switch ?
	je	parse_loop				; pre_parse already checked
							; switches, don't need to here
        mov     ax,msg_inv_parm                         ; None of above, too bad
        mov     dh,2                                    ; message class
        call    display_and_die                         ; Tell the poor user and terminate

;-----------------------------
;- The search string was entered
;-----------------------------
its_a_quoted_string:                                    ; Found a quoted string
        cmp     cs:got_srch_str,TRUE                    ; Do we already have one?
        jne     its_ok                                  ; No, it's ok
        mov     ax,msg_inv_parm                         ; Yes, Invalid parm!
        mov     dh,2                                    ; message class
        call    display_and_die                         ; Tell user and die gracefully
its_ok:                                                 ;
        mov     di,cs:rb_value_lo                       ; Get pointer to it
        mov     bx,offset st_buffer                     ; save buffer offset
        call    get_length                              ; get string length
        mov     cs:st_length,ax                         ; save length
        mov     cs:got_srch_str,TRUE                    ; Indicate that we have it
        jmp     parse_loop                              ;

end_parse:                                              ; A filename should be next
        cmp     cs:got_srch_str,TRUE                    ; Do we already have one?
        je      rett                                    ;
        mov     ax,msg_required_missing                 ;
        mov     dh,-1                                   ; message class
        call    display_and_die                         ; Yes, tell the poor user and terminate
rett:                                                   ;
        ret                                             ;

parse   endp                                           


;------------------------------------
;-
;-  Procedure name: pre_parse
;-
;-  Purpose: parse for all switches now
;-      so that they can be applied for
;-      all filenames on command line.
;-
;-  INPUT: none
;-
;------------------------------------
        assume  ds:nothing

	public pre_parse
pre_parse proc near                     ;AN005;
        push    ax                      ;AN005;
        push    bx                      ;AN005;
        push    cx                      ;AN005;
        push    dx                      ;AN005;
        push    di                      ;AN005;
        push    si                      ;AN005;
        push    es                      ;AN005;
        push    ds                      ;AN005;
;
pp_loop:                                ;AN005;
	mov	di,offset parms1	;
        xor     dx,dx                   ;AN005; Reserved
        call    sysparse                ;AN005; Parse parm at DS:SI

        cmp     ax,EOL                  ;AN005; Are we at End Of Line ??
	jne	pp_chek_switch		;AN005; No eol found
	jmp	pp_end

pp_chek_switch:
        cmp     ax,SWITCH               ;AN005; invalid switch ?
        jne     pp_next                 ;AN005; no
; error
        mov     ax,msg_switch           ;AN005; Invalid switch
        mov     dh,2                    ;AN005; message class
        call    display_and_die         ;AN005; Yes, tell the poor user and terminate
pp_next:
        and     ax,ax                   ;AN007; ;AN005; Was there an error?
        jne     pp_loop                 ;AN005; No, continue processing

	cmp	cs:rb_type,QUOTED_STRING	;M01;get next parm if string
	je	pp_loop				;M01; or filespec
	cmp	cs:rb_type,FILESPEC		;M01
	je	pp_loop				;M01

        cmp     cs:rb_type,SWITCH       ;AN005; Is it a switch ?
        jne     pp_loop                 ;AN005;

; got a switch
        mov     bx,cs:rb_synonym        ;AN005; Get offset of switch entered
        cmp     bx,offset n_swch        ;AN005; Is it the /N switch?
        jne     pp_chek_v               ;AN005:  Yes, process it.
	or	cs:switches,LINE_NUMS	;AN005;  Set the corresponding flag
        jmp     pp_loop                 ;AN005;  Look for another
pp_chek_v:                              ;AN005;
	cmp	bx,offset v_swch	;AN005; Is it the /v switch?
	jne	pp_chek_i		;AN005:  Yes, process it.
	or	cs:switches,NOT_MATCHED ;AN005;  Set the corresponding flag
        jmp     pp_loop                 ;AN005;  Look for another
pp_chek_i:
	cmp	bx,offset i_swch	;AN005; Is it the /i switch?
	jne	pp_chek_c		;AN005:  Yes, process it.
	or	cs:switches,NO_CASE	;AN005;  Set the corresponding flag
        jmp     pp_loop                 ;AN005;  Look for another
pp_chek_c:                              ;AN005;
	cmp	bx,offset c_swch	;AN005; Is it the /c switch?
        jne     pp_chek_?               ;AN005:  Yes, process it.
	or	cs:switches,LINE_COUNT	;AN005;  Set the corresponding flag
        jmp     pp_loop                 ;AN005;  Look for another
pp_chek_?:
	cmp	bx,offset ?_swch
	jne	pp_error
	mov	cs:switches,HELP
	mov	ax, MSG_OPTIONS_FIRST   ; specify first of 3 option desc. msgs
	mov	bx,STDOUT               ; print msgs to standard out
	xor	cx,cx                   ; 0 substitution strings
	mov	dh,-1
@@:
	call	display_msg             ; print first option desc. msgs
	cmp	ax, MSG_OPTIONS_LAST	; last msg?
	je	@F			;  done if so
	inc	ax			; else bump msg number
	jmp	@B			;  and go do it
@@:
 	jmp	short  pp_end           ; get out

pp_error:                               ;AN005; None of the above (can we ever get here?)
        mov     ax,msg_switch           ;AN005; Invalid parameter
        mov     dh,2                    ;AN005; message class
        call    display_and_die         ;AN005; Yes, tell the poor user and terminate

pp_end:                                 ;AN005; A filename should be next
        pop     ds                      ;AN005;
        pop     es                      ;AN005;
        pop     si                      ;AN005;
        pop     di                      ;AN005;
        pop     dx                      ;AN005;
        pop     cx                      ;AN005;
        pop     bx                      ;AN005;
        pop     ax                      ;AN005;
;
        ret                             ;AN005;
pre_parse endp                          ;AN005;


;------------------------------------
;-
;-  Procedure name: prt_find
;-
;-  Purpose: When FIND is used as a filter,
;-      then display error messages with the
;-      prefix: "FIND: ".
;-
;-  INPUT: none
;-
;------------------------------------
        assume  ds:nothing

prt_find proc near                      ;AN005;
        cmp     cs:got_filename,TRUE    ;AN005; Check if should print "FIND:"
        je      prt_ret                 ;AN005;
        push    ax                      ;AN005; Save error
        push    dx                      ;AN005;
        mov     dh,-1                   ;AN005; Display FIND:
        mov     ax,msg_find             ;AN005;
        xor     cx,cx                   ;AN007; ;AN005; No substitution text
        mov     bx,STDERR               ;AN005; Sent to STD OUT
        call    display_msg             ;AN005; Display the message
        pop     dx                      ;AN005;
        pop     ax                      ;AN005; Restore error
prt_ret:
        ret                             ;AN005;
prt_find endp                           ;AN005;


;------------------------------------
;-
;-  Procedure name: display_and_die
;-
;-  Purpose: Called when the parser finds that
;-      required arguments were not entered
;-      from the command line.
;-
;-  INPUT: AX = Error number
;-
;------------------------------------
        assume  ds:nothing

display_and_die proc near
        call    prt_find                ;
        xor     cx,cx                   ; No substitution text
        mov     cs:errlevel,ERROR_EXIT  ; Error code for exit

        mov     bx,STDERR               ; Sent to STD OUT
        call    display_msg             ; Display the message
        call    terminate               ; and Terminate
        ret                             ;
display_and_die endp

;------------------------------------
;-
;-  Procedure name: get_length
;-
;-  Purpose: determine the length of a null
;-      ending string.
;-
;-  INPUT: ES:DI = string address
;-         ES:BX = save address (0=no save)
;-
;-  OUTPUT: AX   = length of string
;------------------------------------
        assume  ds:nothing

get_length      proc near
        push    di
        push    bx
        push    dx
        xor     dx,dx                   ;init string length
look_str:
        mov     al,es:[di]              ;get character
        or      bx,bx                   ;save it?
        jz      no_save
        mov     es:[bx],al              ;save character
        inc     bx                      ;save next character
no_save:                                ;AN007;
        and     al,al                   ;AN007; ;check for eol (asciiz string)
        je      done_look               ;if so, exit
        cmp     al,0dh                  ;AN005; check for eol (carriage return)
        je      done_look               ;AN005;
        inc     dx                      ;increment length
        inc     di                      ;look at next character
        jmp     look_str
done_look:
        xchg    ax,dx                   ;get length in AX
        pop     dx
        pop     bx
        pop     di
        ret
get_length      endp




;
;----- BUFFER AREA --------
st_length dw    0                       ;String argument length
st_buffer db    st_buf_size dup(?)      ;String argument buffer

file_name_len dw 0                      ;File name length
file_name_buf dw 0                      ;File name buffer offset

buffer        db buffer_size+2 dup(?)   ;file data buffer

include msgdcl.inc

code    ends

;--------------------------
;---   STACK SEGMENT    ---
;--------------------------
stack   segment para stack 'STACK'
        dw      (362 - 80h) +64 dup(?,?)    ;(362 - 80h)  == New - old IBM ROM
stack_top equ   $
stack   ends

        end     start
