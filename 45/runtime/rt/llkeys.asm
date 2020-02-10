	TITLE	LLKEYS - Keyboard support and mapping routines
;***
; LLKEYS.ASM - keyboard support and mapping routines
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
;
;******************************************************************************

	INCLUDE switch.inc	;switch file [new]
	INCLUDE rmacros.inc	;useseg macro

	USESEG	RT_TEXT 	;core runtime segment
	USESEG	_DATA		
	USESEG	CONST		
	USESEG	_BSS		

	INCLUDE seg.inc 	;segment definitions
	INCLUDE idmac.inc	;internal debug macros
	INCLUDE oscalls.inc	;Dos 5 structures
	INCLUDE ibmunv.inc
	INCLUDE intmac.inc
	INCLUDE const.inc	

	SUBTTL	local constant definitions
	page

	HidScn1		= 0E0H	; hidden scan code for 101-key keyboard
	HidScn2		= 0F0H	; hidden scan code for 101-key keyboard

	CharSlash	= 2FH	; character code for keypad /
	CharEnter	= 0DH	; character code for keypad Enter
	CharCTRLEnter	= 0AH	; character code for CTRL keypad Enter

	ScanSlash	= 35H	; scan code for keypad /
	ScanEnter	= 1CH	; scan code for keypad Enter



	SUBTTL	data definitions
	page			


sBegin	CONST			

PAGE

;List of Microsoft Key codes and their significance (all key codes 
;are in Hex)
;
;KeyCode(Hex)			Remarks
;________________________________________________________________________
;
;00		Null (but printable if a font exists)
;
;01-0C		Printable 1-byte characters
;
;0D		Carriage Return
;
;0E-0F		Printable 1-byte characters
;
;20-7E		ASCII characters
;
;7F		DELETE character
;
;80		First byte of 2-byte character set
;		Softkey implementation
;  8000-801F	**Unimplemented**
;  8020-803F	Function Keys (mapped by INPUT routines)
;  8040		**Unimplemented**
;  8041-805A	Super-shift Keys
;  805B-807E	**Unimplemented**
;  8080		printable character 80 (if a font exists)
;			Font is 1 byte only
;  8081-80FC	**Unimplemented**
;
;81-9F		First byte of 2-byte character set
;  40-FC		Second byte of 2-byte character set (7F is illegal)
;
;A0-DF		Printable 1-byte character set
;
;E0-FC		First byte of 2-byte character set
;  40-FC		Second byte of 2-byte character set (7F is illegal)
;
;FE		First byte of 3-byte character set
;  0000-FFFF	Second and third bytes of a 3-byte character
;		mapped to 1- or 2- byte character by INPUT routines
;
;FF		First byte of 2-byte character set
;  00-0F	Editor control characters (as input keys)
;  10		Editor control key ( as input key)
;		Printable character FE, if font exists (as print code)
;			Font is 1 byte only
;  FF11-FFFE	Editor control characters (as input keys)
;  FFFF 	Editor control key ( as input key)
;		Printable character FF, (font is space) as print code
;			Font is 1 byte only
;
;Note:
;	**Unimplemented** key codes are also valid but currently they have no
;	assigned function.
;
PAGE

FKPARM	LABEL	WORD
FKCCNT	DB	6		;# of chars/function key to display
FKDCNT	DB	10D		;# of function keys to display
FKNUM	DB	1		;# of first function key to display

sEnd	CONST			

sBegin	_BSS			

	externB	b$RcoFlg	; flag for 101-key keyboard

	externB b$ScrWidth
	externB b$BiosMode


sEnd	_BSS			

PAGE

sBegin	RT_TEXT 		
	assumes CS,RT_TEXT	



; Moved FAR with revision [18].
;	Let function-key mapping table have entries for 0-13 & 28,29,30, & 31

FKYMAP_TABLE	LABEL	BYTE	; Mapped to

	DB	0		; Null (no mapping)
	DB	1		; No special mapping
	DB	2		; No special mapping
	DB	3		; No special mapping
	DB	4		; No special mapping
	DB	5		; No special mapping
	DB	6		; No special mapping
	DB	14		; music bar character
	DB	254		; Small square block char
	DB	26		; Right arrow (->)
	DB	27		; Left arrow (<-)
	DB	127		; House char
	DB	22		; Lower-half block char
	DB	27		; Left arrow (<-)
	DB	16		; Right wedge char
	DB	17		; Lefht wedge char
	DB	24		; Up arrow char
	DB	25		; Down arrow char
;	B$PRTMAP uses this table to determine 1- or 2-byte code
;	The table entry's MSB is set (=1) if it is a 2-byte code
;	else it is cleared (value < 128)
;	This table contains translation bytes for ascii 0-31

PRTMAP_TABLE	LABEL	BYTE	; Mapping done is as follows:

	DB	0D		; No special mapping
	DB	1D		; No special mapping
	DB	2D		; No special mapping
	DB	3D		; No special mapping
	DB	4D		; No special mapping
	DB	5D		; No special mapping
	DB	6D		; No special mapping
	DB	080H+07H	; Backspace
	DB	8D		; No special mapping
	DB	080H+09H	; TAB
	DB	080H+0AH	; LineFeed
	DB	080H+0BH	; Home cursor 
	DB	080H+0CH	; Clear scroll window
	DB	080H+0DH	; Carriage Return
	DB	14D		; No special mapping
	DB	15D		; No special mapping
	DB	16D		; No special mapping
	DB	17D		; No special mapping
	DB	18D		; No special mapping
	DB	19D		; No special mapping
	DB	20D		; No special mapping
	DB	21D		; No special mapping
	DB	22D		; No special mapping
	DB	23D		; No special mapping
	DB	24D		; No special mapping
	DB	25D		; No special mapping
	DB	26D		; No special mapping
	DB	27D		; No special mapping
	DB	080H+1CH	; Cursor Right
	DB	080H+1DH	; Cursor Left
	DB	080H+1EH	; Cursor Up
	DB	080H+1FH	; Cursor Down

PAGE

;	B$EDTMAP uses a table to determine 1- or 2-byte code
;	The table entry's MSB is set (=1) if it is a 2-byte code
;	else it is cleared (value < 128)
;	This table contains translation bytes for ascii 0-31
	
EDTMAP_TABLE	LABEL	BYTE	

	DB	080H+10H	
	DB	0001H		
	DB	080H+02H	
	DB	080H+03H	
	DB	0004H		
	DB	080H+05H	
	DB	080H+06H	
	DB	080H+07H	
	DB	080H+08H	
	DB	080H+09H	
	DB	080H+0AH	
	DB	080H+0BH	
	DB	080H+0CH	
	DB	080H+0DH	
	DB	080H+0EH	
	DB	000FH		
	DB	0010H		
	DB	0011H		
	DB	080H+12H	
	DB	0013H		
	DB	080H+14H	
	DB	080H+15H	
	DB	0016H		
	DB	0017H		
	DB	0018H		
	DB	0019H		
	DB	001AH		
	DB	080H+15H	
	DB	080H+1CH	
	DB	080H+1DH	
	DB	080H+1EH	
	DB	080H+1FH	

PAGE

;KEYCODE TABLE FORMAT : <raw-code>,<new-AL>	;<new-AH>
;
;	Remarks on the KEYINP_TABLE table arrangement
;
;	Currently, the arrangement is as follows:
; <raw-code>	 = All sorts of values found (i.e. cannot make any assumptions)
; <new-AL  >	0 - 1F	20 - 7E	7F	80	FE	FF
; <new-AH  >	  FF	  80	FF	80	Scan	FF
; Byte-category	  2	  1	2	2	3	2
;
;It can be seen, that by arranging the table in such a fashion that <new-AL>
; is ordered so that the returned <new-AH> would be 80,FF,Scan
; we eliminate the need to store <new-AH> in the table. Of course, this would
; need some comparisons once a match has been found in this table but the
; overall search is improved with the expected (or by design) reduction in
; code size.
;
;	In the following table the third entry is commented out
;	The original table is ordered as suggested above
;	The intervening labels identify various <new-AH> types
;	Within each type, the table is ordered by <new-AL> (ascending)
;	To assist the search better (less code), the table has been organized
;	in this order
;	<new-AL>, <scan>	;<new-AH>


KEYINP_TABLE	LABEL	WORD		; Word access

;	<new-AL>, <scan>	;<new-AH>

	DB	032D,059D	;128D	;function keys 1
	DB	033D,060D	;128D	;    2
	DB	034D,061D	;128D	;    3
	DB	035D,062D	;128D	;    4
	DB	036D,063D	;128D	;    5
	DB	037D,064D	;128D	;    6
	DB	038D,065D	;128D	;    7
	DB	039D,066D	;128D	;    8
	DB	040D,067D	;128D	;    9
	DB	041D,068D	;128D	;   10
	DB	042D,133D	;128D	; F11
	DB	043D,134D	;128D	; F12
	DB	044D,135D	;128D	; Shift-F11
	DB	045D,136D	;128D	; Shift-F12
	DB	046D,137D	;128D	; Ctrl-F11
	DB	047D,138D	;128D	; Ctrl-F12
	DB	048D,139D	;128D	; Alt-F11
	DB	049D,140D	;128D	; Alt-F12
	DB	065D,030D	;128D	;super shift A
	DB	066D,048D	;128D	;    B
	DB	067D,046D	;128D	;    C
	DB	068D,032D	;128D	;    D
	DB	069D,018D	;128D	;    E
	DB	070D,033D	;128D	;    F
	DB	071D,034D	;128D	;    G
	DB	072D,035D	;128D	;    H
	DB	073D,023D	;128D	;    I
	DB	074D,036D	;128D	;    J
	DB	075D,037D	;128D	;    K
	DB	076D,038D	;128D	;    L
	DB	077D,050D	;128D	;    M
	DB	078D,049D	;128D	;    N
	DB	079D,024D	;128D	;    O
	DB	080D,025D	;128D	;    P
	DB	081D,016D	;128D	;    Q
	DB	082D,019D	;128D	;    R
	DB	083D,031D	;128D	;    S
	DB	084D,020D	;128D	;    T
	DB	085D,022D	;128D	;    U
	DB	086D,047D	;128D	;    V
	DB	087D,017D	;128D	;    W
	DB	088D,045D	;128D	;    X
	DB	089D,021D	;128D	;    Y
	DB	090D,044D	;128D	;    Z
	DB	128D,253D	;128D	;special M.S. code uses 128D
KEY_128	EQU	THIS WORD + 2		; 2-Byte and <new-AH> = 80H
					; Note that '+2' is used because
					; LODSW increments SI by 2 and
					; 'jump below' condition is used
KEY_128_LEN	EQU	(KEY_128 - KEYINP_TABLE)/2 ; No. of entries for 128

KEY_255_START	LABEL	WORD		; From here it is '255' code
	DB	002D,115D	;255D	;previous word
	DB	005D,117D	;255D	;erase --> EOLN
	DB	006D,116D	;255D	;next word
	DB	011D,071D	;255D	;home cursor
	DB	012D,119D	;255D	;clear screen and home cursor
	DB	014D,079D	;255D	;--> EOLN
	DB	016D,254D	;255D	;special since M.S. code uses 254D
	DB	018D,082D	;255D	;toggle insert mode
	DB	026D,118D	;255D	;clear to end of window
	DB	028D,077D	;255D	;right cursor
	DB	029D,075D	;255D	;left cursor
	DB	030D,072D	;255D	;up cursor
	DB	031D,080D	;255D	;down cursor
	DB	127D,083D	;255D	;delete char under cursor
	DB	255D,255D	;255D	;special since M.S. code uses 255D
KEY_255	EQU	THIS WORD + 2		; end of '255' codes
					; Note that '+2' is used because
					; LODSW increments SI by 2 and
					; 'jump below' condition is used
KEY_255_LEN	EQU	(KEY_255-KEY_255_START)/2 ; No. of entries for 255

	DB	254D,003D	;003D	;unimplemented
	DB	254D,015D	;015D	;	"
	DB	254D,073D	;073D	;	"
	DB	254D,081D	;081D	;	"
	DB	254D,114D	;114D	;	"
	DB	254D,120D	;120D	;	"
	DB	254D,121D	;121D	;	"
	DB	254D,122D	;122D	;	"
	DB	254D,123D	;123D	;	"
	DB	254D,124D	;124D	;	"
	DB	254D,125D	;125D	;	"
	DB	254D,126D	;126D	;	"
	DB	254D,127D	;127D	;	"
	DB	254D,128D	;128D	;	"
	DB	254D,129D	;129D	;	"
	DB	254D,130D	;130D	;	"
	DB	254D,131D	;131D	;	"
	DB	254D,132D	;132D	;	"
	DB	000D,000D	;000D	;end of table specifier
KEYINP_TABLE_END	LABEL	WORD

PAGE

;***
;B$FKYFMT - return format of Function Key display line
;OEM-interface routine
;
;Purpose:
;	This routine supports the function key display line feature by
;	providing the function key display format.  The format is made
;	up of three parts as follows:
;
;		FKCCNT - 1 byte count of the number of bytes
;			 to be displayed for each function key
;			 description.
;		FKDCNT - 1 byte count of the number of function
;			 keys to be displayed.
;		FKNUM  - 1 byte number of the first key to be
;			 displayed.
;
;	The format must be in the preceding order since the address
;	of the format descriptor is what will be returned.  The
;	current screen width must be sufficient to display this format.
;
;	The function key line is displayed as FKDCNT iterations of
;	a single digit key number, FKCCNT characters from the function
;	key expansion table, and a space.  Thus, there will be FKDCNT
;	fields of size (1+FKCCNT+1).  If two digits are necessary to
;	display the function key number, then FKCCNT-1 function key
;	characters are displayed.  If there are fewer than FKDCNT+
;	FKNUM-1 keys to display, then the display will wrap around to
;	the first function key, like thus:
;
;	     8 xxx 9 xxx 10 xx 1 xxx 2 xxx
;
;	for FKCCNT = 3, FKDCNT = 5, FKNUM = 8
;
;	If you are supporting double byte characters, take note that this
;	routine takes the number of bytes of the function key name, not
;	the number of characters.  When it is printed, if the last
;	character would be the first of a two byte sequence, a space
;	will be used instead.
;
;Entry:
;	none
;
;Exit:
;	[BX] = address of format descriptor
;		->	FKCCNT
;			FKDCNT
;			FKNUM
;
;Uses:
;	Per Convention.
;
;Preserves:
;	DX
;
;Modifies:
;	None.
;*****
;ALGORITHM:
;
;	If the width = 40 then count = 5 else (width = 80), count = 10
;	Hence it can just divide by 8 to achieve the result
;

;	Actually this routine need only to return the format descriptor address
;	as the number of function keys can be set by B$SCRSTT when the mode
;	changes. It is assumed, of course, that the user would not want to
;	change the number of softkeys displayed without changing the screen
;	dimension. In this case, the first 4 lines of code can be removed.

cProc	B$FKYFMT,<PUBLIC,NEAR>	
cBegin				

	MOV	BX,OFFSET DGROUP:FKPARM ;get address of format descriptor
	MOV	AL,b$ScrWidth	; Get the current screen width
	MOV	CL,3		; Setup to divide by 8
	SHR	AL,CL		; AL = 5/10
	MOV	FKDCNT,AL	; Store it in the structure

cEnd				; End of FKYFMT

;***
;B$FKYMAP - map character to printable form for Function Key display
;OEM-interface routine
;
;Purpose:
;	This routine is is called to map characters for the function
;	key display. This routine allows certain key codes to be replaced
;	by a different key code.  For example, CHR$(13) might be mapped
;	to the character '<'.
;
;	This mapping should be done for any character that is unprintable
;	or that would cause Screen Editor functions to be performed.  Note
;	that this routine must always return a legal key code, which should
;	be a printable graphic character.  No further mapping of the
;	character will take place before it is printed.
;
;Entry:
;	[AX] = character
;	PSW.C set indicates a two byte character
;
;Exit:
;	[AX] = character
;	PSW.C set indicates a two byte character
;
;Uses:
;	Per convention
;
;Preserves:
;	CX, DX
;
;Exceptions:
;	None.
;****
;	ALGORITHM:
;	If the input character is two-byte then return as it is
;	else
;		if input range is 0-13 or 28-31 (inclusive ranges)
;		then map it using FKYMAP_TABLE
;		else return input character itself

cProc	B$FKYMAP,<PUBLIC,NEAR>	
cBegin				

	JC	FKYRET		; If char = 2-byte then exit
	CMP	AL,31		; Is character in control range?
	JA	FKYRET		; NO, can be printed as is
	CMP	AL,13		; Below 14, use the mapper
	JBE	FKY_XLAT	; Brif need to translate
	CMP	AL,28		; Is 13 < char < 28?
	JB	FKY_RET1	; Brif no mapping
	SUB	AL,(28-14)	; Bring it in range for mapping

FKY_XLAT:			
	CBW			; AL < 31 ; Therefore AH = 0
	XCHG	AX,BX		; BX need to be saved
	MOV	BL,CS:FKYMAP_TABLE[BX]	; Get the substitute char
	XCHG	AX,BX		; Restore back BX

FKY_RET1:			; Clear carry to indicate 1-byte
	CLC			
FKYRET:				; Exit...

cEnd				; End of FKYMAP



;***
;B$KEYINP - input key from keyboard
;OEM-interface routine
;
;Purpose:
;	This routine gets a key from the keyboard and translates it
;	to a code from the Microsoft Standard Keyboard code set.
;	This routine will wait until a character is ready if PSW.Z
;	is set, else it will return immediately with a flag if there
;	was no available character.
;
;	The host keyboard support may return a multiple code sequence
;	for a given key depression rather than a single code.  This
;	routine must collect these codes and return a single Microsoft
;	Standard Keyboard code.
;
;	A goal in key mapping is to allow special keys (such as
;	<backspace>) to cause the same action as a key in the control
;	code range (such as ^H).  Yet it is still desirable to return
;	unique key codes to the user for the INKEY$ function.  This
;	is accomplished by having multiple mapping routines.
;
;	This is one of many different routines that map characters from
;	a machine specific form to and from the MicroSoft Key Codes.
;	These codes are used within BASIC for all functions, and are
;	only translated only on input, for INPUT$, and for INKEY$.
;	This is the only routine which will get characters from the
;	hardware, so it is the only one that has to map from the
;	machine specific format to the MicroSoft format.  The following
;	routines are used to map from MicroSoft format to a different
;	format for printing or for presenting to the user:
;
;		B$EDTMAP - map a character during Screen Editing
;		B$FKYMAP - map a character for the Function Key display
;		B$INFMAP - map a character for INPUT$
;		B$INKMAP - map a character for INKEY$
;		B$PRTMAP - map a character for printing
;
;	See the documentation with each function for the specifics of
;	the particular mapping.  Note that all characters that are sent
;	to OEM functions for displaying, printing, device I/O, etc are
;	Microsoft Key Codes.
;
;	Note that all characters that are presented to the user go
;	through mapping twice.	First they go through B$KEYINP to
;	convert them to Microsoft Standard Key Codes, then through
;	either B$INFMAP, B$INKMAP, or B$EDTMAP before being
;	presented to the user.
;
;Interim Character Support Considerations:
;
;	All entry and exit conditions are unchanged except for two-byte
;	characters (PSW.C set).  Not, the value of DX determines
;	whether the character is interim or final.  If DX is 0, the
;	character in AX is treated as an interim character.
;
;
;MicroSoft Key Codes:
;
;	KeyCode(Hex)		       Remarks
;	_______________________________________________________________
;
;	00	       Null (but printable if a font exists)
;	01-0C	       Printable 1-byte characters
;	0D	       Carriage Return
;	0E-0F	       Printable 1-byte characters
;	20-7E	       ASCII characters
;	7F	       DELETE character
;
;	80	       First byte of 2-byte character set
;	    00-1F      Unimplemented
;	    20-3F      Function Keys (mapped by INPUT routines)
;	    40	       Unimplemented
;	    41-5A      Super-shift Keys
;	    5B-7E      Unimplemented
;	    80	       printable character 80 (if a font exists)
;	    81-FC      Unimplemented
;
;	81-9F	       First byte of 2-byte character set
;	    40-FC      Second byte of 2-byte character set (7F is illegal)
;
;	A0-DF	       Printable 1-byte character set
;
;	E0-FC	       First byte of 2-byte character set
;	    40-FC      Second byte of 2-byte character set (7F is illegal)
;
;	FE	       First byte of 3-byte character set
;	    0000-FFFF  Second and third bytes of a 3-byte character
;		       mapped to 1- or 2- byte character by INPUT routines
;
;	FF	       First byte of 2-byte character set (Editor Control)
;	    00-0F      Editor control characters (as input keys)
;	    10	       Editor control key ( as input key)
;		       Printable character FE (as print code)
;	    11-FE      Editor control characters (as input keys)
;	    FF	       Editor control key ( as input key)
;		       Printable character FF, (font is space) as print code
;
;	Note:
;	    Unimplemented key codes are also valid but currently they have no
;	    assigned function.
;
;
;Entry:
;	PSW.Z  set if B$KEYINP may wait for a key
;
;Exit:
;	[AX] = Microsoft Standard Keyboard code
;	[DX] = bytes 2 and 3 of three byte codes
;	     = 0 to signal a non-interim character (when PSW.C set)
;	     = 1 to signal a interim character (when PSW.C set)
;	PSW.C = set indicates a 2 byte character
;	PSW.C = reset indicates a 1 or 3 byte character code
;	PSW.Z = set if no keys have been typed
;		(the code is in AL in the 1 byte case, AX in the 2 byte
;		 case, and AL and DX in the 3 byte case)
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX
;
;Exceptions:
;	None.
;****
;	For this implementation all control range codes and printable
;	characters will be passed through as one byte keycodes except
;	for the special codes 128D, 254D, and 255D (&H 80, FE, FF)
;	which will be mapped as 2-byte codes.  The IBM extended
;	character set will be mapped as two or three byte codes
;	in the Microsoft Standard Keyboard code set.
;
;ALGORITHM:
;	if PSW.Z is set then
;	    wait for a character to be typed
;	    if IBM 2-byte char then
;		if char is control-break then make it MS control break
;		  else
;		    if char is shifted function key then
;			make it M.S. three byte code
;		      else
;			map to M.S. code
;	      else
;		if char is 128D or 254D or 255D then map to M.S. code
;	else
;	     set PSW.Z and return
;#****

cProc	B$KEYINP,<PUBLIC,NEAR>,<SI,BX>	
cBegin				
GET_A_KEY:
	PUSHF			;save flags


	KYBIO	kPollKeyBoard	;loop while character

	JNZ	CHECK_KEY	; Key is available
NO_KEY:				
	POPF			; Get back flags
	JZ	GET_A_KEY	; Wait until a key is available
	XOR	AH,AH		; Indicate no key is available
	JMP	SHORT KEY_RET	; & Exit...

CHECK_KEY:			; Process the key


	KYBIO	kReadKeyBoard	;get key from buffer


	MOV	SI,RT_TEXTOFFSET KEYINP_TABLE; Get scancode table
	CLD			; Set for autoincrement
	OR	AL,AL		;is it a two byte code?
	JNZ	Chk_One_Byte	; br. if not
	OR	AH,AH		; Check for BREAK key
	JNZ	Supershift_check	; Brif may be supershift key
	OR	AX,0FF03H	; Set BREAK key code
	STC			; ZF = 0 ; CF = 1 (2-BYTE CODE)
	JMP	SHORT KEY_EXIT	; & Exit...

Chk_One_Byte:
	TEST	b$RcoFlg,0FFH	; is ronco installed ?
	JZ	Single_Byte	; Brif not
	CMP	AL,HidScn1	; is hidden char code ?
	JE	SuperShift	; Brif yes
	CMP	AL,HidScn2	; is hidden char code ?
	JE	SuperShift	; Brif yes

;	     CMP     AH,HidScn1      ; KEYPAD / & ENTER need special care
;	     JNE     Single_Byte     ; Brif not, this is a normal character

;	The following comparisons are done only for the RONCO KBD. Still it
;	has to be ascertained as to why these are done as they seem to serve
;	no purpose (judged by the existing code)


;	     CMP     AL,CharSlash    ; is Keypad / ?
;	     MOV     AH,ScanSlash    ; make it normal
;	     JE      Single_Byte     ; Brif yes
;	     CMP     AL,CharEnter    ; is Keypad enter ?
;	     MOV     AH,ScanEnter    ; make it normal
;	     JE      Single_Byte     ; Brif yes
;	     CMP     AL,CharCTRLEnter; is keypad CTRL enter ?
;	JMP	SHORT Single_Byte ; Brif not, a normal character

Single_Byte:
				; If al = 253 then return 3-byt code
	MOV	AH,0		; Assume 1-byte
	CMP	AL,253D 	;is it special char?
	JE	KEY_EXIT	; br. if so - Exit
	JA	KEY_MAP		; Map it to 3-byte code
	CMP	AL,128D 	;is it special char?
	JNE	KEY_EXIT2	; br. if 1-byte char : ZF = CF = 0
	MOV	AL,253D		; Scan code for ALT-128
	JMP	SHORT KEY_MAP	; Map to 8080H

SuperShift:			
	XOR	AL,AL		; make it normal

SuperShift_Check:		
	XCHG	AL,AH		; Copy scan code
				; AH = 0

	CMP	AL,84D		;lower bound of shifted fctn. key?
	JB	KEY_MAP		;br. if below
	CMP	AL,114D 	; upper bound of shifted fctn. key?
	JB	KEY_3_BYTE	; indicate 3-byte M.S. code

KEY_MAP:
	XCHG	AX,BX		; BL = Scan code of key
				; BH = 0 at this point always
KEY_LOOP:
	LODS	KEYINP_TABLE	; [AH] = next <raw-code> entry in table
	TEST	AX,AX		; test for last table entry
	JZ	NO_KEY		; End of table - try again
	CMP	AH,BL		; look for match on scan code
	JNE	KEY_LOOP	; loop back if not found

KEY_FOUND:
	MOV	AH,80H		; Assume AH will have 80H
	CMP	SI,RT_TEXTOFFSET KEY_128; Is it two-byte code?
	JB	KEY_EXIT	; Exit 2-byte code CF = 1 already
	MOV	AH,255D		
	CMP	SI,RT_TEXTOFFSET KEY_255; Is it 2-byte with AH=0FFH
	JB	KEY_EXIT	; Set AH = 0FFH and return

	XCHG	AX,BX		; Get scan code in AL
				; AH = 0
KEY_3_BYTE:			; It is a 3-byte code
				; When control comes here, AH = 0 always
	MOV	DX,0FEH		; 254 indicates 3-byte code
	XCHG	AX,DX		; DL is the keycode and AH = 0

KEY_EXIT2:			; Set ZF & CF to zero
	SAHF			; CF = ZF = 0 at this point

KEY_EXIT:			
	POP	BX		; Discard the flags on stack

KEY_RET:			; Generic Exit...

cEnd				; End of B$KEYINP


;***
;B$CTRLMAP
;
;PURPOSE:
;
; B$CTRLMAP will map the character in AL according to the table
; pointed to by BX. However, if the input character = 0FFFFH, then 
; the returned character will be requested to be ignored in Graphics
; modes 4,5, & 6.
;
;ENTRY:
;	[BX]	= Character-Translation-Table-Address
;	[AX]	= Character to be translated
;
;EXIT:
;	[AX]	= Translated code 
;	PSW.C	= Set to indicate 2-byte char
;	PSW.Z	= Set to indicate ignore the char
;
;MODIFIED:
;	NONE
;
;****

cProc	B$CTRLMAP,<NEAR>	; Map if it is a control character
cBegin				

	CMP	AL,255D		; See if FFh
	JNE	MAP_IT		; Brif it is not 0FFFFH
				; Now if the char is 0FFFFh, do not
				; allow this in Graphics modes
	MOV	AH,b$BiosMode	; Get BIOS mode value
	CMP	AH,4		; If 3 < AH < 6 then graphics
	JB	MAP255		; Brif Text
	CMP	AH,7		; Check for upper limit
	CMC			; CF = 0 if Graphics
MAP255:				; Set AH accordingly
	SBB	AH,AH		; AH = 0/-1
	JMP	SHORT MAP_RET	; That's it.

MAP_IT:
	CMP	AL,31		; Is 0 <= AL < 32
	JA	MAP_RET		; Brif not a control char

MAP_XLAT:			; Translate control char
	XLAT	PRTMAP_TABLE	; Do it now
	CBW			; AH = -1 if 2-byte char
	AND	AL,07FH		; Knock off MSB
				; AH  is either 0 or 0FFH
	CMP	AH,80H		; This sets CF if AH = 0
				; but ZF = 0 always
	CMC			; Reverse the sense of CF for reporting

MAP_RET:

cEnd				; End of B$CTRLMAP

;***
;B$PRTMAP - Map a character into a form for printing
;OEM-interface routine
;
;Purpose:
;	This routine is called to translate character codes from the
;	Microsoft Standard Keyboard code set to either:
;
;	1. another code from the Microsoft Standard Keyboard set for
;	   printing.
;	2. a function code from the Microsoft Standard Keyboard set.
;	3. a request to ignore the character (it has no printable form)
;
;	B$PRTMAP is called as a result of a requests to print characters.
;	This routine must filter out all characters which are non-printable.
;	If a printable character code is returned, then it must not
;	cause any BIOS/VIO functions to be executed when it is sent to
;	either the screen or a device.	These codes must either be ignored
;	or mapped to a Microsoft Standard Keyboard Function code.  Examples
;	of characters that have to be mapped are Function Keys and many
;	of the control characters.
;
;	This routine may return the code for any character, allowing
;	characters to be moved around in the character set. For more
;	information on character mapping, see the documentation for the
;	function B$KEYINP.
;
;	Note:  If you want to use PRINT for escape sequences, B$PRTMAP
;	must not map the ESC character &H3F to &HFF3F.
;
;Entry:
;	[AX] = Microsoft Standard Keyboard code (1 or 2 byte only)
;	PSW.C = set indicates a 2-byte char
;
;Exit:
;	[AX] = code to be printed or function to be executed
;	PSW.Z = set indicates to ignore the current char - AX don't care
;	PSW.Z = reset indicates to process the char
;	PSW.C = set indicates a two byte character
;	PSW.C = reset indicates a one byte character ([AH] = 0)
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****
;ALGORITHM:
;	if 2-byte char then
;	    if char is supershift or soft key then ignore
;	      else process char
;	  else
;	    if char is 255D then
;		if mode is graphics then ignore char
;		  else process as 2-byte 255D,255D
;	      else
;		if control char then map (1 or 2 bytes)
;		process char
;	NOTE:	If ZF = 1 (ignore char) then AX is undefined. Currently, the
;		callers (REDMAP, $SCREEN_OUT in IOTTY.ASM)
;		ignore AX under this condition.


cProc	B$PRTMAP,<PUBLIC,NEAR>	
cBegin				

	JNC	PRT_1_BYTE	; Process 1-byte code

PRT_2_BYTE:			; Process 2-byte chars
	CMP	AH,255D		; See if ctl char
	JE	PRT_1_BYTE	; Nope--1-byte char
	CMP	AH,80H		; If so, then it is a 1-byte char
	JE	PRT_EXIT	; Brif true. CF = 0
	STC			; Indicate 2-byte char
	JMP	SHORT PRT_EXIT	
				; CF = 0 implies 2-byte code
PRT_1_BYTE:			; Check for control char range

	PUSH	BX		; Save BX
	MOV	BX,RT_TEXTOFFSET PRTMAP_TABLE	; Translation table
	cCALL	B$CTRLMAP	; Map the control char
	POP	BX		; Restore BX
PRT_EXIT:			; Generic exit...

cEnd				; End of B$PRTMAP


;***
;B$EDTMAP - map a character into a form for displaying
;OEM-interface routine
;
;Purpose:
;	This routine is called to translate character codes from the
;	Microsoft Standard Keyboard code set to either:
;	1. another code from the MIcrosoft Standard Keyboard set.
;	2. a function code from the Microsoft Standard Keyboard set.
;	3. a request to ignore the character.
;
;	B$EDTMAP is called as a result of Screen editor requests to echo
;	characters.  It may be noted that Screen Editor screen control
;	functions will be active only if this routine exits with the
;	screen control function in AX, PSW.Z reset, and PSW.C set (indicating
;	to process the associated editor function).
;
;	This routine allows the OEM to map characters to Screen Editor
;	functions and to change character codes.  Screen Editor functions
;	are performed when the associated control characters are printed.
;	These control codes must be mapped to the appropriate function code
;	by this routine.  For example, if a carriage return should be
;	mapped to FF0D (Carriage Return Function).  Any character that
;	is printable is printed without any further mapping.
;
;Entry:
;	[AX] = Microsoft Standard Keyboard code (1 or 2 byte only)
;	PSW.C = set indicates a 2-byte char
;
;Exit:
;	[AX] = code to be printed or function to be executed
;	PSW.Z = set indicates to ignore the current char - AX is don't care
;	PSW.Z = reset indicates to process the char
;	PSW.C = set indicates  a 2-byte char
;	PSW.C = reset indicates a single byte char in AL
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	none
;****


cProc	B$EDTMAP,<PUBLIC,NEAR>		
cBegin					

;	The processing of edit mappint is almost identical to that of
;	print map except that the lookup table is different. Hence setup
;	BX to point to EDTMAP_TABLE and use B$PRTMAP control-char processing
;	code.

	JNC	EDT_1_BYTE		; CF =0 means it is 1-byte char
					; check graphics context also
EDT_2_BYTE:				; If special (AX = 0FFFFH) then
	CMP	AX,-1			
	JNE	EDT_EXIT		; Brif not exit

EDT_1_BYTE:				; 1-Byte or 2-byte 0FFFFH char
	PUSH	BX			; Save BX, B$PRTMAP will unsave it
	MOV	BX,RT_TEXTOFFSET EDTMAP_TABLE; Lookup table address
	cCALL	B$CTRLMAP		; Process 1-byte chars
	POP	BX			; Restore BX

EDT_EXIT:				; Generic exit...

cEnd					; End of B$EDTMAP


;***
;B$INFMAP - map character for INPUT$
;OEM-interface routine
;
;Purpose:
;	This routine is called to translate character codes from the
;	Microsoft Standard Keyboard set to an OEM specific key code set.
;	B$INFMAP is called during the processing of the INPUT$ function,
;	when the characters are coming from the keyboard.
;	Since there may be differences in the code values returned
;	by the INPUT$ and INKEY$ functions, two separate support
;	routines are required for these functions.  Codes returned
;	by B$INFMAP may be one or two byte codes.
;
;	The way that a string for INPUT$ from KYBD: is created
;	is by asking for characters from B$KEYINP, passing them
;	to B$INFMAP, and storing the results in the string.
;
;Entry:
;	[AX] = Microsoft Standard Keyboard code
;	[DX] = bytes 2 and 3 of three byte codes
;	PSW.C = set indicates a 2 byte character
;	PSW.C = reset indicates a 1 or 3 byte character code
;		(the code is in AL in the 1 byte case, AX in the 2 byte
;		 case, and AL and DX in the 3 byte case)
;
;Exit:
;	[AX] = OEM code to be returned by INPUT$
;	PSW.C = set indicates a 2 byte character
;	PSW.C = reset indicates a 1 byte character code in AL
;	PSW.Z = set indicates that there is no OEM code to  associate
;		with the Microsoft Code - AX is don't care
;
;Uses:
;	Per Convention
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****

cProc	B$INFMAP,<PUBLIC,NEAR>
cBegin					

	JC	INF_2_CHK		; If CF = 1 then it is 2-byte
	CMP	AL,254			; Check if 3-byte char
	JNE	INF_EXIT		; Br if not a 3-byte char

; Return zero for 3-byte character codes. The following comparisons will
; ALL FAIL and hence the return code will be zero in AX

INF_2_CHK:				; Process 1-byte chars
	CMP	AX,-1			; If AX is special 2-byte
	JE	INF_EXIT		; Then treat it as 1-byte only
	CMP	AX,8080H		; Is it special 80H?
	JE	INF_EXIT		; Brif so
	CMP	AX,0FF10H		; Is it special 0FEH?
	MOV	AL,254			
	JE	INF_EXIT		; Br if true

INF_RET0:
	XOR	AX,AX			; Return ignore char AX = 0

INF_EXIT:				; Reset ZF & CF and exit
	OR	SP,SP			
					; Exit.
cEnd					; End of B$INFMAP


;***
;B$INKMAP - map character for INKEY$
;OEM-interface routine
;
;Purpose:
;	This routine is called to translate character codes from the
;	Microsoft Standard Keyboard set to an OEM specific key code set.
;	B$INKMAP is called during the processing of the INKEY$ function.
;	Since there are differences in IBM BASIC for the code values
;	returned by the INPUT$ and INKEY$ functions, two separate
;	support routines are required for these functions.  One or
;	two byte values will be returned from this code for the
;	INKEY$ function.
;
;Entry:
;	[AX] = Microsoft Standard Keyboard code
;	[DX] = bytes 2 and 3 of three byte codes
;	PSW.C = set indicates a 2 byte character
;	PSW.C = reset indicates a 1 or 3 byte character code
;		(the code is in AL in the 1 byte case, AX in the 2 byte
;		 case, and AL and DX in the 3 byte case)
;
;Exit:
;	[AX] = OEM code to be returned by INKEY$.
;	PSW.C = set indicates a 2 byte character
;	PSW.C = reset indicates a 1 byte character code in AL
;	PSW.Z = set indicates that there is no OEM code to  associate
;		with the Microsoft Code. AX is don't care
;
;Uses:
;	Per Convention.
;
;Preserves:
;	BX, CX, DX
;
;Exceptions:
;	None.
;****
;
;	ALGORITHM:
;	If input is 3-byte code then restore extended code and exit
;	else
;		if 1-byte code then return with same code
;		else 	REM input is 2-byte code
;		if input is special 1-byte then return 1-byte code
;		else search the table for match
;		if match found then return mapped char 
;		else return zero in AX and request to ignore it.
;
;	NOTE:	If ZF = 1 (ignore char) then AX is undefined. Currently, the
;		caller (B$INKY in IOTTY.ASM) ignores AX under this condition.
;
;	IMPLEMENTATION:
;	This function is exact opposite of B$KEYINP in the sense that the
;	mapping is reversed. This assists us in sharing the same keyscan table
;	for mapping. However, the main difference is in setting up the start
;	address for key searching and the count of entries to be searched.
;	The first one is determined the input high-byte (AH) value and
;	the second one by the number of entries for each AH-type. The equates
;	KEY_128_LEN & KEY_255_LEN provide the count values that can be
;	moved into CX for looping. Also, notice that the 3-byte entries
;	<new-AL> = 254 will not be used and  separate in-line check would
;	be made to detect both 3-byte characters and special 1-byte characters
;


cProc	B$INKMAP,<PUBLIC,NEAR>,<BX,CX,SI>	
cBegin				

	JC	INK_2_BYTE	; Process 2-byte codes separately
	CMP	AL,254		; Is it a 3-byte code?
	CLC			; Assume 1-byte return code
	JNE	INK_EXIT	; NO, it is 1-byte, return the same code
	SHL	AL,1		; CF = 1, ZF = 0 to indicate 2-byte code
	XCHG	AX,DX		; Restore extended code and say it is
				; a 2-byte code (CF = 1)
	JMP	SHORT INK_EXIT	

;	Come here for 2-byte code and special 1-byte codes

INK_2_BYTE:			
	CMP	AX,0FFFFH	; Is it 2-byte code for 255?
	JE	INK_1_BYTE	; Yes, set CF=ZF = 0 and exit
	CMP	AX,08080H	; Is it 2-byte code for 128?
	JE	INK_1_BYTE	; Brif so
	CMP	AX,0FF10H	; Is it 2-byte code for 254
	JNE	INK_MAP		; Brif not to map it
	MOV	AL,254		; 0XFF10 maps to 0XFE

INK_1_BYTE:			; Set ZF = CF = 0
	OR	SP,SP
	JMP	SHORT INK_EXIT	; & Exit...

INK_MAP:			; Map the character using KEYINP_TABLE table

;	It is really a 2-byte character. Try to map it to the scan code
;	Examine High-byte to determine the table start

	MOV	CX,KEY_128_LEN	; Assume high-byte would be 128
	MOV	SI,RT_TEXTOFFSET KEYINP_TABLE	; Same as above
	CMP	AH,128		; Is it 128?
	JE	INKMAP_LOOP	; Br to search the table
	MOV	CL,KEY_255_LEN	; Setup count and start address for
	MOV	SI,RT_TEXTOFFSET KEY_255_START	; High-byte = 255

;	If AH <> 255 invalid input code - raise an alarm
DbAssertRelB	AH,E,255,RT_TEXT,<Illegal 2-byte code in INKMAP>

INKMAP_LOOP:			
	XCHG	AX,BX		; BL = Keycode
	CLD			; Autoincrement

INK_LOOP:			; Search Loop
	LODS	KEYINP_TABLE	; Get both keycode and scancode
	CMP	AL,BL		; Is key found
	JE	INK_FOUND	; Brif match occurred
	LOOP	INK_LOOP	; Try again until end of table

INK_NOTFOUND:			; The key was not found. Simply return
	XOR	AX,AX		; zero in AX and request to ignore it.
	JMP	SHORT INK_EXIT	; Exit...

INK_FOUND:			; A match has been found
				; Return the code in correct registers
	MOV	AL,AH		; Get the mapped character
	XOR	AH,AH		; Set High byte zero
	SAHF			; ZF = 0
INK_CEXIT:			
	STC			; CF = 1

INK_EXIT:			; Exit...

cEnd				; End of B$INKMAP



sEnd	RT_TEXT 		
	END
