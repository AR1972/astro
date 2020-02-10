;   Created by Ericst on 5/14/92.
;   Modified by Ericst on 8/25/92 to use 1 for first choice instead of 0
;   Modified by Ericst on 8/31/92 to change /T syntax from /Tnn,c to /Tc,nn
;       to be consistent with select-boot timeout syntax
;       Also fixed bug 1018 (timeout doesn't work with 12 seconds.
;
;   CHOICE [/c choices] [/N] [/S] [/T:c,nn] [text]
;	/c[:]choices	Specifies allowable response keys.  The
;			default is "yn".  Keys are not case sensitive
;			unless /S is specified.  If choices are specified
;			more than once, no error is generated and the last
;			set of choices are used.
;	/N		Causes CHOICE to Not display choices (and ?) at the end of
;			the string, enclosed in square brackets.
;	/S		Case sensitive.  Default is case insensitive.
;       /T[:]c,nn       Timeout after nn seconds and take choice c.
;			c must be in choices. If c is not
;			in choices an error message is displayed.
;	text		Specifies a string to be displayed.  If no text
;			is specified, only the choices are displayed (unless
;			/N is specified).  The string can be enclosed in
;			quotes ("), or may simply be terminated by end of
;			the line, or by a switch.
;
;    When one of the choices is entered, the index of the choice is
;    returned in ERRORLEVEL, where 1 means the first letter in choices
;    was pressed, 2 means the second, and so on.  CHOICE beeps if any
;    choice other than those in choices is specified. CHOICE returns 255
;    if a problem is encountered (bad arg, etc.), or if user
;    runs with /? switch.  No control-break handler is provided, so
;    user pressing control-c results in errorlevel 0.

.MODEL tiny
.code
org	100h
public	start
start:
	jmp begin
include  choice.inc
begin:
	cld				; force string ops to be forward.
; ************************************************************************
; ***	Verify MS-DOS version is 4.0 or later			       ***
; ************************************************************************
	mov	al,01h			; version number, not OEM flag
	mov	ah,30h			; Get Version Function
	int	21h
	cmp	al, 3			; This program requires 4.0 or later
	jg     GetTable

WrongVersion:
	 strout  BADVERMSG
	 mov	al, ERROREXIT
	 jmp	ChoiceDone

; ************************************************************************
; ***	Get Upper Case Table address/bias			       ***
; ************************************************************************
GetTable:
	push	es
	mov	ax,GETUPPERTABLE
        mov     bx,-1                   ; current code page
        mov     cx,5                    ; size of info for upper case table
        mov     dx,bx                   ; current country id
        push    cs

	mov	di,offset UCaseTbl ; point at buffer for table
        int     21h
                                        ; assume no error
        inc     di                      ; point at start of table
        les     di,[di]                 ; load table address
        mov     bx,es:[di]              ; load table size
        mov     ax,256                  ; compute table size bias
        sub     ax,bx
	mov	UCaseTblBias,ax 	; save it for later
	add	di,2			; Shift UcaseTbl Address so that
	mov	word ptr UCaseTbl,di	; it is at UCaseTbl (instead of
	mov	word ptr UCaseTbl+2,es	; UCaseTbl + 1; before shift, UCaseTbl
	pop	es			; points to table ID which is always 2).

;  ********************************************************************
;  ***	 Parse command line					    ***
;  ***	 Set Show=FALSE if /N,					    ***
;  ***	 Set Choices to choices from /C, if specified		    ***
;  ***	 Set CaseSense=True if /S				    ***
;  ***	 Set TimeOut=<secs>&TimeOutDefault=char to timeout too if   ***
;  ***	     /T is specified					    ***
;  ***	 Set PROMPT to user specified prompt, if specified.	    ***
;  ********************************************************************
	 mov	si,081h 	      ; Offset of command line in PSP
; While !(end of line) do
Parse1:
	 lodsb			      ; get first char
	 cmp	al, CR		      ; end of line?
	 je	ParseDone	      ; if so, Exit parse loop
	 cmp	al, SWITCHCHAR	      ; is it a switch?
	 je	IsSwitch
	 cmp	al, Blank	      ; if blank, ignore
	 je	Parse1
	 cmp	al, Tab 	      ; if tab, ignore
	 je	Parse1
					; Prompt: parse it off
	 mov	di, offset PROMPT	; di points to prompt
	 cmp	BYTE PTR [di], NUL	; Prompt already specified?
	 jne	MultPromptErr
					; Set dl depending on Quoted or not.
	 cmp	al, QUOTE		; Terminator for Quoted String: CR and
	 jne	NoQuote 		; QUOTE (set dl to QUOTE).
	 mov	dl, QUOTE
	 jmp	prompt1
NoQuote:
	 mov	dl, SWITCHCHAR		; Terminator for unquoted String:CR and
	 stosb				; SWITCHCHAR (set dl to SWITCHCHAR)
;  Repeat
Prompt1:
	 mov	al, [si]		; want SI to point to term if we exit
	 cmp	al, CR			; End of line? If so, leave si pointing
	 je	Parse1			;    at it. No need to increment.
	 cmp	al, dl			; If an end-quote we'll need to inc
	 je	Terminator		;
	 stosb				; save the char.
	 inc	si			; get next char
	 jmp	Prompt1
; Until (terminator char found)

Terminator:				; encountered end-quote or switch char
	 cmp	al, SWITCHCHAR		; if a switch leave si pointing to it.
	 je	Parse1			; If a quote, si should point to next
	 inc	si			;     character after the quote.
	 jmp	Parse1			; parse next field.

MultPromptErr:
	strout	 MULTPROMPTERRMSG
	strout	 SYNTAXMSG
	mov	al, ERROREXIT
	jmp	ChoiceDone

IsSwitch:			  ; Case Switch of:
	 lodsb			  ; get switch char
	 call ToUpper		  ; Case Switch of:
	 cmp	al,HELPSWITCH	  ; /?
	 je	IsHelpSwitch
	 cmp	al,CHOICESWITCH   ; /c choices
	 je	IsChoiceSwitch
	 cmp	al,DISPLAYSWITCH  ; /N
	 je	IsDisplaySwitch
         cmp    al,TIMEOUTSWITCH  ; /Tc,nn
	 je	IsTimeoutSwitch
	 cmp	al,CASESWITCH	  ; /S
	 je	IsCaseSwitch

	 strout BADSWITCHMSG	 ; invalid switch
	 strout SYNTAXMSG
	 mov	al, ERROREXIT
	 jmp	ChoiceDone	  ; End CASE switch.

IsHelpSwitch:			  ; /?
	 strout  HELPMSG0
	 strout  SYNTAXMSG
	 strout  HELPMSG1
	 mov	al, ERROREXIT
	 jmp	ChoiceDone

IsChoiceSwitch: 		  ; Set of choices, get them.
	 mov	di, offset choices
GetChoices:
	 mov	al, [si]
	 cmp	al, CHOICESDELIM     ; expect choicedelim or first choice
	 jne	ChoiceSw1
	 inc	si
	 mov	al, [si]

ChoiceSw1:
	 cmp	al, BLANK
	 je	ChoiceSwDone
	 cmp	al, TAB
	 je	ChoiceSwDone
	 cmp	al, CR
	 je	ChoiceSwDone
	 cmp	al, SWITCHCHAR
	 je	ChoiceSwDone
	 stosb			  ; save choice
	 inc	si		  ; loop until switch or delim found.
	 mov	al, [si]
	 jmp	ChoiceSw1	  ; get next choice.

ChoiceSwDone:			  ; Have all choices now.
	 mov	al, NUL 	  ; mark end in case of 1 char choice.
	 mov	[di],al
	 cmp	di, offset choices ; At least 1 choice specified?
	 jne	Parse1
	 strout  NEEDCHOICE	  ; No choices specified.
	 mov	al, ERROREXIT
	 jmp	ChoiceDone

IsDisplaySwitch:		  ; Display options Switch
	 mov	Show, FALSE
	 jmp	Parse1

IsTimeoutSwitch:		  ; Timeout Switch
         mov    al, [si]          ; in form /T[:]c,n[n]
	 cmp	al, TIMEDELIM
         jne    GetDefChar
         inc    si                ; skip over ':'
         mov    al, [si]

GetDefChar:
         cmp    al, CR                ; whitespace & Switch not allowed
         je     BadTimeOut
         cmp    al, SWITCHCHAR
         je     BadTimeOut
         cmp    al, Blank
         je     BadTimeOut
         cmp    al, Tab
         je     BadTimeOut
	 mov	TimeOutDefault, al   ; save default char
	 inc	si
	 mov	al, [si]
         cmp    al, TIMEDEFDELIM  ;   Delim for timeout?
         jne    BadTimeOut
         inc    si                ;   skip over delim
	 mov	al, [si]
	 cmp	al, '0' 	  ;   Ensure it's a digit
	 jb	BadTimeOut
	 cmp	al, '9'
         ja     BadTimeOut
	 and	ax, 0fh 	  ;   Conver to binary
	 mov	TimeOut, al	  ;   save (incase 1 digit)
	 inc	si		  ;   Check for another digit
         mov    al, [si]
         cmp    al, '0'
         jb     Parse1            ;   Not part of timeout switch
	 cmp	al, '9'
         ja     Parse1            ;   Not part of timeout switch
         mov    al,TimeOut
	 mov	dx, 10		  ;   num=1st dig *10 + 2nd dig
         mul    dl
         mov    bl, [si]
         and    bl, 0fh
         add    al, bl
         mov    TimeOut, al       ; save timeout
         inc    si
         jmp    Parse1

BadTimeOut:
	 strout  BADTIMEOUTMSG
	 mov	al, ERROREXIT
	 jmp	ChoiceDone

IsCaseSwitch:			   ; Case Sensitive Switch
	 mov	CaseSense,TRUE
	 jmp	Parse1

ParseDone:

;  ********************************************************************
;  ***	Done Parsing Command Line. Unless specified otherwise,	    ***
;  ***	convert choices and default to Upper Case		    ***
;  ********************************************************************
	cmp [CaseSense],TRUE	   ;  If CaseSense is set, skip upper casing
	je DoneUpCasing

	mov al, TimeOutDefault	; Convert Timeout Default to upper case
	call ToUpper
	mov TimeOutDefault, al
	mov si, offset Choices	 ; Now convert the choices to upcase
ChoiceUCase1:
	cmp BYTE PTR [si], NUL
	je  DoneUpCasing
	mov al, [si]
	call ToUpper
	mov [si], al
	inc si
	jmp ChoiceUCase1

DoneUpCasing:
;  ********************************************************************
;  *** if /t (timeout) was specified, verify specified default is   ***
;  *** in Choices						    ***
;  ********************************************************************
	mov di, offset Choices
	mov cx, MAXPROMPT	    ; search entire buffer (it is 0 filled)
	mov al, TimeOutDefault
	repnz scasb
	jz TimeOutValid
	strout	BADTIMEOUTCHARMSG   ; char not in choices.
	mov    al, ERROREXIT
	jmp    ChoiceDone

TimeOutValid:
;  ********************************************************************
;  ***	Display prompt, if any					    ***
;  ***	if /N (No Display Choices) was not specified, display	    ***
;  ***	choices with leading space if any prompt), enclosed in [],  ***
;  ***	separated by commas, followed by '?'			    ***
;  ********************************************************************
	strout	 prompt
	cmp	[Show],TRUE	      ; /N not specified
	jne	PromptDone
	mov	di, offset Choices
;	putchr	BLANK
	putchr	CHOICEBEG	    ; display leading [
ChoiceOut:
	putchr	[di]
	inc	di
	cmp	BYTE PTR [di], NUL  ; more?
	je	Choice20
	putchr	CHOICEDELIM
	jmp	ChoiceOut

Choice20:
	putchr	CHOICEEND	    ; put on final ]?
	putchr	QUESTION

PromptDone:

;  ********************************************************************
;  ***	 If TimeOut specified, wait for timeout or keypress and     ***
;  ***	     set Key to timeout default value if timeout occurs.    ***
;  ********************************************************************
	cmp TimeOut, 0
	je  TimeOutDone

	mov ah, 2ch	    ; Get Start Seconds
	int 21h
	mov LastSecs, dh

CheckKeyPress:
	mov ah, 0bh	    ; check for keypress
	int 21h
	and ax, 0001h
	or ax,0
	jnz TimeOutDone     ; key was pressed

TimeOutLoop1:
	mov ah, 2ch	    ; Get Seconds
	int 21h
	cmp [LastSecs], dh
	je CheckKeyPress
	mov LastSecs,dh
	dec TimeOut		; second elapsed
	jnz CheckKeyPress	; check for another key
	mov al, TimeOutDefault	; Timed out. simulate reading of char
	jmp GetKey01
TimeOutDone:

;  ********************************************************************
;  ***	  If no TimeOut occurred, THEN				    ***
;  ***	      LOOP						    ***
;  ***		 wait for keypress.				    ***
;  ***		 if key is not valid print BEL char & set Key=0	    ***
;  ***	      UNTIL key !=0					    ***
;  ***	  ENDIF 						    ***
;  ********************************************************************
GetKey0:
	    getchr
GetKey01:				; Jump here if Timed Out
	    mov di, offset CHOICES	; search for key in choices

GetKey1:    cmp [CaseSense], TRUE	; Unless CaseSense, convert to ucase
	    je	GetKey2
	    call ToUpper		; convert to al upper case
GetKey2:
	    cmp BYTE PTR [di], NUL
	    je	Badkey
	    scasb
	    je	GotKey
	    jmp GetKey2
Badkey:
	    putchr BEL
	    jmp GetKey0

GotKey:
	    putchr  al
	    putchr  CR
	    putchr  LF
	    sub di, offset choices	; convert to offset
	    mov ax, di

;  ********************************************************************
;  ***	Exit (offset of Key in Choices) 			    ***
;  ********************************************************************

ChoiceDone:
	    mov ah, 04Ch
	    int 21h	; Exit

;  ********************************************************************
;  ***	MyStrOut
;  ***	Input Args:    bx address of string
;  ***	Destroys:      nothing
;  ***	Returns:       nothing
;  ********************************************************************

MyStrOut PROC USES ax di dx
    mov     ah, 02h		    ; Display Char function. Can't use 09
				    ; because we need to print $'s.
    xor     di,di		    ; di is offset of chars to print
MyStrOutLoop1:
    mov     dl, [bx+di]
    cmp     dl, NUL
    je	    MyStrOutDone
    int     21h
    inc     di
    jmp     MyStrOutLoop1
MyStrOutDone:
    ret
MyStrOut ENDP

;  ********************************************************************
;  ***	ToUpper converts charcter in al to upper case		    ***
;  ***	For extended characters, uses uppercase table		    ***
;  ***	Call with:						    ***
;  ***	    al = char to uppcase				    ***
;  ***	destroys: ah & converts al to uppercase 		    ***
;  ********************************************************************
ToUpper PROC uses bx di es
	cmp al,'a'
	jb  ToUpperDone
	cmp al,'z'
	ja  NotAlpha
	sub al, 20h	; convert to uppercase
	jmp ToUpperDone
NotAlpha:
	cmp	al,80h			; if DL is extended char, get mapping
	jb	ToUpperDone		; AL not extended char
	les	di,dword ptr UCaseTbl	   ; get the table
	mov	bx, ax
	xor	bh,bh
	sub	bx, UCaseTblBias	; get the index
	mov	al,es:[di+bx]		; get the char

ToUpperDone:
	ret
ToUpper ENDP

END  start
