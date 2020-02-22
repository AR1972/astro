CHOICES db 'Y','N',80h dup(0)
HELPMSG0 db 'Waits for the user to choose one of a set of choices.',13,10,13,10,0
SYNTAXMSG db 'CHOICE [/C[:]choices] [/N] [/S] [/T[:]c,nn] [text]',13,10,13,10,0
HELPMSG1 db '/C[:]choices Specifies allowable keys. Default is YN',13,10,
'/N           Do not display choices and ? at end of prompt string.',13,10,
'/S           Treat choice keys as case sensitive.',13,10,
'/T[:]c,nn    Default choice to c after nn seconds',13,10,
'text         Prompt string to display',13,10,13,10,
'ERRORLEVEL is set to offset of key user presses in choices.',13,10,13,10,0
BADSWITCHMSG db 'Invalid switch on command line. Expected form:',13,10,'    ',0
NEEDCHOICE db 'CHOICE: invalid choice switch syntax. Expected form: /C[:]choices',13,10,0
BADTIMEOUTCHARMSG db 'CHOICE: Incorrect timeout syntax.  Expected form Tc,nn or T:c,nn',13,10,0
BADTIMEOUTMSG db 'CHOICE: Timeout default not in specified (or default) choices.',13,10,0
MULTPROMPTERRMSG db 'CHOICE: only one prompt string allowed. Expected Form:',13,10,'    ',0
BADVERMSG db 'CHOICE: requires MS-DOS version 4.0 or later.',13,10,0,0
LastSecs db 00h
show db 01h
CaseSense db 00h
TimeOut db 00h
TimeOutDefault db 00h
PROMPT db 80h dup(0)
UCaseTbl dd 00h
UCaseTblBias dw 00h
ERROREXIT equ 0ffh
BLANK equ 20h
CHOICEBEG equ '['
CHOICEEND equ ']'
QUESTION equ '?'
CR equ 0Dh
LF equ 0Ah
BEL equ 07h
NUL equ 00h
TAB equ 09h
QUOTE equ '"'
SWITCHCHAR equ '/'
CHOICEDELIM equ ':'
HELPSWITCH equ '?'
CHOICESWITCH equ 'C'
DISPLAYSWITCH equ 'N'
TIMEOUTSWITCH equ 'T'
CASESWITCH equ 'S'
CHOICESDELIM equ ':'
TIMEDELIM equ ':'
FALSE equ 00h
TRUE equ 01h
TIMEDEFDELIM equ ','
MAXPROMPT equ 80h
GETUPPERTABLE equ 6502h

putchr MACRO ch
	mov ah, 02h
	mov dl, ch
	int 21h
ENDM

getchr MACRO 
	mov ah, 08h
	int 21h
ENDM
