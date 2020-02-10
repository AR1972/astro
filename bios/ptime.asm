;
;------------------------------------------------------------------------------
;M074 - ptime.asm added, containing clock/time routines for power.asm.
;
;   09/11/91  SMR	M077: B#2669. Registered POWER's 2f channels in mult.inc
;
;   09/25/91  NSM	M090: B#2729. Try to update our time from CMOS under 
;			WIN ENH mode once in 1024 I1c ticks. 
;			(approx. once in a minute)
;			This update happens only if DOS calls us for time and
;			does not exactly happen once in a minute.
;
;			(this is changed to 20 secs from 1 minute)
;
;  11/26/91   NSM	M101: We lose date sometimes under windows. To fix this
;			do Int 1a's to get tick count instead of looking at 40:
;			6ch and if we get rollover, then go & update date 
;			and time from CMOS.
;------------------------------------------------------------------------------
;
;

	.xlist

	include version.inc     ; set build flags

IFDEF   POWER			; generate code only if power management
				; is enabled

IFNDEF  POWERALONE              ; segment declarations for resident version

	include biosseg.inc     ; establish bios segment structure

ELSE                            ; segment declarations for standalonde version

.SEQ
Bios_Code       segment word public 'Bios_Code'
Bios_Code       ends

Bios_Data       segment word public 'Bios_Data'
Bios_Data       ends

SysInitSeg      segment word public 'system_init'
SysInitSeg      ends

; following segment definitions used by transient part of code

ENDIF

	include msequ.inc
	include devsym.inc
	include bpb.inc
	include ioctl.inc
	include	mult.inc		; M077
	include power.inc

IFDEF INCL_APM
	include	apmequ.inc		; M001
ENDIF

break   macro
	endm

	include error.inc
	.list
	

	include msgroup.inc     ; define Bios_Data segment

	extrn	month_table:word

IFDEF   POWERALONE                      ; standalone device driver version
Bios_Res        dw      Bios_Code       ; Our code segment address
ELSE                                    ; resident BIOS version
	extrn   Bios_Res:word           ; Code segment address supplied externally
	extrn	ttticks:dword		; far ptr to time_to_ticks routine
	extrn	bintobcd:dword		; ptr to bin_to_bcd routine

IFDEF	INCL_APM
	extrn	Check_and_Init_APM_Ptr:dword	; ptr to APM init routine
ENDIF	;INCL_APM

	extrn	P_UpdFromCMOS_Ptr:dword	; ptr to CMOS clock read	; M081

ENDIF	;NOT POWERALONE

	extrn	daycnt:word	; extrns for both resident and stand-alone
	extrn	daycnt2:word	; versions
	extrn	base_century:byte
	extrn	base_year:byte
	extrn	month_tab:byte
	extrn	bin_date_time:byte
	extrn	CMOSUpdFlg:byte
	extrn	CMOSPollCount:word

	tocode
IFDEF	POWERALONE			; stand alone version

Bios_Data_Word  dw      Bios_Data               ; Our data segment

bintobcd	proc	near	;for real time clock support

;convert a binary input in al (less than 63h or 99 decimal)
;into a bcd value in al.  ah destroyed.

	push	cx

	aam				; M048
	mov	cl, 4			; M048
	shl	ah, cl			; M048
	or	al, ah			; M048

	pop	cx
	ret

bintobcd	endp

ELSE				; resident version
	extrn	Bios_Data_word:word

ENDIF

	public	tim_read
	public	tim_writ
;--------------------------------------------------------------------
;
; tim_writ sets the current time
;
; on entry es:[di] has the current time:
;
;	number of days since 1-1-80	(word)
;	minutes (0-59)			(byte)
;	hours (0-23)			(byte)
;	hundredths of seconds (0-99)	(byte)
;	seconds (0-59)			(byte)
;
; each number has been checked for the correct range.
;
tim_writ proc	near
	assume	ds:Bios_Data
	mov	ax,word ptr es:[di]
	push	ax		;daycnt. we need to set this at the very
				;  end to avoid tick windows.

;	Set hardware clock time.

	mov	al,es:[di+3]		;get binary hours
	call	bintobcd		;convert to bcd
	mov	ch,al			;ch = bcd hours
	mov	al,es:[di+2]		;get binary minutes
	call	bintobcd		;convert to bcd
	mov	cl,al			;cl = bcd minutes
	mov	al,es:[di+5]		;get binary seconds
	call	bintobcd		;convert to bcd
	mov	dh,al			;dh = bcd seconds
	mov	dl,0			;dl = 0 (st) or 1 (dst)
	cli				;turn off timer
	mov	ah,03h			;set rtc time
	int	1ah			;call rom bios clock routine
	sti

	mov	cx,word ptr es:[di+2]
	mov	dx,word ptr es:[di+4]
IFDEF POWERALONE
	call	time_to_ticks			
ELSE
	call	ttticks
ENDIF
				;cx:dx now has time in ticks
	cli			; turn off timer
	mov	ah, 1		; command is set time in clock
	int	1ah		; call rom-bios clock routines
	pop	[daycnt]
	sti

	call	daycnttoday	; convert to bcd format
	cli			; turn off timer
	mov	ah,05h		; set rtc date
	int	1ah		; call rom-bios clock routines
	sti

	clc
	ret
tim_writ endp

;
; gettime reads date and time
; and returns the following information:

;	es:[di]  =count of days since 1-1-80
;	es:[di+2]=hours
;	es:[di+3]=minutes
;	es:[di+4]=seconds
;	es:[di+5]=hundredths of seconds

tim_read proc	near

; M090 BEGIN - See if we have to update our time from CMOS before 
; returning date and time to caller.

	cmp	[CMOSUpdFlg],0		; M090 do we need to update from CMOS
	je	tr_NoCMOSUpdate		; 

tr_CMOSUpd:				; M101
IFDEF	POWERALONE			; M081
	call	far ptr P_UpdFromCMOS
ELSE
	call    P_UpdFromCMOS_ptr   	; M074 update our date and time
					; from CMOS RTC
ENDIF
	mov	[CMOSPollCount],MAXCMOSPOLLCOUNT
	mov	[CMOSUpdFlg],0		; 
; M090 END

tr_NoCMOSUpdate:
; M101	- BEGIN - get tick count through 1a instead of looking at 40:6ch
; and check for rollover
	mov	ax,0			; get tick count
	int	1ah			; cx:dx = tick count	
	or	al,al			; al != 0 if midnight passed	
	jnz	tr_CMOSUpd		; rollover; update date and time
; M101	- END
	mov	si,[daycnt]

; we now need to convert the time in tick to the time in 100th of
; seconds.  the relation between tick and seconds is:
;
;		 65536 seconds
;	       ----------------
;		1,193,180 tick
;
; to get to 100th of second we need to multiply by 100. the equation is:
;
;	ticks from clock  * 65536 * 100
;      ---------------------------------  = time in 100th of seconds
;		1,193,180
;
; fortunately this fromula simplifies to:
;
;	ticks from clock * 5 * 65,536
;      --------------------------------- = time in 100th of seconds
;		59,659
;
; the calculation is done by first multipling tick by 5. next we divide by
; 59,659.  in this division we multiply by 65,536 by shifting the dividend
; my 16 bits to the left.
;
; start with ticks in cx:dx
; multiply by 5

	mov	ax,cx
	mov	bx,dx
	shl	dx,1
	rcl	cx,1		;times 2
	shl	dx,1
	rcl	cx,1		;times 4
	add	dx,bx
	adc	ax,cx		;times 5
	xchg	ax,dx		
	

; now have ticks * 5 in dx:ax
; we now need to multiply by 65,536 and divide by 59659 d.

	mov	cx,59659	; get divisor
	div	cx
				; dx now has remainder
				; ax has high word of final quotient
	mov	bx,ax		; put high work if safe place
	xor	ax,ax		; this is the multiply by 65536
	div	cx		; bx:ax now has time in 100th of seconds


;rounding based on the remainder may be added here
;the result in bx:ax is time in 1/100 second.

	mov	dx,bx
	mov	cx,200		;extract 1/100's

;division by 200 is necessary to ensure no overflow--max result
;is number of seconds in a day/2 = 43200.

	div	cx
	cmp	dl,100		;remainder over 100?
	jb	noadj
	sub	dl,100		;keep 1/100's less than 100
noadj:
	cmc			;if we subtracted 100, carry is now set
	mov	bl,dl		;save 1/100's

;to compensate for dividing by 200 instead of 100, we now multiply
;by two, shifting a one in if the remainder had exceeded 100.

	rcl	ax,1
	mov	dl,0
	rcl	dx,1
	mov	cx,60		;divide out seconds
	div	cx
	mov	bh,dl		;save the seconds
	div	cl		;break into hours and minutes
	xchg	al,ah

;time is now in ax:bx (hours, minutes, seconds, 1/100 sec)

	push	ax
	mov	ax,si		; daycnt
	stosw
	pop	ax
	stosw
	mov	ax,bx
	stosw
	clc
	ret

tim_read endp

	assume	es:nothing

; the following routine is executed at resume time when the system
; powered on after suspension. it reads the real time clock and
; resets the system time and date
;
; This can be patched to be the INT6C vector so that this can be
; used by other people to update DOS time from CMOS (after taking
; care for IRET)

	public	P_UpdFromCMOS
P_UpdFromCMOS	proc	far

	assume	ds:nothing
	push	ds
	mov	ds,cs:Bios_Data_Word
	assume ds:Bios_Data

	call	read_real_date		; get the date from the clock
PUFC_UpdDate:				; M101
	mov	ds:daycnt,si		; update our copy of date
	call	read_real_time		; get the time from the rtc
	cli
	mov	ah,01h			; command to set the time
	int	1ah			; call rom-bios time routine
	sti
; M101 BEGIN - check back to see if date changed when we were reading the
; time; if so, update date and time all over again ( Paranoid?)
	call	read_real_date		; get the date from the clock

; BUGBUG- nagara - Store the ret.value of date from int 1a in this
; procedure read_real_date so that at the end we don't really have to
; call read_real_date again; we just need to call int 1a and compare the
; date registers against the stored values of these registers.

	cmp	si,ds:daycnt		; is the date changed ?
	jne	PUFC_UpdDate		; yes, go back and set the new date
; M101 END
	pop	ds
	ret	

P_UpdFromCMOS	endp


;************************************************************************
;
;   read_real_date reads real-time clock for date and returns the number
;   of days elapsed since 1-1-80 in si
;
read_real_date proc near
	assume	ds:Bios_Data,es:nothing

	push	ax
	push	cx
	push	dx

	xor	ah,ah			; throw away clock roll over
	int	1ah

	pop	dx
	pop	cx
	pop	ax			; cas - bad code!

	push	ax
	push	bx
	push	cx
	push	dx

	mov	daycnt2,1	; real time clock error flag (+1 day)
	mov	ah,4		; read date function code
	int	1ah		; read real-time clock
	jnc	read_ok 	; jmp success
	jmp	r_d_ret 	; jmp error

read_ok:			; ******* get bcd values in binary *****
	mov	word ptr bin_date_time+0,cx  ; store as hex value
	mov	word ptr bin_date_time+2,dx  ; ...

	mov	daycnt2,2	; read of r-t clock successful
	call	bcd_verify	; verify bcd values in range
	jc	r_d_ret 	; jmp some value out of range
	mov	daycnt2,3	; read of r-t clock successful
	call	date_verify	; verify date values in range
	jc	r_d_ret 	; jmp some value out of range
	mov	daycnt2,0	; verify successful
	call	in_bin		; convert date to binary
				; ******* years since 1-1-80 *********
	mov	al,byte ptr bin_date_time    ; get years into century
	cbw				     ;
	cmp	byte ptr bin_date_time+1,20  ; 20th century?
	jnz	century_19		     ; jmp no
	add	ax,100		; add in a century
century_19:			;
	sub	ax,80		; subtract off 1-1-80
	mov	cl,4		; leap year every 4
	div	cl		; al= # leap year blocks, ah= remainder
	mov	bl,ah		; save odd years
	cbw			; zero ah
	mov	cx,366+3*365	; # of days in leap year blocks
	mul	cx		; dx:ax is result
	mov	daycnt2,ax	; save count of days
	mov	al,bl		; get odd years count
	cbw			;
	or	ax,ax		; is ax= 0?
	jz	leap_year	; jmp if none
	mov	cx,365		; days in year
	mul	cx		; dx:ax is result
	add	daycnt2,ax	; add on days in odd years
	jmp	short leap_adjustment ; account for leap year
leap_year:			; possibly account for a leap day
	cmp	byte ptr bin_date_time+3,2 ; is month february
	jbe	no_leap_adjustment ; jan or feb. no leap day yet.
leap_adjustment:		; account for leap day
	inc	daycnt2
no_leap_adjustment:		; ******* get days of month *******
	mov	cl,byte ptr bin_date_time+2
	xor	ch,ch
	dec	cx		; because of offset from day 1, not day 0
	add	daycnt2,cx	 ; ******* get days in months preceeing *****
	mov	cl,byte ptr bin_date_time+3   ; get month
	xor	ch,ch
	dec	cx		; january starts at offset 0
	shl	cx,1		; word offset
	mov	si,offset month_table	; beginning of month_table
	add	si,cx		; point into month table
	mov	ax,word ptr [si]; get # days in previous months
	add	daycnt2,ax
r_d_ret:
	mov	si,daycnt2	 ; result in si
	pop	dx
	pop	cx
	pop	bx
	pop	ax
	ret

r_t_retj:
	xor	cx,cx
	xor	dx,dx
	jmp	short r_t_ret

read_real_date endp

;--------------------------------------------------------------------

; read_real_time reads the time from the rtc. on exit, it has the number of
; ticks (at 18.2 ticks per sec.) in cx:dx.

read_real_time proc near
	assume	ds:Bios_Data,es:nothing	

	mov	ah,2
	int	1ah
	jc	r_t_retj

oktime:
	mov	word ptr bin_date_time,cx	; hours + minutes
	mov	byte ptr bin_date_time+3,dh	; seconds
	mov	byte ptr bin_date_time+2,0	; unused for time

	call	bcd_verify
	jc	r_t_retj
	call	time_verify
	jc	r_t_retj

	call	in_bin
	mov	cx,word ptr bin_date_time
	mov	dx,word ptr bin_date_time+2

; get time in ticks in cx:dx

IFDEF POWERALONE
	call	time_to_ticks			
ELSE
	call	ttticks
ENDIF
r_t_ret:
	ret

read_real_time endp

;--------------------------------------------------------------------

;   in_bin converts bin_date_time values from bcd to bin

in_bin	proc	near
	assume	ds:Bios_Data,es:nothing

	mov	al,byte ptr bin_date_time+0  ; years or minutes 
	call	bcd_to_bin
	mov	byte ptr bin_date_time+0,al

	mov	al,byte ptr bin_date_time+1  ;century or hours
	call	bcd_to_bin
	mov	byte ptr bin_date_time+1,al

	mov	al,byte ptr bin_date_time+2  ; days (not used for time)
	call	bcd_to_bin
	mov	byte ptr bin_date_time+2,al

	mov	al,byte ptr bin_date_time+3  ; months or seconds
	call	bcd_to_bin
	mov	byte ptr bin_date_time+3,al

	ret

in_bin	endp

;--------------------------------------------------------------------

;   bcd_to_bin converts two bcd nibbles in al (value <= 99.) to
;   a binary representation in al
;   ah is destroyed

bcd_to_bin proc	near
	assume	ds:nothing,es:nothing

	mov	ah, al		; M048
	and	al, 0fh		; M048
	mov	cl, 4		; M048
	shr	ah, cl		; M048
	aad			; M048
	ret

bcd_to_bin endp

;--------------------------------------------------------------------

;   date_verify loosely checks bcd date values to be in range in bin_date_time

date_verify proc near
	assume	ds:Bios_Data,es:nothing

	cmp	byte ptr bin_date_time+1,20h  ; century check
	ja	date_error		      ;	error
	jz	century_20		      ; jmp in 20th century

	cmp	byte ptr bin_date_time+1,19h  ; century check
	jb	date_error		      ;  error
	cmp	byte ptr bin_date_time+0,80h  ; year check
	jb	date_error		      ;  error

century_20:
	cmp	byte ptr bin_date_time+0,99h  ; year check
	ja	date_error		      ;  error
	cmp	byte ptr bin_date_time+3,12h  ; month check
	ja	date_error		      ;  error
	cmp	byte ptr bin_date_time+3,00h  ; month check
	jbe	date_error		      ;  error
	cmp	byte ptr bin_date_time+2,31h  ; day check
	ja	date_error		      ;  error
	cmp	byte ptr bin_date_time+2,00h  ; day check
	jbe	date_error		      ;  error
	clc				      ; set success flag
	ret
date_error:
	stc				      ; set error flag
	ret

date_verify endp

;--------------------------------------------------------------------

; time_verify very loosely checks bcd date values to be in range in bin_date_time

time_verify proc near
	assume	ds:Bios_Data,es:nothing

	cmp	byte ptr bin_date_time+1,24h
	ja	time_error
	cmp	byte ptr bin_date_time+0,59h
	ja	time_error
	cmp	byte ptr bin_date_time+3,59h
	ja	time_error

	clc
	ret

time_error:
	stc
	ret

time_verify endp

;--------------------------------------------------------------------

;   bcd_verify checks values in bin_date_time to be valid
;   bcd numerals.  carry set if any nibble out of range

bcd_verify proc near
	assume	ds:Bios_Data,es:nothing

	mov	cx,4		; 4 bytes to check
	mov	bx,offset bin_date_time
bv_loop:
	mov	al,[bx] 	; get a bcd number (0..99)
	mov	ah,al
	and	ax,0f00fh	; 10's place in high ah, 1's in al
	cmp	al,10		; is 1's place in range?
	ja	bv_error	; jmp out of range

	shr	ah,1		; swap nibbles
	shr	ah,1
	shr	ah,1
	shr	ah,1
	and	ah,0fh		; get rid of any erroneous bits
	cmp	ah,10		; is 10's place in range
	ja	bv_error	; jmp out of range

	inc	bx		; next byte
	dec	cx
	jnz	bv_loop

	clc			; set success flag
	ret

bv_error:
	stc			; set error flag
	ret

bcd_verify endp

;--------------------------------------------------------------------
daycnttoday	proc	near	; for real time clock support

;entry: [daycnt] = number of days since 1-1-80
;
;return: ch - centry in bcd
;	 cl - year in bcd
;	 dh - month in bcd
;	 dl - day in bcd

	push	di
	mov	di,daycnt
	cmp	di,(365*20+(20/4))	;# of days from 1-1-1980 to 1-1-2000
	jae	century20

	mov	base_century,19
	mov	base_year,80
	jmp	short years

century20:				;20th century
	mov	base_century,20
	mov	base_year,0
	sub	di,(365*20+(20/4))	;adjust daycnt
years:
	xor	dx,dx
	mov	ax,di
	mov	bx,(366+365*3)		;# of days in a leap year block
	div	bx			;ax = # of leap block, dx = daycnt
	mov	di,dx			;save daycnt left

	mov	bl,4
	mul	bl			;ax = # of years. less than 100 years!
	add	base_year,al		;so, ah = 0. adjust year accordingly.
	inc	di			;set daycnt to 1 base
	cmp	di,366			;the daycnt here is the remainder of the leap year block.
	jbe	leapyear		;so, it should within 366+355+355+355 days.
	inc	base_year		;first if daycnt <= 366, then leap year
	sub	di,366			;else daycnt--, base_year++;
					;and the next three years are regular years.
	mov	cx,3
regularyear:
	cmp	di,365			;for(i=1; i>3 or daycnt <=365;i++)
	jbe	yeardone		;{if (daycnt > 365)
	inc	base_year		;  { daycnt -= 365
	sub	di,365			;  }
	loop	regularyear		;}

;	should never fall through loop

leapyear:
	mov	byte ptr month_tab+1,29 ;leap year. change the month table.
yeardone:
	xor	bx,bx
	xor	dx,dx
	mov	ax,di
	mov	si,offset month_tab
	mov	cx,12
months:
	inc	bl
	mov	dl,byte ptr ds:[si]	;compare daycnt for each month until fits
	cmp	ax,dx			;dh=0.
	jbe	month_done

	inc	si			;next month
	sub	ax,dx			;adjust daycnt
	loop	months

;	should never fall through loop

month_done:
	mov	byte ptr month_tab+1,28 ;restore month table value
	mov	dl,bl
	mov	dh,base_year
	mov	cl,base_century 	;now, al=day, dl=month,dh=year,cl=century
	call	bintobcd		;oh my!!! to save 15 bytes, bin_to_bcd proc
					;was relocated seperately from daycnt_to_day proc.
	xchg	dl,al			;dl = bcd day, al = month
	call	bintobcd

	xchg	dh,al			;dh = bcd month, al = year
	call	bintobcd

	xchg	cl,al			;cl = bcd year, al = century
	call	bintobcd

	mov	ch,al			;ch = bcd century
	pop	di			;restore original value
	ret
daycnttoday	endp

IFDEF	POWERALONE		; needed only for standalone version
				; for resident version use the one in 
				; mschar.asm
;--------------------------------------------------------------------
; convert time to ticks
; input : time in cx and dx
; ticks returned in cx:dx

time_to_ticks proc near

; first convert from hour,min,sec,hund. to
; total number of 100th of seconds

	mov	al,60
	mul	ch		;hours to minutes
	mov	ch,0
	add	ax,cx		;total minutes
	mov	cx,6000 	;60*100
	mov	bx,dx		;get out of the way of the multiply
	mul	cx		;convert to 1/100 sec
	mov	cx,ax
	mov	al,100
	mul	bh		;convert seconds to 1/100 sec
	add	cx,ax		;combine seconds with hours and min.
	adc	dx,0		;ripple carry
	mov	bh,0
	add	cx,bx		;combine 1/100 sec
	adc	dx,0

;	dx:cx is time in 1/100 sec

	xchg	ax,dx
	xchg	ax,cx		;now time is in cx:ax
	mov	bx,59659
	mul	bx		;multiply low half
	xchg	dx,cx
	xchg	ax,dx		;cx->ax, ax->dx, dx->cx
	mul	bx		;multiply high half
	add	ax,cx		;combine overlapping products
	adc	dx,0
	xchg	ax,dx		;ax:dx=time*59659
	mov	bx,5
	div	bl		;divide high half by 5
	mov	cl,al
	mov	ch,0
	mov	al,ah		;remainder of divide-by-5
	cbw
	xchg	ax,dx		;use it to extend low half
	div	bx		;divde low half by 5
	mov	dx,ax
				; cx:dx is now number of ticks in time
	ret
time_to_ticks endp
ENDIF
Bios_Code	ends
ENDIF
	END
