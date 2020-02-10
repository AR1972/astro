;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,160
	title	msinit for BIOS
;
;----------------------------------------------------------------------------
;
; M010 : IFDEFd the usage of Extended Keynboard read functions (10h &11h)
;        To support LOTUS Metro.
;
; M021 : Fix for AT&T 6300 WGS ROM BIOS bug
;
; M036 : Added VDISK header clearing code at INT 19 & CTRL ALT DEL
;
; M043 SR 10/17/90	Added special code which checks for IBM machines and
;			boots only on them. Code is activated only if the
;			flag IBM_VERSION is defined at assembly time.
;
; M048 : bin_to_bcd & bcd_to_bin using aam & aad
;
; M068 : B#6271. Added missed out RPL support.
;
; M083 : Take out bios' INT 6C hook if POWER is built into BIOS.
;
;----------------------------------------------------------------------------
;

EXTENDEDKEY	equ	1	; use extended keyboard functions

	include version.inc	; set build flags
	include biosseg.inc	; establish bios segment structure

	include	msequ.inc
	include	msdskpr.inc
	include dossym.inc
	include	bpb.inc
	include	bootform.inc
	include	dosmac.inc
	include biostruc.inc
	include	msbds.inc

	include	mult.inc	; M068


;	the following segment follows sysinit.  It is used to define
;	  the location to load MSDOS.SYS into.

dos_load_seg	segment	para public 'dos_load_seg'
dos_load_seg	ends


	include msgroup.inc	; establish Bios_Data segment


;	Note:  Most of the disk routines keep a pointer to the current bds
;	   in es:[di] or ds:[di].  The newer routines (in the resident code)
;	   use es:[di] consistently, because they assume that ds: points to
;	   Bios_Data.  The macros "mapnew" and "unmapnew" are used for
;	   switching back and forth when calling subroutines with the new
;	   mapping.

mapnew	macro
	push	es		; save whatever's in es
	push	ds		; copy bds to es:di
	pop	es
	push	cs		; copy Bios_Data to ds
	pop	ds
	endm

unmapnew	macro
	push	es		; copy bds back to ds:di
	pop	ds
	pop	es		; pop whatever was in es
	endm

;=======================================================
;=======================================================
;M011 -- eliminated hardnum,hdsktab,bdsh,bdsx,endtwohard,endonehard
;M011 -- changed bdsms -> bdss
	extrn	hdrv_pat:word
	extrn	orig13:dword
	extrn	RomVectors:dword
	extrn	NUMROMVECTORS:abs
	extrn	harddrv:byte
	extrn	drvmax:byte
	extrn	dskdrvs:word
	extrn	eot:byte
	extrn	fhave96:byte
	extrn	daycnt:word
	extrn	res_dev_list:word
	extrn	dsktnum:byte
	extrn	start_bds:dword
	extrn	fhavek09:byte
	extrn	single:byte
	extrn	bdss:byte		;for mini disk
	extrn	havecmosclock:byte	;set by msinit. used by msclock.asm
	extrn	bintobcd:dword		;set by msinit. used by mschar.asm
	extrn	daycnttoday:dword	;set by msinit. used by mschar.asm
	extrn	old13:dword
	extrn	temp_h:word		; for 32 bit calculation. msdisk
	extrn	start_sec_h:word
	extrn	keyrd_func:byte 	; for mscon. defined in msdata.
	extrn	keysts_func:byte	; for mscon. defined in msdata.
	extrn	disksector:byte
	extrn	fat_12_id:byte
	extrn	fat_16_id:byte
	extrn	vol_no_name:byte
	extrn	motorstartup:byte
	extrn	model_byte:byte
	extrn	secondary_model_byte:byte

IFNDEF	POWER				; M083
	extrn	int6c_ret_addr:dword	; return address from int 6c
ENDIF ;	NOT POWER			; M083

	extrn	bin_date_time:byte
	extrn	month_table:word
	extrn	daycnt2:word
	extrn	feb29:byte
	extrn	ttticks:dword		;indirect intra-segment call address

	extrn	dosdatasg:word

ifdef ROMDOS
	extrn	BIOS_Res:word		; seg addr of bios code ROM
	extrn	BootFlags:word		; boot options
endif

	extrn	errout:near
	extrn	block13:far
	extrn	Int15:far		; M036
	extrn	int19:far
	extrn	intret:near
	extrn	cbreak:near
	extrn	outchr:near
	extrn	errin:near
	extrn	outchr:near

sysinitseg segment 
	assume	cs:sysinitseg
	extrn	current_dos_location:word
	extrn	device_list:dword
	extrn	memory_size:word
	extrn	default_drive:byte
	extrn	buffers:word
	extrn	sysinit:far
	extrn	temp_bcode_seg:word
	extrn	seg_reinit_ptr:dword
        extrn   toomanydrivesflag:byte          ; M029
        extrn   StartMsg:byte
sysinitseg ends

	assume	cs:datagrp

;	end of disk modules for configuration

	extrn	end96tpi:byte
	extrn	endfloppy:byte

;	ibm fixed up at rom

	extrn	ibm_disk_io:far
	extrn	endatrom:byte

;	M015 -- begin additions

;	compaq fixed up rom

	extrn	compaq_disk_io:far
	extrn	end_compaq_i13hook:byte

;	M015 -- end additions

;	equates for cmos.
									  
;----------------------------------------				  
;	cmos equates for this system	:				  
;------------------------------------------------------------------------------- 
cmos_port	equ	070h		; i/o address of cmos address port	 
cmos_data	equ	071h		; i/o address of cmos data port 	 
nmi		equ	10000000b	; disable nmi interrupts mask - 	 
					;  high bit of cmos location address	 
										 
;---------- cmos table location address's ## ----------------------------------- 
cmos_reg_a	equ	00ah		; status register a   '----------------- 
cmos_reg_b	equ	00bh		; status register b  alarm		 
cmos_shut_down	equ	00fh		; shutdown status command byte		 


;cmos clock setting support routines used by msclock.
;
; warning!!! this code will be dynamically relocated by msinit.

	extrn	base_century:byte
	extrn	base_year:byte
	extrn	month_tab:byte


; close data, open Bios_Code segment

	tocode

	extrn	bc_retf:near		; actually should be declared in
					;   Bios_Code segment
	extrn	setdrive:near
	extrn	diskrd:near
	extrn	getbp:near
	extrn	mov_media_ids:near
	extrn	clear_ids:near
Bios_Code ends


Bios_Data_Init segment
	assume	cs:datagrp

	public	daycnt_to_day	; for real time clock support
daycnt_to_day	proc	far	; for real time clock support

;entry: [daycnt] = number of days since 1-1-80
;
;return: ch - centry in bcd
;	 cl - year in bcd
;	 dh - month in bcd
;	 dl - day in bcd

	push	[daycnt]		;save daycnt
	cmp	daycnt,(365*20+(20/4))	;# of days from 1-1-1980 to 1-1-2000
	jae	century20

	mov	base_century,19
	mov	base_year,80
	jmp	short years

century20:				;20th century
	mov	base_century,20
	mov	base_year,0
	sub	daycnt,(365*20+(20/4))	;adjust daycnt
years:
	xor	dx,dx
	mov	ax,daycnt
	mov	bx,(366+365*3)		;# of days in a leap year block
	div	bx			;ax = # of leap block, dx = daycnt
	mov	daycnt,dx		;save daycnt left

	mov	bl,4
	mul	bl			;ax = # of years. less than 100 years!
	add	base_year,al		;so, ah = 0. adjust year accordingly.
	inc	daycnt			;set daycnt to 1 base
	cmp	daycnt,366		;the daycnt here is the remainder of the leap year block.
	jbe	leapyear		;so, it should within 366+355+355+355 days.
	inc	base_year		;first if daycnt <= 366, then leap year
	sub	daycnt,366		;else daycnt--, base_year++;
					;and the next three years are regular years.
	mov	cx,3
regularyear:
	cmp	daycnt,365		;for(i=1; i>3 or daycnt <=365;i++)
	jbe	yeardone		;{if (daycnt > 365)
	inc	base_year		;  { daycnt -= 365
	sub	daycnt,365		;  }
	loop	regularyear		;}

;	should never fall through loop

leapyear:
	mov	byte ptr month_tab+1,29 ;leap year. change the month table.
yeardone:
	xor	bx,bx
	xor	dx,dx
	mov	ax,daycnt
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
	pop	[daycnt]		;restore original value
	ret
daycnt_to_day	endp

enddaycnttoday label	byte

	public	bin_to_bcd
bin_to_bcd	proc	far		;for real time clock support

;convert a binary input in al (less than 63h or 99 decimal)
;into a bcd value in al.  ah destroyed.

	push	cx

	aam				; M048
	mov	cl, 4			; M048
	shl	ah, cl			; M048
	or	al, ah			; M048

;M048	xor	ah,ah
;M048	mov	cl,10
;M048	div	cl		;al - high digit for bcd, ah - low digit for bcd
;M048	mov	cl,4
;M048	shl	al,cl		;mov the high digit to high nibble
;M048	or	al,ah

	pop	cx
	ret
bin_to_bcd	endp

endcmosclockset label byte		; end of cmos clock support routines

IFNDEF	POWER				; M083
	align	2			; cas -- trash aligns!

; the k09 requires the routines for reading the clock because of the suspend/
; resume facility. the system clock needs to be reset after resume.

	assume	es:nothing

; the following routine is executed at resume time when the system
; powered on after suspension. it reads the real time clock and
; resets the system time and date, and then irets.

; warning!!! this code will be dynamically relocated by msinit.

int6c	proc	far
	push	cs
	pop	ds

	assume ds:datagrp

	pop	word ptr int6c_ret_addr ; pop off return address
	pop	word ptr int6c_ret_addr+2
	popf
	call	read_real_date		; get the date from the clock
	cli
	mov	ds:daycnt,si		; update dos copy of date
	sti
	call	read_real_time		; get the time from the rtc
	cli

	mov	ah,01h			; command to set the time
	int	1ah			; call rom-bios time routine

	sti
	jmp	int6c_ret_addr		; long jump

int6c	endp


;************************************************************************
;
;   read_real_date reads real-time clock for date and returns the number
;   of days elapsed since 1-1-80 in si
;
read_real_date proc near
	assume	ds:datagrp,es:nothing

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

	mov	cs:daycnt2,1	; real time clock error flag (+1 day)
	mov	ah,4		; read date function code
	int	1ah		; read real-time clock
	jnc	read_ok 	; jmp success
	jmp	r_d_ret 	; jmp error

read_ok:			; ******* get bcd values in binary *****
	mov	byte ptr bin_date_time+0,ch  ; store as hex value
	mov	byte ptr bin_date_time+1,cl  ; ...
	mov	byte ptr bin_date_time+2,dh  ; ...
	mov	byte ptr bin_date_time+3,dl  ; ...

	mov	cs:daycnt2,2	; read of r-t clock successful
	call	bcd_verify	; verify bcd values in range
	jc	r_d_ret 	; jmp some value out of range
	mov	cs:daycnt2,3	; read of r-t clock successful
	call	date_verify	; verify date values in range
	jc	r_d_ret 	; jmp some value out of range
	mov	cs:daycnt2,0	; verify successful
	call	in_bin		; convert date to binary
				; ******* years since 1-1-80 *********
	mov	al,byte ptr bin_date_time+1  ; get years into century
	cbw				     ;
	cmp	byte ptr bin_date_time+0,20  ; 20th century?
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
	mov	cs:daycnt2,ax	; save count of days
	mov	al,bl		; get odd years count
	cbw			;
	or	ax,ax		; is ax= 0?
	jz	leap_year	; jmp if none
	mov	cx,365		; days in year
	mul	cx		; dx:ax is result
	add	cs:daycnt2,ax	; add on days in odd years
	jmp	short leap_adjustment ; account for leap year
leap_year:			; possibly account for a leap day
	cmp	byte ptr bin_date_time+2,2 ; is month february
	jbe	no_leap_adjustment ; jan or feb. no leap day yet.
leap_adjustment:		; account for leap day
	inc	cs:daycnt2
no_leap_adjustment:		; ******* get days of month *******
	mov	cl,byte ptr bin_date_time+3
	xor	ch,ch
	dec	cx		; because of offset from day 1, not day 0
	add	cs:daycnt2,cx	 ; ******* get days in months preceeing *****
	mov	cl,byte ptr bin_date_time+2   ; get month
	xor	ch,ch
	dec	cx		; january starts at offset 0
	shl	cx,1		; word offset
	mov	si,offset month_table	; beginning of month_table
	add	si,cx		; point into month table
	mov	ax,word ptr [si]; get # days in previous months
	add	cs:daycnt2,ax
r_d_ret:
	mov	si,cs:daycnt2	 ; result in si
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
	assume	ds:datagrp,es:nothing	; cas -- is this correct????

	mov	ah,2
	int	1ah
	jc	r_t_retj

oktime:
	mov	byte ptr bin_date_time,ch	; hours
	mov	byte ptr bin_date_time+1,cl	; minutes
	mov	byte ptr bin_date_time+2,dh	; seconds
	mov	byte ptr bin_date_time+3,0	; unused for time

	call	bcd_verify
	jc	r_t_retj
	call	time_verify
	jc	r_t_retj

	call	in_bin
	mov	ch,byte ptr bin_date_time
	mov	cl,byte ptr bin_date_time+1
	mov	dh,byte ptr bin_date_time+2
	mov	dl,byte ptr bin_date_time+3

; get time in ticks in cx:dx

	call	ttticks			; note:  indirect far call
r_t_ret:
	ret

read_real_time endp

;--------------------------------------------------------------------

;   in_bin converts bin_date_time values from bcd to bin

in_bin	proc	near
	assume	ds:datagrp,es:nothing

	mov	al,byte ptr bin_date_time+0  ; century or hours
	call	bcd_to_bin
	mov	byte ptr bin_date_time+0,al

	mov	al,byte ptr bin_date_time+1  ; years or minutes
	call	bcd_to_bin
	mov	byte ptr bin_date_time+1,al

	mov	al,byte ptr bin_date_time+2  ; months or seconds
	call	bcd_to_bin
	mov	byte ptr bin_date_time+2,al

	mov	al,byte ptr bin_date_time+3  ; days (not used for time)
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

;	  subtract hi nibble * 6

;M048	mov	ah,al
;M048	and	ah,0f0h 	; isolate the high nibble
;M048	shr	ah,1		; get ah=hi nibble*8
;M048	shr	ah,1		; get ah=hi nibble*4
;M048	sub	al,ah		; subtract hi nibble * 4
;M048	shr	ah,1		; get ah=hi nibble*2
;M048	sub	al,ah		; subtract hi nibble * 2
;M048	ret

bcd_to_bin endp

;--------------------------------------------------------------------

;   date_verify loosely checks bcd date values to be in range in bin_date_time

date_verify proc near
	assume	ds:datagrp,es:nothing

	cmp	byte ptr bin_date_time+0,20h  ; century check
	ja	date_error		      ;	error
	jz	century_20		      ; jmp in 20th century

	cmp	byte ptr bin_date_time+0,19h  ; century check
	jb	date_error		      ;  error
	cmp	byte ptr bin_date_time+1,80h  ; year check
	jb	date_error		      ;  error

century_20:
	cmp	byte ptr bin_date_time+1,99h  ; year check
	ja	date_error		      ;  error
	cmp	byte ptr bin_date_time+2,12h  ; month check
	ja	date_error		      ;  error
	cmp	byte ptr bin_date_time+2,00h  ; month check
	jbe	date_error		      ;  error
	cmp	byte ptr bin_date_time+3,31h  ; day check
	ja	date_error		      ;  error
	cmp	byte ptr bin_date_time+3,00h  ; day check
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
	assume	ds:datagrp,es:nothing

	cmp	byte ptr bin_date_time+0,24h
	ja	time_error
	cmp	byte ptr bin_date_time+1,59h
	ja	time_error
	cmp	byte ptr bin_date_time+2,59h
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
	assume	ds:datagrp,es:nothing

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

;  the real time clock structures were moved to msbio2.asm

endk09	label	byte

ENDIF ; NOT POWER		; M083

;*********************************************************
;	system initialization
;
;	the entry conditions are established by the bootstrap
;	loader and are considered unknown. the following jobs
;	will be performed by this module:
;
;	1.	all device initialization is performed
;
;	2.	a local stack is set up and ds:si are set
;		to point to an initialization table. then
;		an inter-segment call is made to the first
;		byte of the dos
;
;	3.	once the dos returns from this call the ds
;		register has been set up to point to the start
;		of free memory. the initialization will then
;		load the command program into this area
;		beginning at 100 hex and transfer control to
;		this program.
;
;********************************************************



; drvfat must be the first location of freeable space!

	align	2
drvfat	dw	0000			;drive and fat id of dos
bios_l dw	0000			;first sector of data (low word)
bios_h dw	0000			;first sector of data (high word)
doscnt	dw	0000			;how many sectors to read
fbigfat db	0			; flags for drive

fatloc		dw	?		; seg addr of fat sector
init_bootseg	dw	?		; seg addr of buffer for reading boot record
rom_drv_num	db	80h		; rom drv number
md_sectorsize	dw 512			; used by get_fat_sector proc.
temp_cluster	dw 0			; used by get_fat_sector proc.
last_fat_secnum dw -1			; used by get_fat_sector proc.

; the following two bytes are used to save the info returned by int 13, ah = 8
; call to determine drive parameters.

num_heads db	2			; number of heads returned by rom
sec_trk   db	9			; sec/trk returned by rom
num_cyln  db	40			; number of cylinders returned by rom

fakefloppydrv db      0 		; if 1, then no diskette drives in the system.

bootbias  =	200h
boot_addr =	7c00h

	align	2
disktable dw	512,  0100h,  64,0
	dw	2048, 0201h, 112,0
	dw	8192, 0402h, 256,0
	dw	32680,0803h, 512,0	;warning !!! old values
	dw	65535,1004h,1024,0



; default disktable under the assumption of total fat size <= 128 kb, and
;	the maxium size of fat entry = 16 bit.

disktable2  dw	 0, 32680, 0803h, 512, 0	;for compatibility.
	    dw	4h, 0000h, 0402h, 512, fbig	;covers upto 134 mb media.
	    dw	8h, 0000h, 0803h, 512, fbig	;	upto 268 mb
	    dw 10h, 0000h, 1004h, 512, fbig	;	upto 536 mb
	    dw 20h, 0000h, 2005h, 512, fbig	;	upto 1072 mb
	    dw 40h, 0000h, 4006h, 512, fbig	;	upto 2144 mb
	    dw 80h, 0000h, 8007h, 512, fbig	;	upto 4288 mb...

;******************************************************
;variables for mini disk initialization
;******************************************************

;M011 -- begin changed section

rom_minidisk_num db	0		; temp variable for phys unit
hnum		db	0		; real number of hardfiles
last_dskdrv_table dw	offset dskdrvs	; index into dskdrv table
end_of_bdss	dw	bdss		;offset value of the ending address
					;of bds table. needed to figure out
					;the dosdatasg address.
mini_hdlim	dw	0
mini_seclim	dw	0

;M011 -- end changed section

;end of mini disk init variables *********************


bios_date db	'01/10/84',0		;used for checking at rom bios date.
bdate_l	=	(offset $)-(offset bios_date)

; the following are the recommended bpbs for the media that we know of so
; far.

bpbx	struc
	dw	512
	db	?
	dw	1
	db	2
	dw	?
	dw	?
	db	?
	dw	?
	dw	?
	dw	2
	dw	0
	dw	0		; hidden sector high
	dd	0		; extended total sectors
bpbx	ends

	align	2
bpb48t	bpbx	<,2,,,112,2*9*40,0fdh,2,9>	; 48 tpi diskettes
	align	2
bpb96t	bpbx	<,1,,,224,2*15*80,0f9h,7,15>	; 96 tpi diskettes
	align	2
bpb35	bpbx	<,2,,,70h,2*9*80,0f9h,3,9>	; 3.5" diskettes
	align	2							; m037
bpb288	bpbx	<,2,,,240,2*36*80,0f0h,9,36>	; 2.88MB diskettes	; m037
	align	2
bpbtable dw	offset datagrp:bpb48t	; 48tpi drives
	dw	offset datagrp:bpb96t;	; 96tpi drives
	dw	offset datagrp:bpb35	; 3.5" drives
	dw	offset datagrp:bpb35	; unused 8" diskette	DUMMY m037
	dw	offset datagrp:bpb35	; unused 8" diskette 	DUMMY m037
	dw	offset datagrp:bpb35	; used for hard disk	DUMMY m037
	dw	offset datagrp:bpb35	; used for tape drive	DUMMY m037
	dw	offset datagrp:bpb35	; FFOTHER		DUMMY m037
	dw	offset datagrp:bpb35	; ERIMO			DUMMY m037
	dw	offset datagrp:bpb288	; 2.88MB drive		      m037

;	entry point to call utility functions in Bios_Code.  At this time,
;	  we aren't doing any A20 switching.  During MSINIT time Bios_Code
;	  will not yet be moved to its final resting place, so we know
;	  it'll be low.
;
;	to use this function, do a "push cs" and load bp with the offset of
;	  the function you want to call in Bios_Code.  This routine will
;	  push the address of a retf in Bios_Code onto the stack which
;	  will get executed when the utility function finishes.  It will
;	  then transfer control to Bios_Code:bp using a couple of pushes
;	  and a retf

	extrn	cdev:dword

addr_of_bcretf	dw	bc_retf

call_bios_code proc near
	assume	ds:nothing, es:nothing

	push	addr_of_bcretf		; set up near return to far return
ifndef ROMDOS
	push	word ptr [cdev].2	; push Bios_Code segment
else
	push	BIOS_Res		; addr of ROM code segment
endif
	push	bp			; save offset of utility function

	retf				; far jump to BIOS code

call_bios_code endp


flp_drvs	db	0


;===========================================================================
;
; entry from boot sector.  the register contents are:
;
;   dl = int 13 drive number we booted from
;   ch = media byte
;   bx = first data sector on disk.
;   ax = first data sector (high)
;   di = sectors/fat for the boot media.
;
	public	init
init	proc	near
	assume	ds:nothing,es:nothing

;
;M043; Begin changes
; This code has been added for IBM. It checks for IBM hardware and refuses
;to boot if the machine is not a true-blue IBM.
;

IF	0
ifdef 	IBM_VERSION

COPYR_LEN	equ	31

	push	ax
	push	bx
	push	cx
	push	di

	mov	ax,0fac0h
	mov	es,ax			; es = ROM Copyright segment
	mov	di,09bh		; es:di points at copyright
	push	cs
	pop	ds
	mov	si,offset datagrp:CopyrightIBM	; ds:si = copyright string
	mov	cx,COPYR_LEN		; length of string
	cld
	repz	cmpsb
	jz	isIBM			; copyright compared, IBM

;
;Check for IBM Basic Lite
;
 	mov	ah,22h
	int	15h
	jc	NotIBM			; not an IBM machine

	or	ah,ah
	jz	isIBM			; is an IBM Basic Lite

NotIBM:
	mov	si,offset datagrp:IBMBootFailMsg	;
	mov	bx,7			; setup display page
shIBMmsg:
	lodsb
	or	al,al
	jz	IBMstall		; done with message, stall

	mov	ah,14
	int	10h			; write teletype
	jmp	short shIBMmsg

IBMstall:
	jmp	short IBMstall		;hang 

CopyrightIBM	db	'The IBM Personal Computer Basic'

	include msbio.cl8		; the boot fail message

isIBM:					; IBM machine, continue init
	pop	di
	pop	cx
	pop	bx
	pop	ax

endif
ENDIF

;
;M043; End changes
;

	cli
	push	ax
	xor	ax,ax
	mov	ds,ax
	pop	ax

IFDEF	ROMDOS
;
;	If we are booting ROMDOS ax will contain a segment address
;	of a ROM BLOCK whcih has a proper signature and check sum.
;	Word at 70:0 needs to be initialized to this segment address
;	to satisfy Windows 3.0 enhanced mode.
;
	mov	word ptr ds:[700h], ax
ENDIF

; msload will check the extended boot record and set ax, bx accordingly.

;	msload passes a 32 bit sector number hi word in ax and low in bx
;	save this in cs:bios_h and cs:bios_l. this is for the start of
;	data sector of the bios.

	mov	cs:bios_h,ax
	mov	cs:bios_l,bx

; with the following information from msload, we don't need the
;     boot sector any more.-> this will solve the problem of 29 kb size
;     limitation of msbio.com file.
;
;

; Save a peck of interrupt vectors...

	push	cs
	pop	es			; cannot use cs override for stos

	push	cx
	push	di
;
;   This is the best point to display the "Starting MS-DOS..." message,
;   and it's also the earliest (MSLOAD doesn't count, because that's a
;   separable module! -JTP)
;
        push    ds
        mov     ax,sysinitseg
        mov     ds,ax
        mov     SI,OFFSET sysinitseg:StartMsg
@@:     lodsb
	or	AL,AL
        jz      @F
        mov     AH,0Eh
        mov     BX,0007h                ; "normal" attribute and page
	int	10h			; video write
        jmp     @B
@@:     pop     ds

	mov	cx, NUMROMVECTORS     	; no. of rom vectors to be saved
	mov	si, offset RomVectors	; point to list of int vectors
next_int:				
	lods	byte ptr cs:[si]	; get int number
	cbw				; assume < 128
	shl	ax, 1
	shl	ax, 1			; int no * 4
	mov	di, ax
	xchg	si, di
	lodsw
	stosw
	lodsw
	stosw				; save the vector
	xchg	si, di
	loop	next_int
	pop	di
	pop	cx

;   we need to save int13 in two places in case we are running on an at.
; on ats we install the ibm supplied rom_bios patch which hooks
; int13 ahead of orig13.  since int19 must unhook int13 to point to the
; rom int13 routine, we must have that rom address also stored away.

	mov	ax, word ptr Old13	; save old13 in orig13 also
	mov	word ptr Orig13, ax
	mov	ax, word ptr Old13+2
	mov	word ptr Orig13+2, ax

; set up int 13 for new action

	mov	word ptr ds:[13h*4],offset block13
	mov	ds:[13h*4+2],cs

; set up int 15 for new action				; M036

	mov	word ptr ds:[15h*4],offset Int15	; M036
	mov	ds:[15h*4+2],cs				; M036



; set up int 19 for new action

	mov	word ptr ds:[19h*4],offset int19
	mov	ds:[19h*4+2],cs

	sti
	int	11h			;get equipment status

; we have to support a system that does not have any diskette
;drives but only hardfiles.  this system will ipl from the hardfile.
;if the equipment flag bit 0 is 1, then the system has diskette drive(s).
;otherwise, the system has only hardfiles.
;
;important thing is that still, for compatibility reason, the drive letter
;for the hardfiles start from "c".  so, we still need to allocate dummy bds
;drive a and drive b.	at sysinit time, we are going to set cds table entry
;of dpb pointer for these drives to 0, so any user attempt to access this
;drives will get "invalid drive letter ..." message.  we are going to
;establish "fakefloppydrv" flag.  ***sysinit module should call int 11h to
;determine whether there are any diskette drivers in the system or not.!!!***

;	check the register returned by the equipment determination interrupt
;	we have to handle the case of no diskettes in the system by faking
;	two dummy drives.
;
;	if the register indicates that we do have floppy drives we don't need
;	to do anything special.
;
;	if the register indicates that we don't have any floppy drives then
;	what we need to do is set the fakefloppydrv variable, change the
;	register to say that we do have floppy drives and then go to execute
;	the code which starts at notsingle.  this is because we can skip the
;	code given below which tries to find if there are one or two drives
;	since we already know about this.


	if	ibmcopyright

;************************************************************************
; p????
;
; the following fix for lloyds of london does work correctly but it
; causes a regression in the ripl machine. machines with no ipl
; diskette will have ax bit 0 = 0. the cds structure is stomped on
; later to insure that a: is a invalid drive. this is where ripl fails.
; it wants the a: drive to be valid because it intercepts requests
; to the drive and returns info from it's memory image. but, if ibmbio
; finds no ipl drive, it tells ibmdos never to request anything from
; that drive.
;
; for the meantime, we will take out the support for lloyds of london
; by setting up a false condition. (ie. ax=1).
; (they have a special build, dos 3.31 to use). architecture needs to
; get the ripl machine to set bit 0 to say that there is in fact an
; virtual ipl diskette.
;
; the next 4 lines of code should be removed when the ripl people have
; gotten their act together. the unique identifier is used in case in
; the future, we want to dynamically patch ibmbio to work on the lloyds
; machine. the program could be written to scan for:
;
;	ebh,03h,52h,50h,53h,0dh,01h
;	 "jmp"	"R" "P" "S"  or  1
;
; and dynamically change the 0dh,01h to 90h,90h. note: that this string
; must be changed twice in ibmbio.com.
;************************************************************************

	jmp    short set_bit		; jmup over db
	db     "RPS"			; unique identifier

set_bit:
	or     ax,1			; turn on the ipl bit
;************************************************************************

	endif


	test	ax,1			; floppy drives present ?
	jnz	normalfloppydrv 	;yes.
;
; Some ROM BIOSs lie that there are no floppy drives. Lets find out
; whether it is an old ROM BIOS or a new one
;
; WARNING !!!
;
; This sequence of code is present in SYSINIT1.ASM also. Any modification
; here will require an equivalent modification in SYSINIT1.ASM also
;
	push	ax
	push	bx
	push	cx
	push	dx
	push	di
	push	es
	
	mov	ah, 8			; get disk parameters
	mov	dl, 0			; of drive 0.
	int	13h

	jc	@f

	mov	flp_drvs, dl
@@:
	pop	es
	pop	di
	pop	dx
	pop	cx
	pop	bx
	pop	ax

	jc	normalfloppydrv		; if error it is an old ROM BIOS
					;  so, lets assume that ROM BIOS lied

	cmp	flp_drvs, 0		; number of drvs == 0?
	jz	@f
	mov	al, flp_drvs
	dec	al			; make it zero based
	jmp	short got_num_flp_drvs
@@:
	mov	fakefloppydrv,1 	;we don't have any floppy drives.
	mov	ax,1			;after setting fakefloppydrv flag, we
	jmp	short settwodrive	; well then set it for two drives!

normalfloppydrv:			;yes, bit 0 is 1. there exist floppy drives.
	rol	al,1			;put bits 6 & 7 into bits 0 & 1
	rol	al,1
got_num_flp_drvs:
	and	ax,3			;only look at bits 0 & 1
	jnz	notsingle		;zero means single drive system
	inc	ax			;pretend it's a two drive system

settwodrive:				; set this to two fakedrives
	inc	cs:single		;remember this

notsingle:
	inc	ax			;ax has number of drives, 2-4
					;is also 0 indexed boot drive if we
					;  booted off hard file
	mov	cl,al			;ch is fat id, cl # floppies

; determine whether we booted from floppy or hard disk...

ifndef ROMDOS

	test	dl,80h			;boot from floppy ?
	jnz	gothrd			;no.

else

	mov	dx, [BootFlags]		; get boot flags
	and	dx, BF_DefaultMask	; mask off default drive indicator
	cmp	dx, BF_DefFloppy	; is it floppy disk?
	jne	gothrd			; no.

endif

	xor	ax,ax			;indicate boot from drive a
gothrd:

;   ax = 0-based drive we booted from
;   bios_l, bios_h set.
;   cl = number of floppies including fake one
;   ch = media byte

	xor	dx,dx
	cli
	mov	ss,dx
	mov	sp,700h 		;local stack
	sti
	assume	ss:nothing

	push	cx			;save number of floppies and media byte
	mov	ah,ch			;save fat id to ah
	push	ax			;save boot drive number, and media byte

; let model_byte, secondary_model_byte be set here!!!

	mov	ah,0c0h 		; return system environment
	int	15h			; call rom-bios routine

	jc	no_rom_system_conf	; just use model_byte
	cmp	ah,0			; double check
	jne	no_rom_system_conf

	mov	al,es:[bx.bios_sd_modelbyte] ;get the model byte
	mov	[model_byte],al
	mov	al,es:[bx.bios_sd_scnd_modelbyte] ;secondary model byte
	mov	[secondary_model_byte],al
	jmp	short turn_timer_on

no_rom_system_conf:
	mov	si,0ffffh
	mov	es,si
	mov	al,es:[0eh]		; get model byte
	mov	model_byte,al		; save model byte

turn_timer_on:
	mov	al,eoi
	out	akport,al		;turn on the timer

;	some olivetti m24 machines have an 8530 serial communications
;	  chip installed at io address 50h and 52h.  if we're running
;	  on one of those, we must inhibit the normal aux port initialization

	cmp	model_byte,0		; next to last byte in rom bios
	jnz	not_olivetti_m24	; skip for all other machines

;	now we know we're on an m24.  read the configuration switch to
;	  see if we think we have an 8530 chip.

	in	al,66h
	test	al,20h			; is 8530 installed?
	jz	not_olivetti_m24	;  we're done if not

;	now double check to make sure the device is really there

	mov	al,0fh
	out	50h,al
	in	al,50h
	test	al,1			; this test was copied from olivetti
	jz	skip_aux_port_init	; take this branch if 8530 installed

not_olivetti_m24:

	mov	al,3		; init com4
	call	aux_init
	mov	al,2		; init com3
	call	aux_init
	mov	al,1		; init com2
	call	aux_init
	xor	al,al		; init com1
	call	aux_init

skip_aux_port_init:
	mov	al,2		; init lpt3
	call	print_init
	mov	al,1		; init lpt2
	call	print_init
	xor	al,al		; init lpt1
	call	print_init

	xor	dx,dx
	mov	ds,dx		; to initialize print screen vector
	mov	es,dx

	xor	ax,ax
	mov	di,initspot
	stosw			; init four bytes to 0
	stosw

	mov	ax,cs		; fetch segment

	mov	ds:word ptr brkadr,offset cbreak ;break entry point
	mov	ds:brkadr+2,ax		;vector for break

	mov	ds:word ptr chrout*4,offset outchr
	mov	ds:word ptr chrout*4+2,ax

	mov	di,4
	mov	bx,offset intret	;will initialize rest of interrupts
	xchg	ax,bx
	stosw				;location 4
	xchg	ax,bx
	stosw				;int 1	;location 6
	add	di,4
	xchg	ax,bx
	stosw				;location 12
	xchg	ax,bx
	stosw				;int 3	;location 14
	xchg	ax,bx
	stosw				;location 16
	xchg	ax,bx
	stosw				;int 4	;location 18

	mov	ds:word ptr 500h,dx	;set print screen & break =0
	mov	ds:word ptr lstdrv,dx	;clean out last drive spec


;	we need to initalize the cs:motorstartup variable from the disk
;	parameter table at sec9.  the offsets in this table are defined in
;	the disk_parms struc in msdskprm.inc.  2 locs

	mov	al,ds:sec9 + disk_motor_strt
	mov	cs:motorstartup,al
	cmp	model_byte,0fdh 	; is this an old rom?
	jb	no_diddle		; no

	mov	word ptr ds:(sec9 + disk_head_sttl),0200h+normsettle
					; set head settle and motor start
					; on pc-1 pc-2 pc-xt hal0

	mov	ds:(sec9 + disk_specify_1),0dfh
					; set 1st specify byte
					; on pc-1 pc-2 pc-xt hal0
no_diddle:
	int	12h			;get memory size--1k blocks in ax
	mov	cl,6
	shl	ax,cl			;convert to 16-byte blocks(segment no.)
	pop	cx			; retreive boot drive number, and fat id (pushed long ago)
	mov	drvfat,cx		;save drive to load dos, and fat id

	push	ax			; save real top of memory

;M068 - BEGIN
;
;------ Check if an RPL program is present at TOM and do not tromp over it
;
	push	ds
	push	bx			; pushes not required but since this
					;  happens to be a last minute change
					;  & since it is only init code.

	xor	bx, bx
	mov	ds, bx
	mov	bx, ds:[2fh*4]
	mov	ds, ds:[2fh*4+2]
	cmp	word ptr ds:[bx+3], 'PR'
	jne	SkipRPL
	cmp	byte ptr ds:[bx+5], 'L'
	jne	SkipRPL

	mov	dx, ax			; get TOM into DX
	mov	ax, (multMULT shl 8) + multMULTRPLTOM
	int	2fh			; Get new TOM from any RPL
	mov	ax, dx
SkipRPL:

	pop	bx
	pop	ds
;
;M068 - END
;


ifndef ROMDOS
	sub	ax,64			; room for fatloc segment. (1 kb buffer)
	mov	fatloc,ax		; location to read fat
endif
	sub	ax,64			;room for boot record buffer segment (1 kb)
	mov	init_bootseg,ax
	pop	ax			; get back real top of memory for
					; sysinitseg

	mov	dx,sysinitseg
	mov	ds,dx

	assume	ds:sysinitseg

; set pointer to resident device driver chain

	mov	word ptr device_list,offset res_dev_list
	mov	word ptr device_list+2,cs

	mov	memory_size,ax
	inc	cl
	mov	default_drive,cl	;save default drive spec

ifndef ROMDOS
	mov	current_dos_location,dos_load_seg ; will load MSDOS here
else
	mov	ax, cs:DosDataSg	; get location of dos (set by rom-load)
	mov	current_dos_location, ax ; set current dos location
	mov	ax, cs:BIOS_Res		; get location of BIOS code
	mov	temp_bcode_seg, ax	; set current BIOS code location

	; have to call seg_reinit (in Bios_Code) to patch up pointer
	; in Bios_Data to Bios_Code.  can't do INT 13 until this is done!
	; this call takes Bios_Code segment in CX

	call	[seg_reinit_ptr]	; call seg_reinit in Bios_Code

	; note: this call trashes ES, DI, CX
endif

; important: some old ibm hardware generates spurious int f's due to bogus
; printer cards.  we initialize this value to point to an iret only if

; 1) the original segment points to storage inside valid ram.

; 2) the original segment is 0f000:xxxx


	mov	ax,sysinitseg		; ES is sysinitseg
	mov	es,ax

	xor	ax,ax			; ax := segment for int 15
	mov	ds,ax			; DS is int vector segment

	assume	es:sysinitseg, ds:nothing

	mov	ax,word ptr ds:(0fh*4+2)

	cmp	ax,es:memory_size	; condition 1
	jna	resetintf

	cmp	ax,0f000h		; condition 2
	jne	keepintf

resetintf:
	mov	word ptr ds:[0fh*4],offset intret
	mov	word ptr ds:[0fh*4+2],cs
keepintf:

; end important

ifdef	EXTENDEDKEY

; we will check if the system has ibm extended keyboard by
; looking at a byte at 40:96.  if bit 4 is set, then extended key board
; is installed, and we are going to set keyrd_func to 10h, keysts_func to 11h
; for the extended keyboard function. use cx as the temporary register.

	xor	cx,cx
	mov	ds,cx
	assume	ds:nothing
	mov	cl,ds:0496h			; get keyboard flag
	test	cl,00010000b
	jz	org_key				; orginal keyboard
	mov	byte ptr keyrd_func,10h		; extended keyboard
	mov	byte ptr keysts_func,11h	; change for extended keyboard functions
org_key:

endif

;**************************************************************
;	will initialize the number of drives
;	after the equipment call (int 11h) bits 6&7 will tell
;	the indications are as follows:
;
;	bits	7	6	drives
;		0	0	1
;		0	1	2
;		1	0	3
;		1	1	4
;**************************************************************

	push	cs
	pop	ds
	push	cs
	pop	es

	assume	ds:datagrp, es:datagrp

IFDEF	POWER		; If power management device driver is part of
			; of BIOS, initialize clock through it.  ;M074

	extrn	clock_init:far
	call	clock_init	; initialize power management clock ;M074
	mov	havecmosclock,1	; assume cmos clock

ELSE	; NOT POWER

	call	cmos_clock_read ; if cmos clock exists,
				;then set the system time according to that.
				;also, reset the cmos clock rate.
ENDIF	; NOT POWER

ifndef	ROMDOS
	mov	hdrv_pat,offset harddrv ;set up pointer to harddrv
					; in case of ROMDOS it is already
					; initialized to a ROM segment.
endif

	pop	ax		;number of floppies and fat id (pushed long ago)
	xor	ah,ah		; chuck fat id byte
;M011	mov	hardnum,al	; remember which drive is hard disk
	mov	drvmax,al	;and set initial number of drives

	mov	dsktnum,al	; and set initial number of drives

	shl	ax,1		;two bytes per address

;M011 -- begin changed section

	add	[last_dskdrv_table],ax

;M011 -- end changed section

;  The following code is to support dual hard disk controllers on
;  Compaq systems ('mode 2').
;
;  Here we attempt to setup INT 13h, mode 2
;  We must execute this code before any INT 13h to fixed disks

	push	ds
	mov	ax,0f000h		; point to ROM BIOS
	mov	ds,ax

	cmp	word ptr ds:[0ffeah],'OC'	; look for COMPAQ
	jnz	skip_mode2
	cmp	word ptr ds:[0ffech],'PM'
	jnz	skip_mode2
	cmp	word ptr ds:[0ffeeh],'QA'
	jnz	skip_mode2

	mov	ax,0E400h	;return advanced system info
	int	15h		;Q: function supported?
	jc	skip_mode2	; N: skip setting mode 2
	or	bx,00040h	; Y: enable mode 2
	mov	ax,0E480h	;    set advanced system info
	int	15h
skip_mode2:
	pop	ds

	mov	dl,80h		; tell rom bios to look at hard drives
	mov	ah,8h		; set command to get drive parameter
	int	13h		; call rom-bios to get number of drives
	jc	enddrv		;carry indicates old rom, so no hardfile

;M011 -- deleted garbage about treating first two drives as
;	special case

	mov	hnum,dl		; save number of hard drives

enddrv:

; scan the list of drives to determine their type.  we have three flavors of
; diskette drives:
;
;   48tpi drives    we do nothing special for them
;   96tpi drives    mark the fact that they have changeline support.
;   3.5"  drives    mark changeline support and small.
;
; the following code uses registers for certain values:
;
;   dl - physical drive
;   ds:di - points to current bds
;   cx - flag bits for bds
;   dh - form factor for the drive (1 - 48tpi, 2 - 96tpi, 3 - 3.5" medium)

	xor	dl,dl			; start out with drive 0.
	push	cs
	pop	ds
	assume	ds:datagrp

	mov	eot,9
	mov	di,offset start_bds

; check if the system has no physical diskette drives.
; if it is, then we don't have to set bds tables.  but since we
; pretend that we have 2 floppies, we are going to reserve two
; bds tables for the fake drive a, and b. and set the end of link
; pointer.

;*********************************************************
;	check to see if we are faking floppy drives.  if not we don't
;	do anything special.  if we are faking floppy drives we need
;	to set aside two bdss for the two fake floppy drives.  we
;	don't need to initalise any fields though. so starting at start_bds
;	use the link field in the bds structure to go to the second bds
;	in the list and initalise it's link field to -1 to set the end of
;	the list.  then jump to the routine at dohard to allocate/initialise
;	the bds for harddrives.

	cmp	fakefloppydrv,1
	jnz	loop_drive		; system has floppy

	mov	di,word ptr ds:[di].bds_link	; di <- first bds link
	mov	di,word ptr ds:[di].bds_link	; di <- second bds link
	mov	word ptr ds:[di].bds_link,-1	; set end of link
	jmp	dohard			; allocate/initialise bds for harddrives
loop_drive:
	cmp	dl,drvmax
	jb	got_more
	jmp	done_drives
got_more:
	xor	cx,cx			; zero all flags
	mov	di,word ptr ds:[di].bds_link	; get next bds
	mov	dh,ff48tpi		; set form factor to 48 tpi
	mov	num_cyln,40		; 40 tracks per side

	push	ds
	push	di
	push	dx
	push	cx
	push	es

	mov	ah,8			;get drive parameters
	int	13h			;call rom-bios
	jc	noparmsfromrom		; got an old rom

; if cmos is bad, it gives es,ax,bx,cx,dh,di=0. cy=0.
;in this case, we are going to put bogus informations to bds table.
;we are going to set ch=39,cl=9,dh=1 to avoid divide overflow when
;they are calculated at the later time.  this is just for the diagnostic
;diskette which need msbio,msdos to boot up before it sets cmos.
;this should only happen with drive b.

	cmp	ch,0			; if ch=0, then cl,dh=0 too.
	jne	pfr_ok

	mov	ch,39			; rom gave wrong info.
	mov	cl,9			; let's default to 360k.
	mov	dh,1

pfr_ok:
	inc	dh			; make number of heads 1-based
	inc	ch			; make number of cylinders 1-based
	mov	num_heads,dh		; save parms returned by rom
	and	cl,00111111b		; extract sectors/track
	mov	sec_trk,cl
	mov	num_cyln,ch		; assume less than 256 cylinders!!

; make sure that eot contains the max number of sec/trk in system of floppies

	cmp	cl,eot			; may set carry
	jbe	eot_ok
	mov	eot,cl
eot_ok:
	pop	es
	pop	cx
	pop	dx
	pop	di
	pop	ds

; check for changeline support on drive

	mov	ah,15h 		; set command to get dasd type
	int	13h			; call rom-bios
	jc	changeline_done

	cmp	ah,02			; check for presence of changeline
	jne	changeline_done

; we have a drive with change line support.

	or	cl,fchangeline		; signal type
	mov	fhave96,1		; remember that we have 96tpi disks

; we now try to set up the form factor for the types of media that we know
; and can recognise. for the rest, we set the form factor as "other".

changeline_done:

; 40 cylinders and 9 or less sec/trk, treat as 48 tpi medium.

	cmp	num_cyln,40
	jnz	try_80

	cmp	sec_trk,9
	jbe	nextdrive

gotother:
	mov	dh,ffother		; we have a "strange" medium
	jmp	short nextdrive

; 80 cylinders and 9 sectors/track => 720 kb device
; 80 cylinders and 15 sec/trk => 96 tpi medium

try_80:
	cmp	num_cyln,80
	jnz	gotother

	mov	dh, ff288		; assume 2.88 MB drive	m037
	cmp	sec_trk, 36		; is it ?		m037
	je	nextdrive		; yeah, go update	m037
	
	cmp	sec_trk,15
	jz	got96

	cmp	sec_trk,9
	jnz	gotother

	mov	dh,ffsmall
	jmp	short nextdrive

got96:
	mov	dh,ff96tpi
	jmp	short nextdrive

; we have an old rom, so we either have a 48tpi or 96tpi drive. if the drive
; has changeline, we assume it is a 96tpi, otherwise we treat it as a 48tpi.

noparmsfromrom:
	pop	es
	pop	cx
	pop	dx
	pop	di
	pop	ds

	mov	ah,15h			; set command to get dasd type
	int	13h			; call rom-bios

	jc	nextdrive
	cmp	ah,2			; is there changeline?
	jnz	nextdrive

	or	cl,fchangeline
	mov	fhave96,1		; remember that we have 96tpi drives
	mov	num_cyln,80
	mov	dh,ff96tpi
	mov	al,15			; set eot if necessary
	cmp	al,eot
	jbe	eot_ok2
	mov	eot,al
eot_ok2:

nextdrive:
	or	cl,fi_own_physical	; set this true for all drives
	mov	bh,dl			;save int13 drive number

; we need to do special things if we have a single drive system and are setting
; up a logical drive. it needs to have the same int13 drive number as its
; counterpart, but the next drive letter. also reset ownership flag.
; we detect the presence of this situation by examining the flag single for the
; value 2.

	cmp	single,2
	jnz	not_special
	dec	bh			; int13 drive number same for logical drive
	xor	cl,fi_own_physical	; reset ownership flag for logical drive
not_special:

; the values that we put in for BDS_RBPB.BPB_HEADS and
;  BDS_RBPB.BPB_SECTORSPERTRACK will only remain if the
;  form factor is of type "ffother".

	xor	ax,ax
	mov	al,num_heads
	mov	ds:[di].BDS_RBPB.BPB_HEADS,ax
	mov	al,sec_trk
	mov	ds:[di].BDS_RBPB.BPB_SECTORSPERTRACK,ax
	mov	ds:[di].bds_flags,cx
	mov	ds:[di].bds_formfactor,dh
	mov	ds:[di].bds_drivelet,dl
	mov	ds:[di].bds_drivenum,bh
	mov	bl,byte ptr num_cyln
	mov	byte ptr ds:[di].bds_ccyln,bl	; only the l.s. byte is set here
	cmp	single,1		; special case for single drive system
	jnz	no_single

	mov	single,2		; don't lose info that we have single system
	or	cx,fi_am_mult
	or	ds:[di].bds_flags,cx
	mov	di,word ptr ds:[di].bds_link	; move to next bds in list
	inc	dl
	jmp	short nextdrive 	; use same info for bds a previous

no_single:
	inc	dl
	jmp	loop_drive

done_drives:
	mov	word ptr ds:[di].bds_link,-1	; set link to null

; set up all the hard drives in the system

dohard:

;M011 -- begin changed section

	mov	dh,hnum
	or	dh,dh			; done if no hardfiles

	jz	static_configure	; M038

	mov	dl,80h

dohard1:
	push	dx

	mov	di,end_of_bdss
	mov	bl,drvmax
	mov	bh,0			; first primary partition (or active)
	call	sethard
	jc	hardfile_err

	call	dmax_check		; M029  error if already 26 drives
	jnc	hardfile_err		; M029

;M038	lea	bx,[di].BDS_BPB
;M038	mov	si,[last_dskdrv_table]
;M038	mov	word ptr [si],bx
;M038	add	[last_dskdrv_table],2

	call	xinstall_bds		; M038 insert new bds into linked list

;M038	inc	drvmax
;M038
;M038	add	end_of_bdss,size BDS_STRUC

hardfile_err:
	pop	dx
	inc	dl			; next hard drive
	dec	dh
	jnz	dohard1


;M011 -- end changed section


; end of physical drive initialization.
; *** do not change the position of the following statement.
; *** domini routine will use [drvmax] value for the start of the logical
; *** drive number of mini disk(s).

	call	domini			;for setting up mini disks, if found


;M018 -- begin added section

	mov	dh,hnum		; we already know this is >0

	mov	dl,80h

dohardx1:
	mov	bh,1			; do all subsequent primary partitions

dohardx2:
	push	dx
	push	bx
	mov	di,end_of_bdss
	mov	bl,drvmax
	call	sethard
	jc	dohardx4		; move to next hardfile if error

	call	dmax_check		; M029 -- make sure <=26 drives
	jnc	dohardx4		; M029 -- skip if error

;M038	lea	bx,[di].BDS_BPB
;M038	mov	si,[last_dskdrv_table]
;M038	mov	word ptr [si],bx
;M038	add	[last_dskdrv_table],2

	call	xinstall_bds		; M038 insert new bds into linked list

;M038	inc	drvmax
;M038	add	end_of_bdss,size BDS_STRUC

	pop	bx			; get partition number
	pop	dx			; restore physical drive counts
	inc	bh
	jmp	dohardx2		; keep looping until we fail

dohardx4:
	pop	bx			; unjunk partition number from stack
	pop	dx			; restore physical drive counts

	inc	dl			; next hard drive
	dec	dh
	jnz	dohardx1

;M018 -- end changed section

;******************************************************************************
; if more than 2 diskette drives on the system, then it is necessary to remap
; the bds chain to adjust the logical drive num (driver letter) with greater
; than two diskette drives
;
; new scheme:	if more than 2 disktte drives, first map the bds structure
;		as usuall and then rescan the bds chain to adjust the  drive
;		letters.  to do this, scan for disk drives and assign logical
;		drive number starting from 2 and then rescan diskette drives
;		and assign next to the last logical drive number of last disk
;		drive to the 3rd and 4th diskette drives.
;******************************************************************************

	cmp	dsktnum,2	; >2 diskette drives
	jbe	no_remap	; no - no need for remapping

	call	remap		; remap bds chain to adjust driver letters

no_remap:

	assume	es:nothing

; end of drive initialization.

; we now decide, based on the configurations available so far, what
;code or data we need to keep as a stay resident code.	the following table
;shows the configurations under consideration.	they are listed in the order
;of their current position memory.
;
;configuration will be done in two ways:
;
;first, we are going to set "static configuration".  static configuration will
;consider from basic configuration to endof96tpi configuration.  the result
;of static configuration will be the address the dynamic configuration will
;use to start with.
;
;secondly, "dynamic cofiguration" will be performed.  dynamic configuration
;involves possible relocation of code or data.	dynamic configuration routine
;will take care of bdsm tables and at rom fix module thru k09 suspend/resume
;code individually.  after these operation, [dosdatasg] will be set.
;this will be the place sysinit routine will relocate msdos module for good.

;M011 -- begin changed section

;
;   1.	 basic configuration for msbio (endfloppy)
;   2.   end96tpi	; a system that supports "change line error"
;   3.	 end of bdss	; end of bdss for hard disks
;   4.	 endatrom	;some of at rom fix module.
;   5.	 endcmosclockset;supporting program for cmos clock write.
;   6.	 endk09 	;k09 cmos clock module to handle suspend/resume operation.
;


static_configure:

	mov	di,end_of_bdss
	cmp	di,offset bdss		; did we allocate any hard drive bdss?
	jnz	dynamic_configure	; that's the end, then

	mov	di,offset end96tpi	; keep everything up to end96tpi
	cmp	fhave96,0		;is change line support there?
	jnz	dynamic_configure	;yes.

	mov	di,offset endfloppy

dynamic_configure:
	push	cs
	pop	es			;es -> datagrp
	assume	es:datagrp
	cld				;clear direction

;M011 -- end changed section

	cmp	model_byte,0fch		;at ?
	jnz	checkcompaqbug		; M015
	cmp	hnum,0			;no hard file?
	jz	checkcompaqbug		; M015

	xchg	ax,di			; save allocation pointer in ax
	mov	si,0f000h
	mov	es,si			;es -> bios segment
	assume	es:nothing

	mov	si,offset datagrp:bios_date ; only patch rom with this date
	mov	di,0fff5h		;  rom bios string is at f000:fff5
	mov	cx,bdate_l
	repz	cmpsb			; check for date + zero on end
	xchg	ax,di			; restore allocation pointer

;	M015 -- begin changes

	jnz	checkcompaqbug

;	install at rom fix

	mov	cx,offset datagrp:endatrom
	mov	si,offset datagrp:ibm_disk_io
	jmp	short install_int13_patch

;	M065 -- begin changes
;
;	On certain systems with Western Digital disk controllers, the
;	following detection scheme caused an unpredictable and serious
;	failure.  In particular, they've implemented a nonstandard
;	Int13(ah=16h) which reconfigures the hard drive, depending on
;	what happens to be at es:[bx] and other memory locations indexed
;	off of it.
;
;	Compaq was unable to tell us exactly which kind of systems have
;	the bug, except that they guarantee that the bug was fixed in
;	ROM BIOSs dated 08/04/86 and later.  We'll check for the COMPAQ
;	string, and then look for date codes before 08/04/86 to decide
;	when to install the hook.

checkcompaqbug:
	mov	ax,0f000h		; point to ROM BIOS
	mov	es,ax

	cmp	es:word ptr [0ffeah],'OC'	; look for COMPAQ
	jnz	not_compaq_patch
	cmp	es:word ptr [0ffech],'PM'
	jnz	not_compaq_patch
	cmp	es:word ptr [0ffeeh],'QA'
	jnz	not_compaq_patch

;	We're running on a COMPAQ.  Now look at the date code.

	mov	ax,es:word ptr [0fffbh]	; get year
	xchg	ah,al
	cmp	ax,'86'			; is it 86?
	ja	not_compaq_patch
	jb	do_compaq_patch

	mov	ax,es:word ptr [0fff5h]	; get month
	xchg	ah,al
	cmp	ax,'08'			; is it 08?
	ja	not_compaq_patch
	jb	do_compaq_patch

	mov	ax,es:word ptr [0fff8h]	; get day
	xchg	ah,al
	cmp	ax,'04'			; is it 04?
	jae	not_compaq_patch	; 08/04/86 or later needs no patch

do_compaq_patch:

;	M065 -- end additions

;M065	
;M065	;	on some Compaq '286 systems there's a ROM BIOS bug.  When an
;M065	;	  int13 is executed with ah>15h and dl>=80h, the byte at
;M065	;	  DS:74h is trashed.  If we detect that bug in the ROM, insert
;M065	;	  an int13 hook which always points ds->ROM DATA AREA on those
;M065	;	  calls.
;M065	
;M065	checkcompaqbug:
;M065	
;M065		push	ds
;M065		mov	ds,init_bootseg		; point to our scratch buffer
;M065		assume	ds:nothing
;M065	
;M065		mov	ds:byte ptr 74h,0ffh	; stick a known value at 74h
;M065		mov	ah,16h			; check disk change
;M065		mov	dl,80h			; on any hard disk
;M065	
;M065	;	we can't just do a straight int 13 here or the damn bug will
;M065	;	 trash something in our Bios Data, since our int13 hook
;M065	;	 has ds->bios_data when it calls the ROM.
;M065	
;M065		xor	si, si			; AT&T 6300 WGS goofs it up
;M065						;  if si != 0			M021
;M065	
;M065		pushf				; simulate int 13
;M065		call	orig13			; call directly into the ROM
;M065	
;M065		cmp	ds:byte ptr 74h,0ffh	; if it changed in error, COMPAQ bug
;M065		pop	ds
;M065		assume	ds:datagrp
;M065		jz	checkcmosclock		; brif no ROM BIOS bug
;M065	

;	insert patch for COMPAQ rom

	mov	cx,offset datagrp:end_compaq_i13hook
	mov	si,offset datagrp:compaq_disk_io

install_int13_patch:
	push	cs
	pop	es			;set es to datagrp seg

	mov	word ptr orig13,di
	mov	word ptr orig13+2,cs	;set new rom bios int 13 vector
	sub	cx,si			;size of rom fix module
	rep	movsb			;relocate it

;	M015 -- end changes

not_compaq_patch:			; M065
checkcmosclock:
	push	cs
	pop	es
	assume	es:datagrp
	cmp	havecmosclock,1 		;cmos clock exists?
	jne	checkk09
	mov	word ptr daycnttoday,di		;set the address for mschar
	mov	cx,offset enddaycnttoday - offset daycnt_to_day
	mov	si,offset datagrp:daycnt_to_day
	rep	movsb

	mov	word ptr bintobcd,di		;set the address for msclock
;						; let original segment stay
	mov	cx,offset endcmosclockset - offset bin_to_bcd
	mov	si,offset datagrp:bin_to_bcd
	rep	movsb


checkk09:
	push	di			;save di??

;M055 - begin
	mov	ax,4101h		; wait for bh=es:[di]
	mov	bl,1			; wait for 1 clock tick
	mov	bh,byte ptr es:[di]
	stc				; Assume we will fail
	int	15h
;M055 - end

	pop	di
	jc	configdone

	mov	fhavek09,1		; remember we have a k09 type

IFNDEF	POWER				; M083
	push	ds			; preserve pointer to datagrp
	xor	ax,ax
	mov	ds,ax			; point DS to interrupt vectors
	assume	ds:nothing
	mov	word ptr ds:[4 * 6ch],di ; new int 6ch handler
	mov	ds:[4 * 6ch +2],cs
	pop	ds			; restore pointer to datagrp
	assume	ds:datagrp

	mov	si,offset datagrp:int6c
	mov	cx,(offset endk09 - offset int6c) ; size of k09 routine
	rep	movsb

;     set up config stuff for sysinit

ENDIF ; NOT POWER			; M083

configdone:				;di is final ending address of msbio.

IFDEF   ROMDOS
IFDEF   ROMDRIVE
;
;------ If ROMDRIVE is built in, check whether ROMDRIVE should be the default
;	drive. If so update 'default_drive' to the ROMDRIVE so that sysinit
;	will set default drive to ROMDRIVE
;
	mov	ax, sysinitseg
	mov	ds, ax
	assume	ds:sysinitseg
	mov	ax, BootFlags
	and	ax, BF_DefaultMask
	cmp	ax, BF_DefROM
	jne	NotRomDrive
	cmp	drvmax, 26
	jae	NotRomDrive
	mov	al, drvmax
	inc	al
	mov	default_drive, al
NotRomDrive:

ENDIF   ; ROMDRIVE
ENDIF   ; ROMDOS


	push	cs
	pop	ds
	assume	ds:datagrp

	add	di,15			; round to paragraph
	shr	di,1
	shr	di,1
	shr	di,1
	shr	di,1
	add	di,datagrp
	mov	[dosdatasg],di		; where the dos data segment will be

	mov	ax,drvfat		; get drive and fat id
	mov	bp,offset setdrive
	push	cs			; simulate far call
	call	call_bios_code		; get bds for drive

	mov	bp,offset getbp		; ensure valid bpb is present
	push	cs			; simulate far call
	call	call_bios_code

;	resort to funky old segment definitions for now

	push	es			; copy bds to ds:di
	pop	ds
	assume	ds:nothing

;	the following read of es:0000 was spurious anyway.  Should look into it.

;	hmmmmmm.  j.k. took out a call to getfat right here a while
;	  back.  Apparently it was what actually setup es: for the following
;	cas----

	xor	di,di
	mov	al,es:[di]		;get fat id byte
	mov	byte ptr drvfat+1,al	;save fat byte
	mov	ax,drvfat

;	cas -- why do a SECOND setdrive here????

	mapnew
	mov	bp,offset setdrive
	push	cs			; simulate far call
	call	call_bios_code		;get correct bds for this drive
	unmapnew


; Now we load in the MSDOS.SYS file, if this is not ROMDOS
ifndef ROMDOS
	
	mov	bx,ds:[di].BDS_BPB.BPB_BYTESPERSECTOR
	mov	cs:md_sectorsize,bx	;used by get_fat_sector proc.
	mov	bl,ds:[di].bds_fatsiz		; get size of fat on media
	mov	fbigfat,bl
	mov	cl,ds:[di].BDS_BPB.BPB_SECTORSPERCLUSTER	;get sectors/cluster

;	32 bit calculation

	mov	ax,word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS	;get number of hidden sectors (low)
	sub	bios_l,ax		;subtract hidden sectors since we
					; need a logical sector number that will
					; be used by getclus(diskrd procedure)
	mov	ax,word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS+2	;subtract upper 16 bits of sector num
	sbb	bios_h,ax

	xor	ch,ch			;cx = sectors/cluster

;	the boot program has left the directory at 0:500

	push	ds
	xor	di,di
	mov	ds,di			; es:di points to load location
	mov	bx,ds:word ptr [53ah]	;   clus=*53a;
	pop	ds

loadit:
	mov	ax,sysinitseg
	mov	es,ax
	assume	es:sysinitseg
	mov	es,current_dos_location
	assume	es:nothing

	call	getclus 		; read cluster at ES:DI (DI is updated)

iseof:
	test	fbigfat,fbig
	jnz	eofbig
	cmp	bx,0ff7h
	jmp	short iseofx
eofbig:
	cmp	bx,0fff7h
iseofx:
	jb	loadit			; keep loading until cluster = eof

endif	; end of non-ROMDOS conditional

	call	setdrvparms

	jmp	sysinit

init	endp

;==========================================================================
;
; Following are subroutines to support resident device driver initialization
;
;M011 -- note:  deleted setup_bdsms and reset_bdsms here

;

;	M035 -- begin changed section

;******************************************************************************
; module name: remap
;
; descriptive name: all the code for himem that could be separated from msbio
;
; function:  remap the bds chain to adjusted logical drive numbers (drive
;	     letters if more than two diskette drives on the system.
;
;     scheme:  if more than 2 disktte drives, first map the bds structure
;	       as usual and then rescan the bds chain to adjust the  drive
;	       letters.  to do this, scan for disk drives and assign logica
;	       drive number starting from 2 and then rescan diskette drives
;	       and assign next to the last logical drive number of last disk
;	       drive to the 3rd and 4th diskette drives.

; input:       none
; exit:	drive letters have been remapped in bds chain
; exit error:	none
; called from: msinit
;
; notes:  this function  will be called only if more than 2 diskettes are
;	  found in the system
;	  this function assumes that there are no more than 26 drives assigned
;	    this is guaranteed by the code that creates bdss for partitions
;	  this function assumes that the first entries in the chain are
;	   floppy drives, and all the rest are hard drives
;	  will alter the boot drive if necessary to reflect remapping
;
;******************************************************************************

remap	proc	near

	mov	di,word ptr start_bds	   ; get first bds

; search for 1st fixed disk physical drive num

drive_loop:
	cmp	ds:[di].bds_drivenum,80h ; first hard disk??
	je	fdrv_found		 ; yes - continue

	mov	di,word ptr ds:[di].bds_link ; get next bds, assume segment
	cmp	di,-1			; last bds?
	jnz	drive_loop		; loop if not

	jmp	short rmap_exit		; yes- no hard drive on system

;------------------------------------------------------------------------------
;first disk drive bds, now change the logical drive num to 2 and the subsequent
;logical drive nums to 3, 4, 5 etc.
;------------------------------------------------------------------------------

fdrv_found:
	mov	al,2			; start with logical drv num=2
fdrv_loop:
	mov	ds:[di].bds_drivelet,al ; found ??
	mov	di,word ptr ds:[di].bds_link	; ds:di--> next bds
	inc	al			; set num for next drive

	cmp	di,-1			; last hard drive ??
	jne	fdrv_loop		; no - assign more disk drives

;------------------------------------------------------------------------------
; now, rescan and find bds of 3rd floppy drive and assign next drive letter
; in al to 3rd. if the current drive letter is past z, then do not allocate
; any more.
;------------------------------------------------------------------------------


	mov   di,word ptr start_bds	   ; get first bds
	mov   di,word ptr ds:[di].bds_link ; ds:di-->bds2
	mov   ah,dsktnum		   ; get number of floppies to remap
	sub   ah,2			   ; adjust for a: & b:

remap_loop1:
	mov   di,word ptr ds:[di].bds_link ; ds:di -> bds to change
	mov   ds:[di].bds_drivelet,al      ; set new num to next floppy

	inc   al			; new number for next floppy
	dec   ah			; count down extra floppies
	jnz   remap_loop1		; and loop until we got 'em all

;	now we've got to adjust the boot drive if we reassigned it

	mov	al,byte ptr drvfat
	cmp	al,2			; is it a: or b:
	jb	rmap_exit

	sub	al,dsktnum		; is it one of the other floppies?
	jb	remap_boot_flop		;  brif so

;	we've got to remap the boot hard drive
;	  subtract the number of EXTRA floppies from it

	add	al,2			; bootdrv -= (dsktnum-2)
	jmp	short remap_change_boot_drv

;	we've got to remap the boot floppy.
;	  add the number of hard drive partitions to it

remap_boot_flop:
	add	al,drvmax		; bootdrv += (drvmax-dsktnum)

remap_change_boot_drv:
	mov	byte ptr drvfat,al	; alter msdos.sys load drive
	inc	al

	push	ds
	mov	di,sysinitseg
	mov	ds,di
	assume	ds:sysinitseg
	mov	default_drive,al	; pass it to sysinit as well
	pop	ds
	assume	ds:datagrp

rmap_exit:
	ret

remap	endp

;	M035 -- end changes


;**************************
;
; getboot - get the boot sector for a hard disk
;
; Reads the boot sector from a specified drive into a buffer at the top
; of memory.
;
; dl = int13 drive number to read boot sector for

getboot proc	near
	mov	ax,cs:init_bootseg	; prepare to load es
	mov	es,ax			; load es segment register
	assume	es:nothing
	mov	bx,bootbias		; load bx, es:bx is where sector goes
	mov	ax,0201h		; command to read & num sec. to 1
	xor	dh,dh			; head number zero
	mov	cx,0001h		; cylinder zero and sector one
	int	13h			; call rom bios
	jc	erret

	cmp	word ptr es:[bootbias+1feh],0aa55h ; dave litton magic word?
	jz	norm_ret
erret:
	stc
norm_ret:
	ret
getboot endp


;***************************************************************************
;   sethard - generate bpb for a variable sized hard file.  ibm has a
;   partitioned hard file; we must read physical sector 0 to determine where
;   our own logical sectors start.  we also read in our boot sector to
;   determine version number
;
;   inputs:	dl is rom drive number (80...)  M018
;		bh is partition number (0....)  M018
;		ds:di points to bds
;   outputs:	carry clear -> bpb is filled in
;		carry set   -> bpb is left uninitialized due to error
;	trashes (at least) si, cx
;	MUST PRESERVE ES:!!!!		; M011
;***************************************************************************


sethard proc	near
	assume	ds:datagrp,es:nothing

	push	di
	push	bx
	push	ds
	push	es		; M011
	mov	ds:[di].bds_drivelet,bl
	mov	ds:[di].bds_drivenum,dl
	or	ds:byte ptr [di].bds_flags,fnon_removable ; M011
	mov	ds:[di].bds_formfactor,ffhardfile
	mov	fbigfat,0		; assume 12 bit fat
	mov	dh,bh			; M018 - partition number
	push	dx
	mov	ah,8			; set command to get drive parameters
	int	13h			; call rom-bios disk routine

; dh is number of heads-1
; dl is number of hard disks attached
; low 6 bits of cl is sectors/track
; high 2 bits of cl with ch are max # of cylinders

	inc	dh			; get number of heads
	mov	byte ptr ds:[di].BDS_BPB.BPB_HEADS,dh
	pop	dx

	jc	setret			; error if no hard disk

	and	cl,3fh			; extract number of sectors/track
	mov	byte ptr ds:[di].BDS_BPB.BPB_SECTORSPERTRACK,cl
	push	dx			; M018 -- save partition number
	call	getboot 		;   if (getboot ())
	assume	es:nothing
	pop	dx			; M018 -- restore partition number
	jc	setret			;	return -1;
	mov	bx,1c2h+bootbias	;   p = &boot[0x1c2];

;	M018 -- begin changed section

;	The first 'active' partition is 00, the second is 01....
;	  then the remainder of the 'primary' but non-active partitions

act_part:
	test	BYTE PTR es:[bx - 4],80h  ;AN016;is the partition active?
	jz	not_act
	cmp	byte ptr es:[bx],1	; reject if partitiontype != 1, 4 or 6
	jz	got_good_act
	cmp	byte ptr es:[bx],4
	jz	got_good_act
	cmp	byte ptr es:[bx],6
	jnz	not_act			; reject!
got_good_act:
	or	dh,dh			; is this our target partition #?
	jz	set2			; WE GOT THE ONE WANTED!!
	dec	dh			; count down
not_act:
	add	bx,16			;AN016;next entry
	cmp	bx,202H + BOOTBIAS	;AN016;last entry done?
	jnz	act_part		;AN016;no,process next entry

;	Now scan the non-active partitions

	mov	bx,1c2h + BOOTBIAS	;AN016;restore original value of bx

get_primary:
	test	byte ptr es:[bx - 4],80h ; we've already scanned
	jnz	not_prim		; the ACTIVE ones
	cmp	byte ptr es:[bx],1	; see if partitiontype == 1, 4 or 6
	jz	got_prim
	cmp	byte ptr es:[bx],4
	jz	got_prim
	cmp	byte ptr es:[bx],6
	jnz	not_prim

got_prim:
	or	dh,dh			; is this our target partition?
	jz	set2
	dec	dh

not_prim:
	add	bx,16
	cmp	bx,202h+bootbias
	jnz	get_primary		; loop till we've gone through table

setret:
	stc				; error return
	jmp	ret_hard

;	M018 -- this concludes the changes.

;  until we get the real logical boot record and get the bpb,
;  BDS_BPB.BPB_BIGTOTALSECTORS will be used instead of BDS_BPB.BPB_TOTALSECTORS for the
;  convenience of the computation.
;
;  at the end of this procedure, if a bpb information is gotten from
;  the valid boot record, then we are going to use those bpb information
;  without change.
;
;  otherwise, if (hidden sectors + total sectors) <= a word, then
;  we will move BDS_BPB.BPB_BIGTOTALSECTORS (low) to BDS_BPB.BPB_TOTALSECTORS and zero out
;  BDS_BPB.BPB_BIGTOTALSECTORS entry to make
;  it a conventional bpb format.

set2:
	mov	cs:rom_drv_num, dl	; save the rom bios drive number we are handling now.

	mov	ax,word ptr es:[bx+4]	;hidden sectors
	mov	dx,word ptr es:[bx+6]

;  decrement the sector count by 1 to make it zero based. exactly 64k
;   sectors should be allowed

	sub	ax,1
	sbb	dx,0

	add	ax,word ptr es:[bx+8]	;sectors in partition
	adc	dx,word ptr es:[bx+10]
	jnc	okdrive
	or	fbigfat,ftoobig
okdrive:
	mov	ax,word ptr es:[bx+4]

	mov	word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS,ax	;   BPB_HIDDENSECTORS = p->partitionbegin;
	mov	ax,word ptr es:[bx+6]
	mov	word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS+2,ax

	mov	dx,word ptr es:[bx+10]	; # of sectors (high)
	mov	ax,word ptr es:[bx+8]	;# of sectors (low)
	mov	word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS+2,dx
	mov	word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS,ax ;   bpb->maxsec = p->partitionlength;
	cmp	dx,0
	ja	okdrive_cont

	cmp	ax,64			;   if (p->partitionlength < 64)
	jb	setret			;	return -1;

okdrive_cont:
	mov	dx,word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS+2
	mov	ax,word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS	; boot sector number - for mini disk,;j.k.
	xor	bx,bx			;usually equal to the # of sec/trk.  ;j.k.
	mov	bl,byte ptr ds:[di].BDS_BPB.BPB_SECTORSPERTRACK
	push	ax
	mov	ax,dx
	xor	dx,dx
	div	bx
	mov	cs:[temp_h],ax
	pop	ax
	div	bx			;(sectors)dx;ax / (BDS_BPB.BPB_SECTORSPERTRACK)bx =(track) temp_h;ax + (sector)dx
	mov	cl,dl			; cl is sector number;j.k.assume sector number < 255.
	inc	cl			; sectors are 1 based

	xor	bx,bx
	mov	bl,byte ptr ds:[di].BDS_BPB.BPB_HEADS
	push	ax
	xor	dx,dx
	mov	ax,cs:[temp_h]
	div	bx
	mov	cs:[temp_h],ax
	pop	ax
	div	bx			; dl is head, ax is cylinder
	cmp	cs:[temp_h],0
	ja	setret_brdg		; exceeds the limit of int 13h
	cmp	ax,1024
	ja	setret_brdg		; exceeds the limit of int 13h

; dl is head.
; ax is cylinder
; cl is sector number (assume less than 2**6 = 64 for int 13h)

;*** for mini disks ***
	cmp	ds:[di].bdsm_ismini,1 		;check for mini disk
	jnz	oknotmini		;not mini disk.
	add	ax,ds:[di].bdsm_hidden_trks	;set the physical track number
oknotmini:
;*** end of added logic for mini disk

	ror	ah,1			; move high two bits of cyl to high
	ror	ah,1			; two bits of upper byte
	and	ah,0c0h 		; turn off remainder of bits
	or	cl,ah			; move two bits to correct spot
	mov	ch,al			; ch is cylinder

; cl is sector + 2 high bits of cylinder
; ch is low 8 bits of cylinder
; dl is head
; rom_drv_num is drive

	mov	dh,dl			; dh is head
	mov	dl,cs:rom_drv_num	; set the drive number

; cl is sector + 2 high bits of cylinder
; ch is low 8 bits of cylinder
; dh is head
; dl is drive

; for convenience, we are going to read the logical boot sector
;   into cs:disksector area.

;  read in boot sector using bios disk interrupt.  the buffer where it
;  is to be read in is cs:disksector.

	push	cs
	pop	es
	mov	bx,offset disksector
	mov	ax,201h		; read, one sector
	int	13h

; cs:disksec contains the boot sector.	in theory, (ha ha) the bpb in this thing
; is correct.  we can, therefore, suck out all the relevant statistics on the
; media if we recognize the version number.

	mov	bx,offset disksector

	push	bx			; save changed regs
	push	ax

	cmp	byte ptr cs:[bx],0e9h	; is it a near jump?
	je	check_1_ok		; no,
	cmp	byte ptr cs:[bx],0ebh	;   is it a short jump?
	jne	invalid_boot_record	;   no, invalid boot record
	cmp	byte ptr cs:[bx+2],090h ;   yes, is the next one a nop?
	jne	invalid_boot_record	;     no, invalid boot record

check_1_ok:				; yes, jmup instruction ok.
					; now check some fields in
					;  the boot record bpb
	mov	bx,offset disksector.EXT_BOOT_BPB ; point to the bpb
					;  in the boot record

					; get the mediadescriptor byte
	mov	al,byte ptr cs:[bx].bpb_mediadescriptor

	and	al,0f0h 		; mask off low nibble
	cmp	al,0f0h 		; is high nibble = 0fh?
	jne	invalid_boot_record	;   no, invalid boot record

	cmp	cs:[bx].bpb_bytespersector,512	; M042
	jnz	invalid_boot_record	; M042 invalidate non 512 byte sectors

check_2_ok:				; yes, mediadescriptor ok.
					; now make sure that
					;     the sectorspercluster
					;	is a power of 2

					; get the sectorspercluster

	mov	al,byte ptr cs:[bx].bpb_sectorspercluster

	or	al,al			; is it zero?
	jz	invalid_boot_record	;   yes, invalid boot record

;	M032 begin

ck_power_of_two:
	shr	al,1			; shift until first bit emerges
	jnc	ck_power_of_two

	jz	valid_boot_record	; if no bits left, then proceed ok

;	M032 end

invalid_boot_record:			; for invalid boot record
	pop	ax			; restore registers
	pop	bx			;
	jmp	unknown 		; jump to invalid boot record
					; unformatted or illegal media.

valid_boot_record:			; for valid boot record
	pop	ax			; restore registers
	pop	bx			;

; signature found.  Now check version.

	cmp	word ptr cs:[bx+8],"." shl 8 + "2"
	jnz	try5
	cmp	byte ptr cs:[bx+10],"0"
	jnz	try5

	jmp	short copybpb

setret_brdg:
	jmp	setret

;M042 unknownj:
;M042	jmp	unknown 		;unformatted or illegal media.

unknown3_0_j:				;legally formatted media,
	jmp	unknown3_0		; although, content might be bad.

try5:
	call	cover_fdisk_bug

; see if it is an os2 signature

	cmp	word ptr cs:[bx+8],'.' shl 8 + '0'	; M053
	jne	no_os2					; M053
	mov	al,byte ptr cs:[bx+7]			; M053
	sub	al,'1'					; M053
	and	al,0feh					; M053
	jz	copybpb		; accept either '1' or '2' ; M053

; M053	cmp	word ptr cs:[bx+7],"0" shl 8 + "1"
; M053	jne	no_os2
; M053	cmp	byte ptr cs:[bx+9],"."
; M053	je	copybpb
	jmp	unknown

; no os2 signature, this is to check for real dos versions

no_os2:
	cmp	word ptr cs:[bx+8],"." shl 8 + "3"
	jb	unknown3_0_j		; must be 2.1 boot record. do not trust it, but still legal.
	jnz	copybpb 		; honor os2 boot record, or dos 4.0 version
	cmp	byte ptr cs:[bx+10],"1"	;do not trust 3.0 boot record. but still legal
	jb	unknown3_0_j		; if version >= 3.1, then o.k.

copybpb:

;	we have a valid boot sector. use the bpb in it to build the
;	bpb in bios. it is assumed that only
;		BDS_BPB.BPB_SECTORSPERCLUSTER
;		BDS_BPB.BPB_ROOTENTRIES, and
;		BDS_BPB.BPB_SECTORSPERFAT
;	need to be set (all other values in already). fbigfat
;	is also set.

;if it is non fat based system, then just copy the bpb from the boot sector
;into the bpb in bds table, and also set the boot serial number, volume id,
;and system id according to the boot record.
;for the non_fat system, don't need to set the other value. so just
;do goodret.

	cmp	cs:disksector.EXT_BOOT_SIG, ext_boot_signature
	jne	copybpb_fat		; conventional fat system
	cmp	cs:disksector.EXT_BOOT_BPB.BPB_NUMBEROFFATS,0	; if (# of fat <> 0) then
	jne	copybpb_fat		;   a fat system.

; non fat based media.

	push	di			; sav reg.
	push	ds

	push	ds
	pop	es			; now es:di -> bds
	push	cs
	pop	ds			; ds = cs

	mov	si,offset disksector.EXT_BOOT_BPB ; ds:si -> bpb in boot
	add	di,BDS_BPB.BPB_BYTESPERSECTOR	; es:di -> bpb in bds

;	M034 -- begin additions

;       just for completeness, we'll make sure that total_sectors and
;         big_total_sectors aren't both zero.  I've seen examples of
;         this on DOS 3.30 boot records.  I don't know exactly how it
;         got that way.  If it occurs, then use the values from the
;         partition table.

        cmp     cs:[si].BPB_TOTALSECTORS,0      ; is total_sectors nonzero?
        jnz     already_nonz                    ; done if so, use it
        cmp     cs:[si].BPB_BIGTOTALSECTORS,0   ; how 'bout big_total?
        jnz     already_nonz                    ; we're okay if any are != 0
        cmp     cs:[si].BPB_BIGTOTALSECTORS+2,0
        jnz     already_nonz

;       now let's copy the values from the partition table (now in the BDS)
;         into the BPB in the boot sector buffer, before they get copied
;         back.

        mov     ax,ds:[di.BPB_TOTALSECTORS]     ; get value from part table
        mov     cs:[si].BPB_TOTALSECTORS,ax     ; store it into bpb buffer
        mov     ax,ds:[di.BPB_BIGTOTALSECTORS]  ; get value from part table
        mov     cs:[si].BPB_BIGTOTALSECTORS,ax  ; store it into bpb buffer
        mov     ax,ds:[di.BPB_BIGTOTALSECTORS+2] ; get value from part table
        mov     cs:[si].BPB_BIGTOTALSECTORS+2,ax ; store it into bpb buffer

already_nonz:

;	end M034 additions

	mov	cx,size A_BPB - 6	; ****** Use SMALL version!

	rep	movsb

	pop	ds			; restore reg.
	pop	di
	mapnew
	mov	bp,offset mov_media_ids	; set volume id, systemid, serial.
	push	cs			; simulate far call
	call	call_bios_code
	unmapnew
	jmp	goodret

copybpb_fat:				;  fat system

;	M034 -- begin changes


;       ****** cas ---
;          IBM DOS 3.30 doesn't seem to mind that the TOTAL_SECTORS and
;          BIG_TOTAL_SECTORS field in the boot sector are 0000.  This
;          happens with some frequency -- perhaps through some OS/2 setup
;          program.  We haven't actually been COPYING the TOTAL_SECTORS
;          from the boot sector into the DPB anyway, we've just been using
;          it for calculating the fat size.  Pretty scary, huh?  For now,
;          we'll go ahead and copy it into the DPB, except in the case
;          that it equals zero, in which case we just use the values in
;          the DPB from the partition table.

	mov	si,offset disksector.EXT_BOOT_BPB ; M040  cs:si -> bpb in boot
	xor	dx,dx
        mov     ax,cs:[si.BPB_TOTALSECTORS]     ; get totsec from boot sec
        or      ax,ax
        jnz     copy_totsec                     ; if non zero, use that

        mov     ax,cs:[si.BPB_BIGTOTALSECTORS]  ; get the big version
        mov     dx,cs:[si.BPB_BIGTOTALSECTORS+2]
        mov     cx,dx
        or      cx,ax                           ; see if it is a big zero
        jz      totsec_already_set              ; screw it.  it was bogus.

copy_totsec:
	mov	ds:[di.BDS_BPB.BPB_BIGTOTALSECTORS],ax   ; M040 make DPB match
	mov	ds:[di.BDS_BPB.BPB_BIGTOTALSECTORS+2],dx ; M040 boot sec

totsec_already_set:

;	M034 -- end changes

;M034	xor	dx,dx
;M040	mov	si,offset disksector.EXT_BOOT_BPB ;  cs:bx -> bpb in boot
;M034	mov	ax,cs:[si.BPB_TOTALSECTORS]	;  total sectors
;M034	or	ax,ax			; double word sector number?
;M034	jnz	fat_big_small		;  no. conventional bpb.

;M034	mov	ax,word ptr cs:[si.BPB_BIGTOTALSECTORS] ; use double word
;M034	mov	dx,word ptr cs:[si.BPB_BIGTOTALSECTORS+2]
	mov	ax,word ptr ds:[di.BDS_BPB.BPB_BIGTOTALSECTORS]
	mov	dx,word ptr ds:[di.BDS_BPB.BPB_BIGTOTALSECTORS+2]


fat_big_small:				 ; determine fat entry size.

;at this moment dx;ax = total sector number

;
;Do not assume 1 reserved sector. Update the reserved sector field in BDS 
;from the BPB on the disk
;
	mov	bx,cs:[SI.BPB_RESERVEDSECTORS]		;AN017;get #reserved_sectors from BPB
	mov	ds:[di].BDS_BPB.BPB_RESERVEDSECTORS,bx	;AN017;update BDS field
	sub	ax,bx					;AN017;
	sbb	dx,0					;AN017;update the count

;	sub	ax,1					;AN017; subtrack # reserved (always 1)
;	sbb	dx,0					;AN017;

	mov	bx,cs:[si.BPB_SECTORSPERFAT]	;  bx = sectors/fat
	mov	ds:[di].BDS_BPB.BPB_SECTORSPERFAT,bx 	;  set in bds bpb
	shl	bx,1			; always 2 fats
	sub	ax,bx			; sub # fat sectors
	sbb	dx,0
	mov	bx,cs:[si.BPB_ROOTENTRIES]	;  # root entries
	mov	ds:[di].BDS_BPB.BPB_ROOTENTRIES,bx		;  set in bds bpb

	mov	cl,4
	shr	bx,cl			;  div by 16 ents/sector
	sub	ax,bx			;  sub # dir sectors
	sbb	dx,0
					; dx;ax now contains the # of data sectors
	xor	cx,cx
	mov	cl,cs:[si.BPB_SECTORSPERCLUSTER]	; sectors per cluster
	mov	ds:[di].BDS_BPB.BPB_SECTORSPERCLUSTER,cl	; set in bios bpb
	push	ax
	mov	ax,dx
	xor	dx,dx
	div	cx			; cx = sectors per cluster
	mov	cs:[temp_h],ax
	pop	ax
	div	cx			;  [temp_h];ax now contains the # clusters.
	cmp	cs:[temp_h],0
	ja	toobig_ret		;  too big cluster number

	cmp	ax,4096-10		; is this 16-bit fat?
	jb	copymediaid		; no, small fat
	or	fbigfat,fbig		; 16 bit fat

copymediaid:
	mapnew
	mov	bp,offset mov_media_ids	; copy filesys_id, volume label,
	push	cs			; simulate far call
	call	call_bios_code
	unmapnew
					;and volume serial to bds table, if extended
					;boot record.
	jmp	massage_bpb		; now final check for bpb info. and return.

toobig_ret:
	or	cs:fbigfat,ftoobig
	jmp	goodret 		; still drive letter is assigned
					; but useless. to big for
					; current pc dos fat file system
unknown:
	or	ds:[di].bds_flags, unformatted_media ; M042
					; Set unformatted media flag.

;  the boot signature may not be recognizable,
;     but we should try and read it anyway.

unknown3_0:				;skip setting unformatted_media bit
	mov	dx,word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS+2
	mov	ax,word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS
	mov	si,offset datagrp:disktable2
scan:

	cmp	dx,word ptr cs:[si]
	jb	gotparm
	ja	scan_next
	cmp	ax,word ptr cs:[si+2]
	jbe	gotparm
scan_next:
	add	si,5 * 2
	jmp	scan			;  covers upto 512 mb media
gotparm:
	mov	cl,byte ptr [si+8]	;  fat size for fbigfat flag
	or	fbigfat,cl
	mov	cx,word ptr cs:[si+4]
	mov	dx,word ptr cs:[si+6]

;	dx = number of dir entries,
;	ch = number of sectors per cluster
;	cl = log base 2 of ch

;	now calculate size of fat table


	mov	ds:[di].BDS_BPB.BPB_ROOTENTRIES,dx	;save number of dir entries

;now, cx = BDS_BPB.BPB_SECTORSPERCLUSTER|clusshift
;    ds:[di].BDS_BPB.BPB_ROOTENTRIES = number of directory entries.

	mov	dx,word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS+2
	mov	ax,word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS
	mov	byte ptr ds:[di].BDS_BPB.BPB_SECTORSPERCLUSTER,ch ;save sectors per cluster
	test	fbigfat,fbig		;   if (fbigfat)
	jnz	dobig			;	goto dobig;

;   we don't need to change "small fat" logic since it is gauranteed
;   that double word total sector will not use 12 bit fat (unless
;   it's sectors/cluster >= 16 which will never be in this case.)
;   so in this case we assume dx = 0 !!!.

	xor	bx,bx
	mov	bl,ch
	dec	bx
	add	bx,ax			; dx=0
	shr	bx,cl			; bx = 1+(bpb->maxsec+BDS_BPB.BPB_SECTORSPERCLUSTER-1)/
	inc	bx			; BDS_BPB.BPB_SECTORSPERCLUSTER
	and	bl,11111110b		; bx &= ~1; (=number of clusters)
	mov	si,bx
	shr	bx,1
	add	bx,si
	add	bx,511			; bx += 511 + bx/2
	shr	bh,1			; bh >>= 1; (=bx/512)
	mov	byte ptr ds:[di].BDS_BPB.BPB_SECTORSPERFAT,bh ;save number of fat sectors
	jmp	short massage_bpb

dobig:
; for bigfat we do need to extend this logic to 32 bit sector calculation.

	mov	cl,4			; 16 (2^4) directory entries per sector
	push	dx			; save total sectors (high)
	mov	dx,ds:[di].BDS_BPB.BPB_ROOTENTRIES
	shr	dx,cl			; cseBDS_BPB.BPB_ROOTENTRIES = BDS_BPB.BPB_ROOTENTRIES / 16;
	sub	ax,dx			; dx;ax -= cseBDS_BPB.BPB_ROOTENTRIES; dx;ax -= csecreserved;
	pop	dx
	sbb	dx,0
	sub	ax,1			; dx;ax = t - r - d
	sbb	dx,0			;
	mov	bl,2
	mov	bh,ds:[di].BDS_BPB.BPB_SECTORSPERCLUSTER	; bx = 256 * BDS_BPB.BPB_SECTORSPERCLUSTER + 2

;	i don't understand why to add bx here!!!

	add	ax,bx			; ax = t-r-d+256*spc+2
	adc	dx,0
	sub	ax,1			; ax = t-r-d+256*spc+1
	sbb	dx,0

;     assuming dx in the table will never be bigger than bx.

	div	bx			; BDS_BPB.BPB_SECTORSPERFAT = ceil((total-dir-res)/
					;		 (256*BDS_BPB.BPB_SECTORSPERCLUSTER+2));
	mov	ds:[di].BDS_BPB.BPB_SECTORSPERFAT,ax ; number of fat sectors

; now, set the default filesys_id, volume label, serial number

	mov	bl,fbigfat
	mov	ds:[di].bds_fatsiz,bl		; set size of fat on media

	push	ds			; save bds pointer
	push	ds
	pop	es			; pass to subroutine in es
	push	cs			; set ds to datagrp
	pop	ds
	mov	bp,offset clear_ids
	push	cs			; simulate far call
	call	call_bios_code
	pop	ds			; restore bds pointer

;   at this point, in bpb of bds table, BDS_BPB.BPB_BIGTOTALSECTORS which is
;   set according to the partition information. we are going to
;   see if (hidden sectors + total sectors) > a word.  if it is true,
;   then no change.  otherwise, BDS_BPB.BPB_BIGTOTALSECTORS will be moved
;   to BDS_BPB.BPB_TOTALSECTORS and BDS_BPB.BPB_BIGTOTALSECTORS will be set to 0.
;   we don't do this for the bpb information from the boot record. we
;   are not going to change the bpb information from the boot record

massage_bpb:
	mov	dx,word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS+2
	mov	ax,word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS

	cmp	dx,0			; double word total sector?
	ja	goodret 		; don't have to change it.
	cmp	word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS+2,0
	ja	goodret 		; don't have to change it.
	add	ax,word ptr ds:[di].BDS_BPB.BPB_HIDDENSECTORS
	jc	goodret 		; bigger than a word boundary

	mov	ax,word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS
	mov	ds:[di].BDS_BPB.BPB_TOTALSECTORS,ax
	mov	word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS,0

goodret:
	mov	bl,fbigfat
	mov	ds:[di].bds_fatsiz,bl		; set size of fat on media
	clc
ret_hard:
	pop	es				; M011
	pop	ds
	pop	bx
	pop	di
	ret

sethard endp

cover_fdisk_bug 	proc

;fdisk of pc dos 3.3 and below, os2 1.0 has a bug.  the maximum number of
;sector that can be handled by pc dos 3.3 ibmbio should be 0ffffh.
;instead, sometimes fdisk use 10000h to calculate the maximum number.
;so, we are going to check that if BPB_TOTALSECTORS + hidden sector = 10000h
;then subtract 1 from BPB_TOTALSECTORS.

	push	ax
	push	dx
	push	si
	cmp	cs:disksector.EXT_BOOT_SIG,ext_boot_signature
	je	cfb_retit		;if extended bpb, then >= pc dos 4.00

	cmp	word ptr cs:[bx+7],"0" shl 8 + "1" ; os2 1.0 ? = ibm 10.0
	jne	cfb_chk_BPB_TOTALSECTORS
	cmp	byte ptr cs:[bx+10],"0"
	jne	cfb_retit

cfb_chk_BPB_TOTALSECTORS:
	mov	si,offset disksector.EXT_BOOT_BPB
	cmp	cs:[si.BPB_TOTALSECTORS],0	;just to make sure.
	je	cfb_retit

	mov	ax,cs:[si.BPB_TOTALSECTORS]
	add	ax,word ptr cs:[si.BPB_HIDDENSECTORS]
	jnc	cfb_retit

;M031	xor	ax,ax			;if carry set and ax=0?
	jnz	cfb_retit		; M031 -- zero reflects ax from add

;	M031 -- result of add was 10000h

	dec	cs:[si.BPB_TOTALSECTORS]		; then decrease BPB_TOTALSECTORS by 1.
;M034	dec	word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS
	sub	word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS,1	; M034
	sbb	word ptr ds:[di].BDS_BPB.BPB_BIGTOTALSECTORS+2,0 ; M034


cfb_retit:
	pop	si
	pop	dx
	pop	ax
	ret

cover_fdisk_bug endp


;****************************
;
; setdrvparms sets up the recommended bpb in each bds in the system based on
; the form factor. it is assumed that the bpbs for the various form factors
; are present in the bpbtable. for hard files, the recommended bpb is the same
; as the bpb on the drive.

; no attempt is made to preserve registers since we are going to jump to
; sysinit straight after this routine.

word2	dw	2		; word constants for mul/div
word3	dw	3
word512	dw	512

setdrvparms proc near
	xor	bx,bx
	les	di,[start_bds] 		; get first bds in list

next_bds:
	push	es
	push	di			; preserve pointer to bds
	mov	bl,es:[di].bds_formfactor
	cmp	bl,ffhardfile
	jnz	nothardff

	xor	dx,dx
	mov	ax,es:[di].BDS_BPB.BPB_TOTALSECTORS
	or	ax,ax
	jnz	get_ccyl

	mov	dx,word ptr es:[di].BDS_BPB.BPB_BIGTOTALSECTORS+2	; use double word sector number
	mov	ax,word ptr es:[di].BDS_BPB.BPB_BIGTOTALSECTORS
get_ccyl:
	push	dx
	push	ax
	mov	ax,word ptr es:[di].BDS_BPB.BPB_HEADS
	mul	word ptr es:[di].BDS_BPB.BPB_SECTORSPERTRACK ;assume sectorsp per cyl. < 64k.
	mov	cx,ax			; cx has # sectors per cylinder
	pop	ax
	pop	dx			; restore BDS_BPB.BPB_TOTALSECTORS.

	push	ax
	mov	ax,dx
	xor	dx,dx
	div	cx
	mov	cs:[temp_h],ax		; ax be 0 here.
	pop	ax

	div	cx			; div #sec by sec/cyl to get # cyl.
	or	dx,dx
	jz	no_cyl_rnd		; came out even
	inc	ax			; round up
no_cyl_rnd:
	mov	es:[di].bds_ccyln,ax
	push	es
	pop	ds
	lea	si,[di].BDS_BPB.BPB_BYTESPERSECTOR	; ds:si -> bpb for hard file
	jmp	short set_recbpb

nothardff:

; we don't use the extended bpb for a floppy.

	push	cs
	pop	ds
	assume	ds:datagrp

;	if fake floppy drive variable is set then we don't have to handle this
;	bds.  we can just go and deal with the next bds at label go_to_next_bds.

	cmp	cs:fakefloppydrv,1
	jz	go_to_next_bds

	cmp	bl,ffother		; special case "other" type of medium
	jnz	not_process_other

process_other:
	xor	dx,dx
	mov	ax,ds:[di].bds_ccyln
	mul	ds:[di].BDS_RBPB.BPB_HEADS
	mul	ds:[di].BDS_RBPB.BPB_SECTORSPERTRACK
	mov	ds:[di].BDS_RBPB.BPB_TOTALSECTORS,ax 	; have the total number of sectors
	dec	ax

	mov	dl, 1
again:
	cmp	ax, 4096-10
	jb	@f
	shr	ax, 1
	shl	dl, 1
	jmp	again
@@:
	cmp	dl, 1				; is it a small disk
	je	@f				; yes, 224 root entries is enuf
	mov	ds:[di].BDS_RBPB.BPB_ROOTENTRIES, 240
@@:
	mov	ds:[di].BDS_RBPB.BPB_SECTORSPERCLUSTER, dl

;	logic to get the sectors/fat area.
;	fat entry is assumed to be 1.5 bytes!!!

	mul	cs:word3
	div	cs:word2
	xor	dx,dx
	div	cs:word512
	inc	ax

no_round_up:
	mov	ds:[di].BDS_RBPB.BPB_SECTORSPERFAT,ax
	jmp	short go_to_next_bds

not_process_other:
	shl	bx,1			; bx is word index into table of bpbs
	mov	si,offset datagrp:bpbtable
	mov	si,word ptr [si+bx]	; get address of bpb

set_recbpb:
	lea	di,ds:[di].BDS_RBPB	; es:di -> recbpb
	mov	cx,size bpbx
	rep	movsb			; move (size bpbx) bytes

go_to_next_bds:
	pop	di
	pop	es			; restore pointer to bds
	les	di,es:[di].bds_link
	cmp	di,-1
	jz	got_end_of_bds_chain
	jmp	next_bds
got_end_of_bds_chain:
	ret


setdrvparms endp


;--------------------------------------------------------------------

; al = device number

print_init proc	near
	assume	ds:nothing,es:nothing

	cbw
	mov	dx,ax			; get printer port number into dx
	mov	ah,1			;initalize printer port
	int	17h			;call rom-bios routine
	ret

print_init endp

;--------------------------------------------------------------------

aux_init proc	near
	assume	ds:nothing,es:nothing

	cbw
	mov	dx,ax
	mov	al,rsinit		;2400,n,1,8 (msequ.inc)
	mov	ah,0			;initalize aux port
	int	14h			;call rom-bios routine
	ret

aux_init endp

;--------------------------------------------------------------------

; domini **********************************************************************
;
;mini disk initialization routine. called right after dohard
;modified for >2 hardfile support
;
; **cs=ds=es=datagrp
;
; **domini will search for every extended partition in the system, and
;   initialize it.
;
; **bdsm stands for bds table for mini disk and located right after the label
;   end96tpi.  end_of_bdsm will have the offset value of the ending
;   address of bdsm table.
;
; **bdsm is the same as usual bds structure except that tim_lo, tim_hi entries
;   are overlapped and used to identify mini disk and the number of hidden_trks.
;   right now, they are called as ismini, hidden_trks respectively.
;
; **domini will use the same routine in sethard routine after label set2 to
;   save coding.
;
; **drvmax determined in dohard routine will be used for the next
;   available logical mini disk drive number.
;
;M011 -- begin changed section
;
; input: drvmax, dskdrvs
;
; output: minidisk installed. bdsm table established and installed to bds.
;	  end_of_bdsm - ending offset address of bdsm.
;
;
; called modules:
;		  getboot
;		  find_mini_partition (new), xinstall_bds (new), M038

;		  setmini (new, it will use set2 routine)
;
; variables used: end_of_bdsm
;		  rom_minidisk_num
;		  mini_hdlim, mini_seclim
;		  BDS_STRUC, start_bds
;
;******************************************************************************

domini	proc	near
	assume	ds:datagrp,es:datagrp

	mov	dh,hnum			; get number of hardfiles
	cmp	dh,0
	jz	dominiret		;no hard file? then exit.

	mov	dl,80h			; start with hardfile 80h

domini_loop:
	push	dx			; save hard drive number, count
	mov	rom_minidisk_num,dl

	mov	ah,8h			;get drive parameters
	int	13h			;call rom-bios
	inc	dh			;get # of heads (convert it to 1 based)
	xor	ax,ax
	mov	al,dh
	mov	mini_hdlim,ax		;save it.
	and	cl,3fh			;get # of sectors/track
	mov	al,cl
	mov	mini_seclim,ax		;and save it.

	push	es			; preserve es
	mov	dl,rom_minidisk_num
	call	getboot 		;read master boot record into 7c0:bootbias
	assume	es:nothing
	jc	domininext
	call	find_mini_partition
domininext:
	pop	es
	assume	es:datagrp

	pop	dx
	inc	dl			; next hard file
	dec	dh
	jnz	domini_loop

dominiret:
	ret

domini	endp

;--------------------------------------------------------------------

;find_mini_partition tries to find every extended partition on a disk.
;at entry:	di -> bdsm entry
;		es:bx -> 07c0:bootbias - master boot record
;		rom_minidisk_num  - rom drive number
;		drvmax - logical drive number
;		mini_hdlim, mini_seclim
;
;called routine: setmini which uses set2 (in sethard routine)
;variables & equates used from original bios - flags, fnon_removable, fbigfat

find_mini_partition proc near
	assume	ds:datagrp,es:nothing

	add	bx,1c2h			;bx -> system id.

fmpnext:
	cmp	byte ptr es:[bx],5	; 5 = extended partition id.
	jz	fmpgot
	add	bx,16			; for next entry
	cmp	bx,202h+bootbias
	jnz	fmpnext
	jmp	short fmpret		; M038 not found extended partition

fmpgot: 				;found my partition.

	call	dmax_check		; M029 -- check for drvmax already 26
	jnc	fmpret			; M038, M029 -- done if too many

	mov	di,[end_of_bdss]	; get next free bds
	mov	ds:[di].bdsm_ismini,1
	or	ds:[di].bds_flags,fnon_removable
	mov	ds:[di].bds_formfactor,ffhardfile
	mov	fbigfat,0		;assume 12 bit fat.

	mov	ax,mini_hdlim
	mov	ds:[di].BDS_BPB.BPB_HEADS,ax
	mov	ax,mini_seclim
	mov	ds:[di].BDS_BPB.BPB_SECTORSPERTRACK,ax
	mov	al,rom_minidisk_num
	mov	ds:[di].bds_drivenum,al	;set physical number
	mov	al,drvmax
	mov	ds:[di].bds_drivelet,al	;set logical number

	cmp	word ptr es:[bx+10],0
	ja	fmpgot_cont
	cmp	word ptr es:[bx+8],64	;**with current bpb, only lower word
					; is meaningful.
	jb	fmpret			;should be bigger than 64 sectors at least
fmpgot_cont:

	sub	bx,4			;let bx point to the start of the entry
	mov	dh,byte ptr es:[bx+2]
	and	dh,11000000b		;get higher bits of cyl
	rol	dh,1
	rol	dh,1
	mov	dl,byte ptr es:[bx+3]	;cyl byte
	mov	ds:[di].bdsm_hidden_trks,dx	;set hidden trks

;** now, read the volume boot record into bootbias.

	mov	cx,es:[bx+2]		;cylinder,cylinder/sector
	mov	dh,es:[bx+1]		;head
	mov	dl,rom_minidisk_num	;drive
	mov	bx,bootbias		;buffer offset
	mov	ax,201h 		;read, 1 sector
	int	13h			;call rom-bios routine
	jc	fmpret			;cannot continue.

	mov	bx,1c2h+bootbias
	push	es			; addressability to next minidisk
	call	setmini 		; install a mini disk. bx value saved.
	pop	es
	jc	fmpnextchain

;M038	add	[end_of_bdss],size BDS_STRUC

	call	xinstall_bds		; M038 -- install the bdsm into table

;M038	inc	drvmax			; increase the logical drive for next
;M038
;M038	push	bx			; now, set the dskdrvs pointer to bpb info.
;M038	mov	bx,[last_dskdrv_table]
;M038	lea	si,ds:[di].BDS_BPB.BPB_BYTESPERSECTOR	; points to bpb of bdsm
;M038	mov	[bx],si
;M038	add	bx,2
;M038	mov	[last_dskdrv_table],bx
;M038	pop	bx


fmpnextchain: jmp fmpnext		;let's find out if we have any chained partition

fmpret:
	ret

find_mini_partition endp

;--------------------------------------------------------------------

setmini	proc	near
	assume	ds:datagrp,es:nothing

	push	di
	push	bx
	push	ds
	push	es			; M011 - match stack for standard exit

;	M019 -- begin changes

setmini_1:
	cmp	byte ptr es:[bx],1
	jz	setmini_2

	cmp	byte ptr es:[bx],4
	jz	setmini_2

	cmp	byte ptr es:[bx],6
	jz	setmini_2

	add	bx,16
	cmp	bx,202h+bootbias
	jnz	setmini_1

	stc
	pop	es
	pop	ds
	pop	bx
	pop	di	
	ret

setmini_2:
	jmp	set2			; branch into middle of sethard

;	M019 -- end changes

setmini	endp

;M011 -- end changed section

;M029 -- begin additions
;
;	dmax_check --- call this when we want to install a new drive.
;			it checks for drvmax < 26 to see if there is
;			a drive letter left.
;
;		drvmax < 26 : carry SET!
;		drvmax >=26 : carry RESET!, error flag set for message later
;				trash ax
;
dmax_check	proc	near
	cmp	drvmax,26		; already have max?
	jc	dmax_ok			; return with carry if okay

	push	es
	mov	ax,SYSINITSEG
	mov	es,ax
	mov	byte ptr es:[toomanydrivesflag],1 ; set message flag
	pop	es

;	note:  carry still clear!!!

dmax_ok:
	ret

dmax_check	endp

;	M029 -- end additions

;	M038 -- begin additions

;--------------------------------------------------------------------
;	link next bds (at ds:di) into the chain.  assume that the
;	  chain is entirely within ds == datagrp.  also update drvmax,
;	  dskdrv_table, and end_of_bdss.


xinstall_bds proc near
	assume	ds:datagrp,es:nothing

	push	si
	push	bx

	mov	si,word ptr [start_bds]	; get first bds

xinstall_bds_1:
	cmp	word ptr [si].bds_link,-1 ; is this the last one?
	jz	xinstall_bds_2		; skip ahead if so
	mov	si,word ptr [si].bds_link ; chain through list
	jmp	xinstall_bds_1

xinstall_bds_2:
	mov	word ptr [si].bds_link,di
	mov	word ptr [si].bds_link+2,ds
	mov	word ptr [di].bds_link,-1	;make sure it is a null ptr.
	mov	word ptr [di].bds_link+2,ds	; might as well plug segment
						;  into final bds in chain
	lea	bx,[di].BDS_BPB
	mov	si,[last_dskdrv_table]
	mov	word ptr [si],bx
	add	[last_dskdrv_table],2

	inc	drvmax

	add	end_of_bdss,size BDS_STRUC
	pop	bx
	pop	si
	ret

xinstall_bds endp

;	M038 -- end additions

;--------------------------------------------------------------------

;**end of mini disk initialization**

;--------------------------------------------------------------------

IFNDEF	POWER		; CMOS clock is initialized by power management
			; device driver, if present.  ;M074

cmos_clock_read proc	near
	assume	ds:datagrp,es:datagrp

; in order to determine if there is a clock present in the system, the following
; needs to be done.

	push	ax
	push	cx
	push	dx
	push	bp

	xor	bp,bp
loop_clock:
	xor	cx,cx
	xor	dx,dx
	mov	ah,2			;read real time clock
	int	1ah			;call rom-bios routine
	cmp	cx,0			; cas - arrrrgh!
	jnz	clock_present

	cmp	dx,0
	jnz	clock_present

	cmp	bp,1			; read again after a slight delay, in case clock
	jz	no_readdate		; was at zero setting.

	inc	bp			; only perform delay once.
	mov	cx,4000h
delay:
	loop	delay
	jmp	loop_clock

clock_present:
	mov	cs:havecmosclock,1	; set the flag for cmos clock
	call	cmosck			; reset cmos clock rate that may be
					;possibly destroyed by cp dos and post routine did not
					;restore that.
	push	si
	call	read_real_date		; read real-time clock for date

	cli
	mov	daycnt,si		; set system date
	sti
	pop	si

no_readdate:
	pop	bp
	pop	dx
	pop	cx
	pop	ax
	ret

cmos_clock_read endp

; the following code is written by jack gulley in engineering group.
; cp dos is changing cmos clock rate for its own purposes and if the
; use cold boot the system to use pc dos while running cp dos, the cmos
; clock rate are still slow which slow down disk operations of pc dos
; which uses cmos clock.  pc dos is put this code in msinit to fix this
; problem at the request of cp dos.
;
; the program is modified to be run on msinit. equates are defined in cmosequ.inc.
; this program will be called by cmos_clock_read procedure.
;
;  the following code cmosck is used to insure that the cmos has not
;	had its rate controls left in an invalid state on older at's.
;
;	it checks for an at model byte "fc" with a submodel type of
;	00, 01, 02, 03 or 06 and resets the periodic interrupt rate
;	bits in case post has not done it.  this initilization routine
;	is only needed once when dos loads.  it should be run as soon
;	as possible to prevent slow diskette access.
;
;	this code exposes one to dos clearing cmos setup done by a
;	resident program that hides and re-boots the system.

cmosck	proc	near			; check and reset rtc rate bits
	assume	ds:nothing,es:nothing

;model byte and submodel byte were already determined in msinit.

	push	ax
	cmp	cs:model_byte,0fch	;check for pc-at model byte
					; exit if not "fc" for a pc-at
	jne	cmosck9 		; exit if not an at model

	cmp	cs:secondary_model_byte,06h  ; is it 06 for the industral at
	je	cmosck4 		; go reset cmos periodic rate if 06
	cmp	cs:secondary_model_byte,04h  ; is it 00, 01, 02, or 03
	jnb	cmosck9 		; exit if problem fixed by post
					; also,secondary_model_byte = 0 when ah=0c0h, int 15h failed.
cmosck4:				;	reset the cmos periodic rate
					;  model=fc submodel=00,01,02,03 or 06

	mov	al,cmos_reg_a or nmi	;nmi disabled on return
	mov	ah,00100110b		;set divider & rate selection
	call	cmos_write

					; clear set,pie,aie,uie and sqwe
	mov	al,cmos_reg_b or nmi	;nmi disabled on return
	call	cmos_read
	and	al,00000111b		;clear set,pie,aie,uie,sqwe
	mov	ah,al
	mov	al,cmos_reg_b		;nmi enabled on return
	call	cmos_write


cmosck9:				; exit routine
	pop	ax
	ret				; return to caller
					;  flags modifyied
cmosck	endp


;--- cmos_read -----------------------------------------------------------------
;		read byte from cmos system clock configuration table	       :
;									       :
; input: (al)=	cmos table address to be read				       :
;		bit    7 = 0 for nmi enabled and 1 for nmi disabled on exit    :
;		bits 6-0 = address of table location to read		       :
;									       :
; output: (al)	value at location (al) moved into (al).  if bit 7 of (al) was  :
;		on then nmi left disabled.  during the cmos read both nmi and  :
;		normal interrupts are disabled to protect cmos data integrity. :
;		the cmos address register is pointed to a default value and    :
;		the interrupt flag restored to the entry state on return.      :
;		only the (al) register and the nmi state is changed.	       :
;-------------------------------------------------------------------------------

cmos_read	proc	near		; read location (al) into (al)
	assume	es:nothing,ds:nothing

	pushf				; save interrupt enable status and flags
	cli
	push	bx
	push	ax			;save user nmi state
	or	al,nmi			;disable nmi for us
	out	cmos_port,al
	nop				;undocumented delay needed
	in	al,cmos_data		;get data value

;	set nmi state to user specified

	mov	bx,ax			;save data value
	pop	ax			;get user nmi
	and	al,nmi
	or	al,cmos_shut_down
	out	cmos_port,al
	nop
	in	al,cmos_data

	mov	ax,bx			;data value
	pop	bx

	push	cs			; *place datagrp segment in stack and
	call	cmos_popf		; *handle popf for b- level 80286
	ret				; return with flags restored

cmos_read	endp

cmos_popf	proc	near		; popf for level b- parts
	iret				; return far and restore flags

cmos_popf	endp

;--- cmos_write ----------------------------------------------------------------
;		write byte to cmos system clock configuration table	       :
;									       :
; input: (al)=	cmos table address to be written to			       :
;		bit    7 = 0 for nmi enabled and 1 for nmi disabled on exit    :
;		bits 6-0 = address of table location to write		       :
;	 (ah)=	new value to be placed in the addressed table location	       :
;									       :
; output:	value in (ah) placed in location (al) with nmi left disabled   :
;		if bit 7 of (al) is on.  during the cmos update both nmi and   :
;		normal interrupts are disabled to protect cmos data integrity. :
;		the cmos address register is pointed to a default value and    :
;		the interrupt flag restored to the entry state on return.      :
;		only the cmos location and the nmi state is changed.	       :
;-------------------------------------------------------------------------------

cmos_write	proc	near		; write (ah) to location (al)
	assume	es:nothing,ds:nothing
	pushf				; save interrupt enable status and flags
	push	ax			; save work register values

	cli
	push	ax			; save user nmi state
	or	al,nmi			; disable nmi for us
	out	cmos_port,al
	nop
	mov	al,ah
	out	cmos_data,al		; write data

;   set nmi state to user specified

	pop	ax			; get user nmi
	and	al,nmi
	or	al,cmos_shut_down
	out	cmos_port,al
	nop
	in	al,cmos_data

	pop	ax			; restore work registers
	push	cs			; *place datagrp segment in stack and
	call	cmos_popf		; *handle popf for b- level 80286
	ret

cmos_write	endp

ENDIF	; NOT POWER


;==========================================================================
;
; The following routines provide support for reading in the file MSDOS.SYS.
;
; Thus, they are not needed for ROMDOS
;

ifndef ROMDOS

;*******************************
;
; GetClus, read in a cluster at a specified address
;
;  bx = cluster to read
;  cx = sectors per cluster
;  es:di = load location
;
getclus proc	near
	push	cx
	push	di
	mov	doscnt,cx	      ;save number of sectors to read
	mov	ax,bx
	dec	ax
	dec	ax
	mul	cx			;convert to logical sector

; now dx;ax = matching logical sector number starting from the data sector.
;	add the bios start sector to the sector number in dx:ax.  the bios
;	start sector number is in bios_h:bios_l

	add	ax,cs:bios_l
	adc	dx,cs:bios_h

; now dx;ax = first logical sector to read

getcl1:

;	   si = bx, bx = next allocation unit
;		get the fat entry at bx

unpack:
	push	ds
	push	ax			;save first logical sector (low)
	push	bx
	mov	si,fatloc
	mov	ds,si			;ds -> fatloc segment
	mov	si,bx

	test	cs:fbigfat,fbig 	;16 bit fat?
	jnz	unpack16

	shr	si,1			;12 bit fat. si=si/2
	add	si,bx			; si = clus + clus/2

	push	dx			; M054
	xor	dx,dx			; M054 12 bit fat has <16 bit offset
	call	get_fat_sector		; offset of fat entry in bx
	pop	dx			; M054

	mov	ax,[bx]			;save it into ax
	jne	even_odd		;if not a splitted fat, check even-odd.

	mov	al,byte ptr [bx]	;splitted fat.
	mov	byte ptr cs:temp_cluster,al
	inc	si
	push	dx			; M054
	xor	dx,dx			; M054 12 bit fat has <16 bit offset
	call	get_fat_sector
	pop	dx			; M054

	mov	al,byte ptr ds:[0]
	mov	byte ptr cs:temp_cluster+1, al
	mov	ax,cs:temp_cluster
even_odd:
	pop	bx			;restore old fat entry value
	push	bx			;save it right away.
	shr	bx,1			;was it even or odd?
	jnc	havclus 		;it was even.

	shr	ax,1			;odd. massage fat value and keep
	shr	ax,1			;the highest 12 bits.
	shr	ax,1
	shr	ax,1

havclus:
	mov	bx,ax			; now bx = new fat entry.
	and	bx,0fffh		; keep low 12 bits.
	jmp	short unpackx

unpack16:				;16 bit fat.
	push	dx			; M054
	xor	dx,dx			; M054 extend to 32 bit offset
	shl	si,1			;get the offset value.
	adc	dx,0			; M054 32 bit
	call	get_fat_sector
	pop	dx			; M054

	mov	bx,[bx]		; now bx = new fat entry.
unpackx:
	pop	si			;retore old bx value into si
	pop	ax			;restore logical sector (low)
	pop	ds

	sub	si,bx
	cmp	si,-1			;one apart?
	jnz	getcl2

	add	doscnt,cx
	jmp	getcl1

getcl2:
	push	bx
	push	dx			; sector to read (high)
	push	ax			; sector to read (low)
	mov	ax,drvfat		;get drive and fat spec
	mov	cx,doscnt
	pop	dx			; sector to read for diskrd (low)
	pop	cs:[start_sec_h]	; sector to read for diskrd (high)

	push	ds			; save whatever we might have had in ds
	push	cs			; point ds at datagrp
	pop	ds

	push	cs			; simulate far call
	mov	bp,offset diskrd
	call	call_bios_code		;read the clusters

;	hmmmmm.  Error checking?????

	pop	ds			; restore our ds

	pop	bx

	pop	di
	mov	ax,doscnt		;get number of sectors read
	xchg	ah,al			;multiply by 256
	shl	ax,1			;times 2 equal 512
	add	di,ax			;update load location
	pop	cx			;restore sectors/cluster
	ret

getclus endp

get_fat_sector	proc	near

;function: find and read the corresponding fat sector into ds:0
;
;in). dx:si - offset value (starting from fat entry 0) of fat entry to find. M054
;     ds - fatloc segment
;     cs:drvfat - logical drive number, fat id
;     cs:md_sectorsize
;     cs:last_fat_secnum - last fat sector number read in.
;
;out). corresponding fat sector read in.
;      bx = offset value from fatlog segment.
;      other registera saved.
;      zero flag set if the fat entry is splitted, i.e., wehn 12 bit fat entry
;      starts at the last byte of the fat sector. in this case, the caller
;      should save this byte, and read the next fat sector to get the rest
;      of the fat entry value. (this will only happen with the 12 bit fat.)

	push	ax
	push	cx
;M054	push	dx
	push	di
	push	si
	push	es
	push	ds

;M054	xor	dx,dx
	mov	ax,si			; M054 dx:ax == offset
	mov	cx,cs:md_sectorsize		; =512 bytes
	div	cx				; ax=sector number, dx = offset

	PUBLIC TEST3
TEST3:
	nop


;AN017 Get rid of the assumption that there is only one reserved sector
;
	push	es
	push	ds				;AN017;
	push	di				;AN017;
	push	ax				;AN017;


	push	cs
	pop	ds
	assume	ds:datagrp

	mov	ax,cs:DRVFAT			;AN017;get drive # and FAT id

	mov	bp,offset setdrive
	push	cs				;5.00 simulate far call
	call	call_bios_code			;5.00 get bds for drive

;	call	setdrive			;5.00; AN017;get the BDS for the drive

	pop	ax				;AN017;
	add	ax,ES:[DI.BDS_BPB.BPB_RESERVEDSECTORS] ;AN017;add #reserved_sectors
	pop	di				;AN017;
	pop	ds				;AN017;
	pop	es

;	inc	ax				;AN017; make ax to relative logical sector number

	cmp	ax,cs:last_fat_secnum		;  by adding reserved sector number.
	je	gfs_split_chk			; don't need to read it again.

	mov	cs:last_fat_secnum,ax		; update last_fat_secnum
	push	dx				; save offset value.
	mov	cs:[start_sec_h],0		; prepare to read the fat sector
	mov	dx,ax				; start_sec_h is always 0 for fat sector.
	mov	cx,1				; 1 sector to read
	mov	ax,cs:drvfat
	push	ds
	pop	es
	xor	di,di				; es:di -> fatloc segment:0

	push	ds			; save whatever we had in ds
	push	cs			; point ds to datagrp for diskio
	pop	ds
	push	cs			; simulate far call
	mov	bp,offset diskrd
	call	call_bios_code		; cross your finger.
	pop	ds			; restore our old ds

	pop	dx				; restore offset value.
	mov	cx,cs:md_sectorsize

gfs_split_chk:
	dec	cx				;if offset points to the
	cmp	dx,cx				;last byte of this sector, then splitted entry.
	mov	bx,dx				;set bx to dx

	pop	ds
	pop	es
	pop	si
	pop	di
;M054	pop	dx
	pop	cx
	pop	ax
	ret
get_fat_sector	endp

endif	; end of non-ROMDOS-only code to load MSDOS.SYS M054 (comment change)


Bios_Data_Init	ends
	end

