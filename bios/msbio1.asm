;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,160
	title	msbio1.asm - Bios_Data definition and device driver entry/exit

;
;----------------------------------------------------------------------------
;
; Modification history
;
; M001 : BIOS used to local enable a20 before calling original int 13 fn
;        and local disable a20 after the int 13 call. This created some
;        problems with disk caching programs which do lazy reads (PC-Kwik)
;	 Now BIOS turns a20 on if it is off and leaves it on after the
;	 int 13 call
;
; M028 : Added INT 10 to the list of interrupt vectors to be restored
;	 at INT 19 time.
;
; M033 : BIOS was setting the EOT in DPT to its desired value and then
;	 wrongly resetting it to 9 after it job. Now it saves the EOT
;	 before setting & resets it to the old value.
;
; M036 : Added VDISK header clearing code at INT 19 & CTRL ALT DEL
;
; M041 : Saving & restoring A20 status for all transitions from HMA
;	 to Lomem & vice versa in INT 13 handler. For PCKWIK
;
; M058 : Bug #4925. Changed Orig13 offset to 70:b4 so that people like CMS,
;	ProComm who patch it directly will work.
;
; M059 : Bug #5002. Treat rollover byte as a count instead of a flag, if
;			t_switch is not set.
;
;
; M064 : Bug #5070. Ensure that A20 is enabled before entering Device driver
;		entry point in HMA. And do not try to preserve A20 status
;		across INT 13 calls (support norton cache).
;
;
; M073 : Add far code pointers in power.asm to seg_reinit list.
;
; M084 : Remove IOCTL support for built-in POWER$ 
;
; M097 : B#2958. Define endfloppy label after ROMDRIVE & POWER code, so that we
;	  do not overwrite ROMDRIVE and POWER on a harddiskless system
;
;----------------------------------------------------------------------------
;
	include version.inc	; set build flags
	include biosseg.inc	; define BIOS segments

	include	devsym.inc
	include	msequ.inc
	include	bpb.inc
	include	bootform.inc
	include	msbds.inc

;SR;
; Assembly conditional for stack switching
;
STACKSW		equ	1

Bios_Data	segment

	assume	cs:Bios_Data
	public	BData_start
BData_start:


	assume	ds:nothing,es:nothing

	public	hdrv_pat
hdrv_pat label	word			; patched by msinit
	assume	cs:Bios_Data

	extrn	init:near		; this is in msinit

	jmp	init			; go to initialization code


;	define some stuff that is also used by msdos.sys from an include file

In_Bios	=	0ffffh	; define flag for msbdata.inc
	include	msbdata.inc


	public	inHMA,xms
inHMA	db	0		; flag indicates we're running from HMA
xms	dd	0		; entry point to xms if above is true

	align	2
;M058;	public orig13
;M058;orig13	dd	?

	public	ptrsav
ptrsav	dd	0

	public	auxbuf
auxbuf	db	0,0,0,0   	;set of 1 byte buffers for com 1,2,3, and 4
	public	zeroseg
zeroseg dw	0		; easy way to load segment registers with zero

	public	i13_ds,prevoper,number_of_sec	; M030
i13_ds		dw	?	; M030 ds register for int13 call through
prevoper	dw	?	; holds int 13 request (i.e. register ax).
number_of_sec	db	?	; holds number of secs. to read on an ecc error
	public	auxnum
auxnum	dw	0			;which aux device was requested


	public	res_dev_list

res_dev_list	label	byte
	p_attr	=	chardev+outtilbusy+dev320+IOQUERY
; **	p_attr	=	chardev+outtilbusy+dev320

	sysdev <auxdev2,8013h,strategy,con_entry,'CON     '>
auxdev2 sysdev <prndev2,8000h,strategy,aux0_entry,'AUX     '>
prndev2 sysdev <timdev,p_attr,strategy,prn0_entry,'PRN     '>
timdev	sysdev <dskdev,8008h,strategy,tim_entry,'CLOCK$  '>
dskdev	dd	com1dev
	dw	842h + IOQUERY		; 00001001 01000010
; **	dw	842h			; 32 bit sector calculation
	dw	strategy,dsk_entry

	public	drvmax
	public	step_drv
	public	fhave96
	public	single
	public	fhavek09
	public	fsetowner

drvmax		db	4
step_drv	db     -2		; last drive accessed
fhave96 	db	0		; flag to indicate presence of
					;   96tpi support
single		db	0		; used to detect single drive systems
fhavek09	db	0		; indicates if this is a k09 or not
					;  used by console driver.
fsetowner	db	?		; =1 if we are setting the owner of a
					;  drive. (examined by checksingle).

com1dev sysdev <lpt1dev,8000h,strategy,aux0_entry,'COM1    '>
lpt1dev sysdev <lpt2dev,p_attr,strategy,prn1_entry,"LPT1    ">

IFDEF	ROMDOS
;
;ROM DOS BIOS has an additional six bytes of variables before this
;point, so Orig13 has been repositioned yet again.  -DBO
;
;M058; Start of changes
; Orig13 needs to be at offset 0b4h for the CMS floppy driver to work.
;These guys patch Orig13 with their own int 13h hook and so this offset
;cannot change for them to work. Even ProComm does this.
;
	db	15 dup (?)	; to make Orig13 offset 0B4h

IF2
	  .errnz (OFFSET Orig13 - 0b4h) ; Orig13 offset must be 0b4h
ENDIF

	public orig13
orig13	dd	?

;
;M058; End of changes
;
ENDIF	;ROMDOS

lpt2dev sysdev <lpt3dev,p_attr,strategy,prn2_entry,"LPT2    ">

IFNDEF	ROMDOS
;
;M058; Start of changes
; Orig13 needs to be at offset 0b4h for the CMS floppy driver to work.
;These guys patch Orig13 with their own int 13h hook and so this offset
;cannot change for them to work. Even ProComm does this.
;
	db	3 dup (?)	; to make Orig13 offset 0B4h

IF2
	  .errnz (OFFSET Orig13 - 0b4h) ; Orig13 offset must be 0b4h
ENDIF

	public orig13
orig13	dd	?

;
;M058; End of changes
;
ENDIF	;NOT ROMDOS

lpt3dev sysdev <com2dev,p_attr,strategy,prn3_entry,"LPT3    ">
com2dev sysdev <com3dev,8000h,strategy,aux1_entry,"COM2    ">
com3dev sysdev <com4dev,8000h,strategy,aux2_entry,"COM3    ">


;	define a label for the device (if any) which immediately
;	  follows com4

IFDEF	POWER
after_com4	equ	powerdev
ELSE
IFDEF	ROMDRIVE
	extrn	rdrive:near
after_com4	equ	rdrive
ELSE
after_com4	equ	0ffffh
ENDIF
ENDIF

com4dev dw	after_com4,Bios_Data,8000h,strategy,aux3_entry
	db	"COM4    "

IFDEF	POWER
powerdev	label	word

IFDEF	ROMDRIVE
	extrn	rdrive:near
	dw	rdrive
ELSE
	dw	0ffffh		; end of chain
ENDIF

		dw	Bios_Data, 8000h, strategy, power_entry		;M084
	db	"POWER$  "

ENDIF

		public	RomVectors
RomVectors	label	byte
	public	Old10, Old13, Old15, Old19, Old1B	; M028
	db	10h					; M028
Old10	dd	(?)					; M028
	db	13h
Old13	dd	(?)
	db	15h
Old15	dd	(?)
	db	19h
Old19	dd	(?)
	db	1bh
Old1B	dd	(?)
EndRomVectors	equ	$
		public	NUMROMVECTORS
NUMROMVECTORS	equ	((EndRomVectors - RomVectors)/5)

	public	start_bds
	public	accesscount
	public	tim_drv
	public	medbyt

start_bds dd      bds1			;start of bds linked list.
accesscount	db	0		; number of times media check called
tim_drv 	db	-1		; time when last disk i/o performed
medbyt		db	?

	public	wrtverify
	public	rflag
	public	seccnt
	public	dsktnum
	public	motorstartup,settlecurrent,settleslow
	public	save_eot		; M033
	public	save_head_sttl
	public	eot

wrtverify	label word
rflag		db	romread 	;2 for read, 3 for write
verify		db	0		;1 if verify after write
seccnt		dw	0
		db	00		; M011 -- pad where hardnum was
dsktnum 	db	1		;number of diskette drives
motorstartup	db	?		; value from table
settlecurrent	db	?		; value from table
settleslow	db	?		; slow settle value

nextspeed	db	?		; value of speed to be used
save_head_sttl	db	?		;used by read_sector routine
save_eot	db	?		; saved eot from the default DPT M033

eot		db	9

	public	dpt
	public	cursec,curhd,curtrk,spsav
	public	formt_eot,hdnum,trknum,gap_patch
	public	errin
	public	lsterr

dpt		dd	?

;	keep the next two items contiguous - see ioctl_block for reason

cursec		db	0		;current sector
curhd		db	0		;current head
curtrk		dw	0		;current track
spsav		dw	0		;save the stack pointer

;	the following are used for ioctl function calls

formt_eot	db	8		    ; eot used for format
hdnum		db	0		    ; head number
trknum		dw	0		    ; track being manipulated
gap_patch	db	50h		    ; format gap patched into dpt

;	disk errors returned from the ibm rom

errin	db	0cch			; write fault error
	db	80h			;no response
	db	40h			;seek failure
	db	10h			;bad crc
	db	8			;dma overrun
	db	6			; media change
	db	4			;sector not found
	db	3			;write attempt to write-protect disk
lsterr	db	0			;all other errors

;	returned error codes corresponding to above

	public errout
errout	db	10			; write fault error
	db	2			;no response
	db	6			;seek failure
	db	4			;bad crc
	db	4			;dma overrun
	db	15			; invalid media change
	db	8			;sector not found
	db	0			;write attempt on write-protect disk
	db	12			;general error
	public numerr
numerr	=	errout-errin

;-------------------------------------------------------------
;
;   read in boot sector here,  read done in readboot.
;   also read sector for dma check for hard disk.
;
;	This buffer is word aligned because certain AMI BIOSs	; M061
;	have a bug in them which causes the byte after the	; M061
;	buffer to be trashed on floppy reads to odd-byte	; M061
;	boundaries.  Although no general effort is made to	; M061
;	enforce this in the bigger picture, this one small	; M061
;	sacrifice makes that system more-or-less work.		; M061

	align	2		; M061

	public	disksector

disksector		db	size EXT_IBMBOOT_HEADER dup (?)
			db	512-($-disksector) dup (?)

;*********************************************************************
;	"bds" contains information for each drive in the system.
;	various values are patched whenever actions are performed.
;	sectors/alloc. unit in bpb initially set to -1 to signify that
;	the bpb has not been filled. link also set to -1 to signify end
;	of list. # of cylinders in maxparms initialized to -1 to indicate
;	that the parameters have not been set.
;
bds_str struc
	dw	?		; dword link to next structure
	dw	?
	db	0		; int 13 drive number
	db	0		; logical drive letter

xfdrv	dw	512		; physical sector size in bytes
	db	-1		; sectors/allocation unit
	dw	1		; reserved sectors for dos
	db	2		; no of allocation tables
	dw	64		; number of directory entries
	dw	9*40		; number sectors (at 512 bytes each)
	db	0		; media descriptor, initially 0
	dw	2		; number of fat sectors
	dw	9		; sector limit
	dw	1		; head limit
	dw	0		; hidden sector count (low word)
	dw	0		; hidden sector (high)
	dw	0		; number sectors (low)
	dw	0		; number sectors (high)
	db	0		;  true => large fats
	dw	0		; open ref. count
	db	3		; form factor
	dw	20h		; various flags
	dw	40		; number of cylinders

;	recommended bps for this drive

	dw	512		; bytes per sector
	db	1		; sectors/alloc unit
	dw	1		; reserved sectors for dos
	db	2		; number of allocation tables
	dw	0e0h		; number directory entries
	dw	9*40		; number 512 byte sectors
	db	0f0h		; media descriptor, initially 0f0h
	dw	2		; number of fat sectors
	dw	9		; sector limit
	dw	2		; head limit
	dw	0		; hidden sector count(low)
	dw	0		; hidden sector count(high)
	dw	0		; number sectors(low)
	dw	0		; number sectors(high)
	db	6 dup (?)

	db	-1		; last track accessed on this drive
	dw	-1		; keep these two contiguous (?)
	dw	-1
	db	"NO NAME    ",0	; volume id for this disk
	dd	0		; current volume serial from boot record
	db	"FAT12   ",0	; current file system id from boot record
bds_str ends

bds1	bds_str <bds2,seg Bios_Data>
bds2	bds_str <bds3,seg Bios_Data>
bds3	bds_str <bds4,seg Bios_Data>
bds4	bds_str <-1,seg Bios_Data>

	public	fdrive1
	public	fdrive2
	public	fdrive3
	public	fdrive4

fdrive1 equ	bds1.xfdrv
fdrive2 equ	bds2.xfdrv
fdrive3 equ	bds3.xfdrv
fdrive4 equ	bds4.xfdrv


bpbtype struc
spf	db	?
spt	db	?
cdire	db	?
csec	dw	?
spau	db	?
chead	db	?
bpbtype ends
	public sm92
sm92	bpbtype <3,9,70h,2*9*80,2,2>

	public	keyrd_func
	public	keysts_func

; moved altah to inc\msbdata.inc so it could go in instance table in DOS

keyrd_func	db	0	; default is conventional keyboard read
keysts_func	db	1	; default is conventional keyboard status check.

	public printdev
printdev	db	0		; index into above array

; the following variable can be modified via ioctl sub-function 16. in this
; way, the wait can be set to suit the speed of the particular printer being
; used. one for each printer device.

	public wait_count
wait_count	dw	4 dup (50h)	; array of retry counts for printer

		public daycnt, t_switch			; M059
daycnt		dw	0
t_switch	db	0	; flag to indicate whther to use old	; M059
				; way of updating daycnt or not		; M059

; variables for real time clock setting
	public	havecmosclock
havecmosclock	db	0	;set by msinit
	public	base_century
base_century	db	19
	public	base_year
base_year	db	80
	public	month_tab
month_tab	db	31,28,31,30,31,30,31,31,30,31,30,31

; the following are indirect far call addresses. the
;procedures are defined in msinit for relocation.  msinit will reset these
;adress when the relocation is done.

	extrn	bin_to_bcd:far
	extrn	daycnt_to_day:far
	public	bintobcd
	public	daycnttoday

bintobcd	dd	bin_to_bcd	;points to bin_to_bcd proc in msinit
daycnttoday	dd	daycnt_to_day	;points to daycnt_to_day in msinit


;	this stuff is related to the msdisk.asm module

	public	set_id_flag
set_id_flag	db	0	; if 1, getbp routine will set the
				;vol_serial and filesys_id in bds table
				;from the media boot record, if it is > dos 4.00
				;formatted one. then set_id_flag will be set to 2
				;to signal that volume_label is set from the extended
				;boot record and do not set it from the root
				;directory as done in set_volume_id routine.
				;for the old version, vol_serial
				;will be set to -1, and filesys_id will be set
				;to "fat12   " if it is a floppy.

	public	fat_12_id
fat_12_id  db  "FAT12   ",0	;  default system id for floppy.
	public	fat_16_id
fat_16_id  db  "FAT16   ",0
	public	vol_no_name
vol_no_name db "NO NAME    ",0
	public	temp_h
temp_h		dw	0		; temporary for 32 bit calculation.

	public	start_sec_h
start_sec_h	dw	0		; starting sector number high word.
					;used as an input to diskio subroutine.
	public	saved_word
saved_word	dw	0		;  tempory saving place for a word.

	public	multrk_flag
multrk_flag	dw	0
	public ec35_flag
ec35_flag	db	0		; flags for electrically compatible 3.5 inch disk drives
	public	vretry_cnt
vretry_cnt	dw	0
	public	soft_ecc_cnt
soft_ecc_cnt	dw	0
	public	multitrk_format_flag
multitrk_format_flag	db	0	;testing. if 1, then multi track format request
	public	xfer_seg
xfer_seg	dw	0		; temp for transfer segment

;	variables for msdioctl.asm module

; tracktable contains a 4-tuples (c,h,r,n) for each sector in a track
; c = cylinder number,h = head number,r = sector id,n = bytes per sector
;	n	bytes per sector
;      ---	----------------
;	0	      128
;	1	      256
;	2	      512
;	3	     1024
;
	public	max_sectors_curr_sup
max_sectors_curr_sup	equ	63	; current maximum sec/trk that
					; we support (was 40 in dos 3.2)
	public	sectorspertrack
sectorspertrack dw	36
	public	tracktable
tracktable	db	0,0,1,2
		db	0,0,2,2
		db	0,0,3,2
		db	0,0,4,2
		db	0,0,5,2
		db	0,0,6,2
		db	0,0,7,2
		db	0,0,8,2
		db	0,0,9,2
		db	0,0,10,2
		db	0,0,11,2
		db	0,0,12,2
		db	0,0,13,2
		db	0,0,14,2
		db	0,0,15,2
		db	0,0,16,2
		db	0,0,17,2
		db	0,0,18,2
		db	0,0,19,2
		db	0,0,20,2
		db	0,0,21,2
		db	0,0,22,2
		db	0,0,23,2
		db	0,0,24,2
		db	0,0,25,2
		db	0,0,26,2
		db	0,0,27,2
		db	0,0,28,2
		db	0,0,29,2
		db	0,0,30,2
		db	0,0,31,2
		db	0,0,32,2
		db	0,0,33,2
		db	0,0,34,2
		db	0,0,35,2
		db	0,0,36,2
		db	4*max_sectors_curr_sup - ($ - tracktable) dup (0)

; this is a real ugly place to put this
; it should really go in the bds

	public	mediatype
mediatype		db	0

	public	media_set_for_format	; also used by msint13 and msdisk

media_set_for_format	db	0	; 1 if we have done an int 13 set media
					; type for format call
	public	had_format_error
had_format_error	db	0	; 1 if the previous format operation
					; failed.

; temp disk base table. it holds the the current dpt which is then replaced by
; the one passed by "new roms" before we perform a format operation. the old
; dpt is restored in restoreolddpt. the first entry (disk_specify_1) is -1 if
; this table does not contain the previously saved dpt.

	public	tempdpt
tempdpt 	dd	-1


	public	model_byte
model_byte db	0ffh			; model byte set at init time
	public	secondary_model_byte
secondary_model_byte db 0

	public	int19sem
int19sem db	0			; indicate that all int 19
					; initialization is complete

;	we assume the following remain contiguous and their order doesn't change
i19_lst:
	irp	aa,<02,08,09,0a,0b,0c,0d,0e,70,72,73,74,76,77>
	public	int19old&aa
		db	aa&h	; store the number as a byte
int19old&aa	dd	-1	;orignal hardware int. vectors for int 19h.
	endm

num_i19 =	((offset $) - (offset i19_lst))/5

	public	dskdrvs
dskdrvs dw	fdrive1
	dw	fdrive2
	dw	fdrive3
	dw	fdrive4
;M011 -- made all hard drive stuff variable
	dw	22 dup (?)		; up to 26 drives for mini disks

;variables for dynamic relocatable modules
;these should be stay resident.

	public	int6c_ret_addr
int6c_ret_addr	dd	?		; return address from int 6c for p12 machine

;
;   data structures for real-time date and time
;
	public	bin_date_time
	public	month_table
	public	daycnt2
	public	feb29

bin_date_time:
	db	0		; century (19 or 20) or hours (0-23)
	db	0		; year in century (0...99) or minutes (0-59)
	db	0		; month in year (1...12) or seconds (0-59)
	db	0		; day in month (1...31)

month_table:
	dw	0		; january
	dw	31		; february
	dw	59
	dw	90
	dw	120
	dw	151
	dw	181
	dw	212
	dw	243
	dw	273
	dw	304
	dw	334		; december
daycnt2 dw	0000		; temp for count of days since 1-1-80
feb29	db	0		; february 29 in a leap year flag


;************************************************************************
;*									*
;*	entry points into Bios_Code routines.  The segment values	*
;*	  are plugged in by seg_reinit.					*
;*									*
;************************************************************************

	public	cdev
cdev	dd	chardev_entry
	public	ttticks
ttticks	dd	time_to_ticks
bcode_i2f dd	i2f_handler
i13x	dd	i13z
;BEGIN M073
IFDEF	POWER
	extrn	Check_and_Init_APM:far
	public	Check_and_Init_APM_Ptr
Check_and_Init_APM_Ptr dd Bios_Code:Check_and_Init_APM
	extrn	P_UpdFromCMOS:far		;M074
	public	P_UpdFromCMOS_Ptr		;M074
P_UpdFromCMOS_Ptr dd Bios_Code:P_UpdFromCMOS	;M074
ENDIF	;POWER
;END M073
end_BC_entries:

;************************************************************************
;*									*
;*	cbreak - break key handling - simply set altah=3 and iret	*
;*									*
;************************************************************************

	public	cbreak
cbreak	proc	near
	assume	ds:nothing,es:nothing

	mov	altah,3		;indicate break key set

	public	intret		; general purpose iret in the Bios_Data seg
intret:
	iret
cbreak	endp

;************************************************************************
;*									*
;*	strategy - store es:bx (device driver request packet)		*
;*		     away at [ptrsav] for next driver function call	*
;*									*
;************************************************************************

	public	strategy
strategy proc	far
	assume	ds:nothing,es:nothing

	mov	word ptr cs:[ptrsav],bx
	mov	word ptr cs:[ptrsav+2],es
	ret
strategy endp

;************************************************************************
;*									*
;*	device driver entry points.  these are the initial		*
;*	  'interrupt' hooks out of the device driver chain.		*
;*	  in the case of our resident drivers, they'll just		*
;*	  stick a fake return address on the stack which		*
;*	  points to dispatch tables and possibly some unit		*
;*	  numbers, and then call through a common entry point		*
;*	  which can take care of a20 switching				*
;*									*
;************************************************************************

con_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry	; call into code segment handler
	dw	con_table

con_entry endp

;--------------------------------------------------------------------

prn0_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	prn_table
	db	0,0		; device numbers

prn0_entry endp

;--------------------------------------------------------------------

prn1_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	prn_table
	db	0,1

prn1_entry endp

;--------------------------------------------------------------------

prn2_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	prn_table
	db	1,2

prn2_entry endp

;--------------------------------------------------------------------

prn3_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	prn_table
	db	2,3

prn3_entry endp

;--------------------------------------------------------------------

aux0_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	aux_table
	db	0

aux0_entry endp

;--------------------------------------------------------------------

aux1_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	aux_table
	db	1

aux1_entry endp

;--------------------------------------------------------------------

aux2_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	aux_table
	db	2

aux2_entry endp

;--------------------------------------------------------------------

aux3_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	aux_table
	db	3

aux3_entry endp

;--------------------------------------------------------------------

tim_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	tim_table

tim_entry endp

;--------------------------------------------------------------------

	public	dsk_entry	; this entry point is called from msbio2

dsk_entry proc	near
	assume	ds:nothing,es:nothing

	call	cdev_entry
	dw	dsktbl

dsk_entry endp

;--------------------------------------------------------------------

IFDEF	POWER

power_entry	proc	near
	assume	ds:nothing, es:nothing

	call	cdev_entry
	dw	power_table

power_entry	endp

ENDIF

;************************************************************************
;*									*
;*	Ensure A20 is enabled before jumping into code in HMA.		*
;*	This code assumes that if Segment of Device request packet is	*
;*	DOS DATA segment then the Device request came from DOS & that	*
;*	A20 is already on.						*
;*									*
;************************************************************************

cdev_entry proc	near
	assume	ds:nothing,es:nothing
;
; M064 - BEGIN
;
	cmp	inHMA, 0
	je	ce_enter_codeseg; optimized for DOS in HMA

	push	ax
	mov	ax, DosDataSg
	cmp	word ptr [ptrsav+2], ax
	pop	ax
	jne	not_from_dos	; jump is coded this way to fall thru
				;	in 99.99% of the cases
ce_enter_codeseg:
	jmp	cdev
not_from_dos:
	call	EnsureA20On
;
; M064 - END
;
	jmp	short ce_enter_codeseg
cdev_entry endp

;************************************************************************
;*									*
;*	outchr - this is our int 29h handler.  it writes the		*
;*	   character in al on the display using int 10h ttywrite	*
;*									*
;************************************************************************

	public	outchr
outchr	proc	far
	assume	ds:nothing,es:nothing

	push	ax
	push	si
	push	di
	push	bp
	push	bx
	mov	ah,0eh		; set command to write a character
	mov	bx,7		; set foreground color
	int	10h		; call rom-bios
	pop	bx
	pop	bp
	pop	di
	pop	si
	pop	ax
	iret
outchr	endp

;************************************************************************
;*									*
;*	block13 - our int13 hooker					*
;*									*
;************************************************************************

;
; M064 - BEGIN : Removed for Norton cache
;
; A20WasOff is used to save & restore A20 status between transition from
; HMA into LOW mem & vice versa. This will remain initialized to zero
; in DOS=LOW case
;
; A20WasOff	db	0	; M041
;
; M064 - END
;

	public	block13
block13	proc	far
	assume	ds:nothing,es:nothing

	cmp	inHMA,0
	jz	skipa20		; M041

;	mov	A20WasOff, 0	; M041 Assume A20 ON	M064
	call	IsA20off	; M041 A20 Off?
	jnz	skipa20		; M041
;	mov	A20WasOff, 0ffh	; M041 Yes		M064
	call	EnableA20	; assure a20 enabled		;M001
skipa20:
	mov	i13_ds,ds	; M030 -- save caller's ds for call-through
	pushf			; fake interrupt
	call	i13x		; call through Bios_Code entry table
	mov	ds,i13_ds	; M030 -- get called-thru ds

;	pushf			; M041			M064
;	cmp	A20WasOff, 0	; M041			M064
;	jz	@f		; M041			M064
;	call	DisableA20	; M041			M064
;@@:				; M041			M064
;	popf			; M041			M064

	ret	2
block13	endp

;	M030 -- begin added routine

	public	call_orig13

;	the int13 hook calls back here to call-through to the ROM
;	this is necessary because some people have extended their
;	 ROM BIOSs to use ds as a parameter/result register and
;	 our int13 hook relies heavily on ds to access Bios_Data


call_orig13	proc	far

;	pushf				; M041			M064
;	cmp	A20WasOff, 0		; M041 Was A20 off ?	M064
;	je	@f			; M041 No, go ahead	M064
;	call	DisableA20		; M041 Yes, turn A20 off M064
;@@:					; M041			M064
;	popf				; M041			M064

	assume	ds:Bios_Data
	mov	ds,i13_ds		; get caller's ds register
	assume	ds:nothing
	pushf				; simulate an int13
	call	orig13
	mov	i13_ds,ds
	push	cs
	pop	ds			; restore ds -> Bios_Data before return

	pushf				; M041
	cmp	inHMA, 0		; M041
	jz	@f			; M041
;	mov	A20WasOff, 0h		; M041 Assume A20 is ON	M064
	call	IsA20Off		; M041 Is A20 off ?
	jnz	@f			; M041 No, go ahead
;	mov	A20WasOff, 0ffh		; M041 Yes		M064
	call	EnableA20		; M041
@@:					; M041
	popf				; M041

	ret

call_orig13	endp

;	M030 -- end added routine

; M001 - BEGIN

;************************************************************************
;*									*
;*	EnsureA20On - ensure that a20 is enabled if we're running	*
;*	  in the HMA before interrupt entry points into Bios_Code	*
;*									*
;************************************************************************

HiMem	label	dword
	dw	90h
	dw	0ffffh

LoMem	label	dword
	dw	80h
	dw	0h

EnsureA20On	proc near
	assume	ds:nothing,es:nothing
	call	IsA20Off
	jz	ea_enable
	ret

EnableA20	proc	near	; M041
ea_enable:
	push	ax
	push	bx
	mov	ah,5		; localenablea20
	call	xms
	pop	bx
	pop	ax
bie_done:
	ret
EnableA20	endp		; M041

EnsureA20On	endp
;
; M001 - END

; M041 : BEGIN 
;
;----------------------------------------------------------------------------
;
; procedure : IsA20Off
;
;----------------------------------------------------------------------------
;
IsA20Off	proc	near
		push	ds
		push	es
		push	cx
		push	si
		push	di
		lds	si, HiMem
		les	di, LoMem
		mov	cx, 8
		rep	cmpsw
		pop	di
		pop	si
		pop	cx
		pop	es
		pop	ds
		ret
IsA20Off	endp

;
;----------------------------------------------------------------------------
;
; procedure : DisableA20
;
;----------------------------------------------------------------------------
;
DisableA20	proc	near
		push	ax
		push	bx
		mov	ah,6		; localdisable a20
		call	xms
		pop	bx
		pop	ax
		ret
DisableA20	endp

; M041 : END

;************************************************************************
;*									*
;*	int19 - bootstrap interrupt -- we must restore a bunch of the	*
;*	  interrupt vectors before resuming the original int19 code	*
;*									*
;************************************************************************


	public	int19
int19	proc	far
	assume	ds:nothing,es:nothing

	push	cs
	pop	ds
	assume	ds:Bios_Data

	mov	es,zeroseg

	mov	cx, NUMROMVECTORS	; no. of rom vectors to be restored
	mov	si, offset RomVectors	; point to list of saved vectors
next_int:
	lodsb				; get int number
	cbw				; assume < 128
	shl	ax, 1
	shl	ax, 1			; int * 4
	mov	di, ax
	lodsw
	stosw
	lodsw
	stosw				; install the saved vector
	loop	next_int

	cmp	byte ptr int19sem,0	; don't do the others unless we
	jz	doint19			; set our initialization complete flag

;	stacks code has changed these hardware interrupt vectors
;	stkinit in sysinit1 will initialize int19holdxx values.

	mov	si,offset i19_lst
	mov	cx,num_i19

i19_restore_loop:
	lodsb			; get interrupt number
	cbw			; assume < 128
	mov	di,ax		; save interrupt number
	lodsw			; get original vector offset
	mov	bx,ax		; save it
	lodsw			; get original vector segment
	cmp	bx,-1		; check for 0ffffh (unlikely segment)
	jz	i19_restor_1	;opt no need to check selector too 
	cmp	ax,-1		;opt 0ffffh is unlikely offset
	jz	i19_restor_1

	add	di,di
	add	di,di
	xchg	ax,bx
	stosw
	xchg	ax,bx
	stosw			; put the vector back

i19_restor_1:
	loop	i19_restore_loop

doint19:
;
; M036 - BEGIN
;
	cmp	inHMA, 0	; Is dos running from HMA
	je	SkipVDisk
	call	EraseVDiskHead	; Then erase our VDISK header at 1MB boundary
				;  Some m/c's (AST 386 & HP QS/16 do not
				;  clear the memory above 1MB during a warm
				;  boot.
SkipVDisk:
;
; M036 - END
;
	int	19h
int19	endp
;
; M036 - BEGIN
;
;
;----------------------------------------------------------------------------
;
; procedure : int15
;
;		Int15 handler for recognizing ctrl-alt-del seq
;		If it recognizes ctrl-alt-del and if DOS was
;		is running high, it Erases the VDISK header
;		present at 1MB boundary
;
;----------------------------------------------------------------------------
;
DELKEY		equ	53h
ROMDATASEG	equ	40h
KBFLAG		equ	17h
CTRLSTATE	equ	04h
ALTSTATE	equ	08h

	public	Int15
Int15	proc	far
	assume	ds:nothing
	cmp	ax, (4fh shl 8) + DELKEY	; del keystroke ?
	je	@f
	jmp	dword ptr Old15			
@@:
	push	ds
	push	ax
	mov	ax, ROMDATASEG
	mov	ds, ax
	mov	al, byte ptr ds:[KBFLAG]
	and	al, (CTRLSTATE or ALTSTATE)
	cmp	al, (CTRLSTATE or ALTSTATE)	; is ctrl-alt active?
	jne	@f

	push	cs
	pop	ds
	assume	ds:Bios_Data
	cmp	inHMA, 0			; is DOS running from HMA
	je	@f
	call	EraseVDiskHead
@@:
	pop	ax
	pop	ds
	assume	ds:nothing
	stc
	jmp	dword ptr Old15
Int15	endp
;
;----------------------------------------------------------------------------
;
; procedure : EraseVDiskHead
;
;		Erases the VDisk Header present in the 1MB boundary
;
;----------------------------------------------------------------------------
;
EraseVDiskHead	proc	near
	push	ax
	push	cx
	push	di
	push	es
	call	EnsureA20On
	mov	ax, 0ffffh	; HMA seg
	mov	es, ax
	mov	di, 10h		; point to VDISK header
	mov	cx, 10h		; size of vdisk header
	xor	ax, ax
	rep	stosw		; clear it
	pop	es
	pop	di
	pop	cx
	pop	ax
	ret
EraseVDiskHead	endp
;
; M036 - END
;
;
;************************************************************************
;*									*
;*	the int2f handler chains up to Bios_Code through here.		*
;*	  it returns through one of the three functions that follow.	*
;*	  notice that we'll assume we're being entered from DOS, so	*
;*	  that we're guaranteed to be A20 enabled if needed		*
;*									*
;************************************************************************

int_2f	proc	far
	assume	ds:nothing,es:nothing
	jmp	bcode_i2f
int_2f	endp


;	re-enter here to transition out of hma mode and jmp to dsk_entry
;	   note:  is it really necessary to transiton out and then back
;	   in?  It's not as if this is a really speed critical function.
;	   might as well do whatever's most compact.

	public	i2f_dskentry
i2f_dskentry proc far
	jmp	dsk_entry
i2f_dskentry endp


;************************************************************************
;*									*
;*	re_init - called back by sysinit after a bunch of stuff		*
;*		is done.  presently does nothing.  affects no		*
;*		registers!						*
;*									*
;************************************************************************

	public	re_init
re_init proc	far
	assume	ds:nothing,es:nothing
	ret
re_init endp


;SR; WIN386 support
; WIN386 instance data structure
;
;
; Here is a Win386 startup info structure which we set up and to which
; we return a pointer when Win386 initializes.
;

public	Win386_SI, SI_Version, SI_Next

Win386_SI	label	byte		; Startup Info for Win386
SI_Version	db	3, 0		; for Win386 3.0
SI_Next		dd	?		; pointer to next info structure
		dd	0		; a field we don't need
		dd	0		; another field we don't need
SI_Instance	dw	Instance_Table, Bios_Data ; far pointer to instance table

;
; This table gives Win386 the instance data in the BIOS and ROM-BIOS data
; areas.  Note that the address and size of the hardware stacks must
; be calculated and inserted at boot time.
;
Instance_Table	label	dword
	dw	00H, 50H		; print screen status...
	dw	02			; ...2 bytes
	dw	0Eh, 50H		; ROM Basic data...
	dw	14H			; ...14H bytes
	dw	ALTAH, Bios_Data	; a con device buffer...
	dw	01			; ... 1 byte
IF STACKSW
public NextStack
NextStack	label dword

;	NOTE:  If stacks are disabled by STACKS=0,0, the following
;		instance items WILL NOT be filled in by SYSINIT.
;		That's just fine as long as these are the last items
;		in the instance list since the first item is initialized
;		to 0000 at load time.

	dw	0, 0		; pointer to next stack to be used...
	dw	02			; ...2 bytes
; The next item in the instance table must be filled in at sysinit time
public IT_StackLoc, IT_StackSize
IT_StackLoc	dd	?		; location of hardware stacks
IT_StackSize	dw	?		; size of hardware stacks
ENDIF
	dd	0			; terminate the instance table

;SR;
; Flag to indicate whether Win386 is running or not
;
public	IsWin386
IsWin386		db	0

;
;This routine was originally in BIOS_CODE but this causes a lot of problems
;when we call it including checking of A20. The code being only about 
;30 bytes, we might as well put it in BIOS_DATA
;
PUBLIC	V86_Crit_SetFocus

V86_Crit_SetFocus	PROC	FAR

			push	di
			push	es
			push	bx
			push	ax

			xor	di,di
			mov	es,di
			mov	bx,0015h	;Device ID of DOSMGR device
			mov	ax,1684h	;Get API entry point
			int	2fh
			mov	ax,es
			or	ax,di		
			jz	Skip
;
;Here, es:di is address of API routine. Set up stack frame to simulate a call
;
			push	cs		;push return segment
			mov	ax,OFFSET Skip
			push	ax		;push return offset
			push	es
			push	di		;API far call address
			mov	ax,1		;SetFocus function number
			retf			;do the call
Skip:
			pop	ax
			pop	bx
			pop	es
			pop	di
			ret
V86_Crit_SetFocus	ENDP



;
;End WIN386 support
;

		public	FreeHMAPtr
		public	MoveDOSIntoHMA
FreeHMAPtr	dw	-1
MoveDOSIntoHMA	dd	sysinitseg:FTryToMovDOSHi


;SR;
; A communication block has been setup between the DOS and the BIOS. All
;the data starting from SysinitPresent will be part of the data block. 
;Right now, this is the only data being communicated. It can be expanded 
;later to add more stuff
;
		public	SysinitPresent
SysinitPresent	db	0

IFNDEF	ROMDOS						; M097

; this will be the end of the BIOS data if no hard disks are in system
; in DISK based DOS

	public	endfloppy
endfloppy label byte

ENDIF ; NOT ROMDOS					; M097

Bios_Data ends

;
; Possibly disposable BIOS data
; This data follows the regular BIOS data, and is part of the same group.
;

Bios_Data_Init	segment

; M097 : begin

IFDEF	ROMDOS

; this will be the end of the BIOS data if no hard disks are in system
; in ROMDOS

	public	endfloppy
endfloppy label byte

ENDIF ; ROMDOS

; M097 : end

;	M011 -- begin changed section

;	this stuff is only used for changeline support

	public	nul_vid
	public	tmp_vid
nul_vid db  "NO NAME    ",0	; null volume id
tmp_vid db  "NO NAME    ",0	; vid scratch buffer

	public	end96tpi
end96tpi label	byte

	public	harddrv
harddrv db	80h			;physical drive number of first hardfile
;*********************************************************************
;memory allocation for bdss
;*********************************************************************

	public bdss
bdss	BDS_STRUC (2+max_mini_dsk_num) dup (<>)	;currently max. 25

;M011 -- end of changed section

Bios_Data_Init	ends


;	okay.  so much for Bios_Data.  Now let's put our device driver
;	  entry stuff up into Bios_Code.

Bios_Code	segment
	assume	cs:Bios_Code

; ORG a bit past zero to leave room for running in HMA...

	org	30h
	public	BCode_start
BCode_start:

;	device driver entry point tables

	extrn	dsktbl:near
	extrn	con_table:near
	extrn	tim_table:near
	extrn	prn_table:near
	extrn	aux_table:near

IFDEF POWER
	extrn	power_table:near
ENDIF

	extrn	i2f_handler:far
	extrn	time_to_ticks:far
	extrn	i13z:far

	public	Bios_Data_Word
Bios_Data_Word	dw	Bios_Data

;************************************************************************
;*									*
;*	seg_reinit is called with ax = our new code segment value,	*
;*	  trashes di, cx, es						*
;*									*
;*	cas -- should be made disposable!				*
;*									*
;************************************************************************

	public	seg_reinit
seg_reinit	proc	far
	assume	ds:nothing,es:nothing

	mov	es,Bios_Data_Word
	assume	es:Bios_Data
	mov	di,2+offset cdev
	mov	cx,((offset end_BC_entries) - (offset cdev))/4

seg_reinit_1:
	stosw				; modify Bios_Code entry points
	inc	di
	inc	di
	loop	seg_reinit_1
	ret
seg_reinit	endp

;************************************************************************
;*									*
;*	chardev_entry - main device driver dispatch routine		*
;*	   called with a dummy parameter block on the stack		*
;*	   dw dispatch_table, dw prn/aux numbers (optional)		*
;*									*
;*	will eventually take care of doing the transitions in		*
;*	   out of Bios_Code						*
;*									*
;************************************************************************

chardev_entry	proc	far
	assume	ds:nothing,es:nothing

	push	si
	push	ax
	push	cx
	push	dx
	push	di
	push	bp
	push	ds
	push	es
	push	bx
	mov	bp,sp			; point to stack frame
	mov	si,18[bp]		; get return address (dispatch table)
	mov	ds,Bios_Data_Word	;  load ds: -> Bios_Data
	assume	ds:Bios_Data
	mov	ax,word ptr 2[si]	; get the device number if present
	mov	byte ptr [auxnum],al
	mov	byte ptr [printdev],ah
	mov	si,word ptr [si]	; point to the device dispatch table

	les	bx,[ptrsav]		;get pointer to i/o packet

	mov	al,byte ptr es:[bx].unit	;al = unit code
	mov	ah,byte ptr es:[bx].media	;ah = media descrip
	mov	cx,word ptr es:[bx].count	;cx = count
	mov	dx,word ptr es:[bx].start	;dx = start sector

	cmp	si,offset dsktbl
	jnz	no_sector32_mapping


;*********************************************************************
;
;	Special case for 32-bit start sector number:
;	   if (si==dsktbl) /* if this is a disk device call */
;	      set high 16 bits of secnum to 0
;	      if (secnum == 0xffff) fetch 32 bit sector number
;
;	pass high word of sector number in start_sec_h, low word in dx
;
; note: start_l and start_h are the offsets within the io_request packet
;	  which contain the low and hi words of the 32 bit start sector if
;	  it has been used.
;
; note:remember not to destroy the registers which have been set up before


	mov	start_sec_h,0		; initialize to 0
	cmp	dx,-1
	jnz	no_sector32_mapping

	mov	dx,es:[bx].start_h	; 32 bits dsk req
	mov	start_sec_h,dx		; start_sec_h = packet.start_h
	mov	dx,es:[bx].start_l	; dx = packet.start_l

no_sector32_mapping:
	xchg	di,ax
	mov	al,byte ptr es:[bx].cmd
	cmp	al,cs:[si]
	jae	command_error

	cbw				; note that al <= 15 means ok
	shl	ax,1

	add	si,ax
	xchg	ax,di

	les	di,dword ptr es:[bx].trans

	cld				; ***** always clear direction
	call	cs:word ptr [si+1] 	;go do command
	assume	ds:nothing

	jc	already_got_ah_status	; if function returned status, don't
	mov	ah,1			;  load with normal completion

already_got_ah_status:
	mov	ds,Bios_Data_Word	; cas///// note: shouldn't be needed!
	assume	ds:Bios_Data
	lds	bx,[ptrsav]
	assume	ds:nothing
	mov	word ptr [bx].status,ax ;mark operation complete

	pop	bx
	pop	es
	pop	ds
	pop	bp
	pop	di
	pop	dx
	pop	cx
	pop	ax
	pop	si
	add	sp,2		; get rid of fake return address

chardev_entry endp		; fall through into bc_retf

	public	bc_retf
bc_retf	proc	far
	assume	ds:nothing,es:nothing

	ret

bc_retf	endp


command_error:
	call	bc_cmderr
	jmp	short already_got_ah_status

;
;----------------------------------------------------------------------------
; The following piece of hack is for supporting CP/M compatibility
; Basically at offset 5 we have a far call into 0:c0. But this does not call
; 0:c0 directly instead it call f01d:fef0, because it needs to support 'lhld 6'
; The following hack has to reside at ffff:d0 (= f01d:fef0) if BIOS is loaded
; high.
;----------------------------------------------------------------------------
;
	dw	?	; pad to bring offset to 0d0h
if2
	if ( offset off_d0 - 0d0h )
		%out CP/M compatibilty broken!!!
		%out Please re-pos hack to ffff:d0
	endif
endif

	public	off_d0
off_d0	db	5 dup (?)	; 5 bytes from 0:c0 will be copied onto here
				;  which is the CP/M call 5 entry point
	.errnz (offset off_d0 - 0d0h)


;----------------------------------------------------------
;
;	exit - all routines return through this path
;

	public	bc_cmderr
bc_cmderr:
	mov	al,3			;unknown command error

;	now zero the count field by subtracting its current value,
;	  which is still in cx, from itself.


;	subtract the number of i/o's NOT YET COMPLETED from total
;	  in order to return the number actually complete


	public	bc_err_cnt
bc_err_cnt:
	assume	ds:Bios_Data
	les	bx,[ptrsav]
	assume	es:nothing
	sub	es:word ptr [bx].count,cx;# of successful i/o's
	mov	ah,81h			;mark error return
	stc				; indicate abnormal end
	ret

Bios_Code	ends


;	the last real segment is sysinitseg

sysinitseg	segment
	assume	cs:sysinitseg
	extrn	FTryToMovDOSHi:far
	public	SI_start
SI_start:
sysinitseg	ends

	end
