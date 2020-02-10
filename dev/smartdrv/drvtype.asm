;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

include bambi.inc
include devsym.inc      ; get device driver structure definitions
include dpb.inc

public  IsMagicDrive
public  get_drive_type
public  setup_default_drive_list

;
;       routines from bambinit.asm
;
extrn   get_DPB                         :near
extrn   dos_3x                          :word
;
;       data from cmdline.asm
;
extrn   drives_to_cache                 :byte
extrn   msg_and_fail                    :near
extrn   dosinfo                         :word
extrn   display_message                 :near

extrn   first_instance                  :byte
extrn   shutupmsdosflag                 :byte
extrn   warnmsdosmessage                :byte

zseg    segment public 'CODE'

	assume  cs:zseg
	assume  ds:zseg
;
; locals
;

ioctl_buffer    db      64 dup(?)       ;buffer for ioctl 73h
thisdrivetype   dw      ?

DIRSTRLEN       EQU     64+3            ; Max length in bytes of directory strings
TEMPLEN         EQU     DIRSTRLEN*2


CURDIR_LIST     STRUC
CURDIR_TEXT     DB      DIRSTRLEN DUP (?)   ; text of assignment and curdir
CURDIR_FLAGS    DW      ?               ; various flags
CURDIR_DEVPTR   DD      ?               ; local pointer to DPB or net device
CURDIR_ID       DW      2 dup (?)       ; cluster of current dir (net ID)
CURDIR_USER_WORD DW     ?
CURDIR_END      DW      ?               ; index to ".." backup limit - see above
CURDIR_TYPE     DB      ?               ; IFS drive (2=ifs, 4=netuse)
CURDIR_IFS_HDR  DD      ?               ; Ptr to File System Header
CURDIR_FSDA     DB      2 DUP (?)       ; File System Dependent Data Area
CURDIR_LIST     ENDS

curdirLen       EQU     Size CURDIR_list        ; Needed for screwed up
						; ASM87 which doesn't allow
						; Size directive as a macro
						; argument
CURDIR_netID    EQU     DWORD PTR CURDIR_ID


;**     Flag values for CURDIR_FLAGS

CURDIR_isnet    EQU     1000000000000000B
CURDIR_isifs    EQU     1000000000000000B
CURDIR_inuse    EQU     0100000000000000B
CURDIR_splice   EQU     0010000000000000B
CURDIR_local    EQU     0001000000000000B



SysInitVars STRUC
SYSI_DPB            DD      ?           ; DPB chain
SYSI_SFT            DD      ?           ; SFT chain
SYSI_CLOCK          DD      ?           ; CLOCK device
SYSI_CON            DD      ?           ; CON device
SYSI_MAXSEC         DW      ?           ; maximum sector size
SYSI_BUF            DD      ?           ; points to Hashinitvar
SYSI_CDS            DD      ?           ; CDS list
SYSI_FCB            DD      ?           ; FCB chain
SYSI_Keep           DW      ?           ; keep count
SYSI_NUMIO          DB      ?           ; Number of block devices
SYSI_NCDS           DB      ?           ; number of CDS's
SYSI_DEV            DD      ?           ; device list
SYSI_ATTR           DW      ?           ; null device attribute word
SYSI_STRAT          DW      ?           ; null device strategy entry point
SYSI_INTER          DW      ?           ; null device interrupt entry point
SYSI_NAME           DB   8 DUP(?)       ; null device name
SYSI_SPLICE         DB      0           ; TRUE -> splicees being done
SYSI_IBMDOS_SIZE    DW      ?           ; DOS size in paragraphs
SYSI_IFS_DOSCALL@   DD      ?           ; IFS DOS service rountine entry
SYSI_IFS            DD      ?           ; IFS header chain
SYSI_BUFFERS        DW      ?,0         ; BUFFERS= values (m,n)
SYSI_BOOT_DRIVE     DB      ?           ; boot drive A=1 B=2,..
SYSI_DWMOVE         DB      0           ; 1 if 386 machine
SYSI_EXT_MEM        DW      0           ; Extended memory size in KB.
SysInitVars ENDS

NET_MAP         EQU    1000000000000b ; Mask for net drive bits
;
; code taken from DOS 5 setup to determine if drive is a network.
; There is a bug in dos 4.0 that requires us to slime around
;
;RETURNS

NON_LOCAL_DRIVE         equ     0
LOCAL_DRIVE             equ     1
NOTSUPP_DRIVE           equ     2

drive   db 0
IsLocalDrive proc near
	push    es
	push    dx
	mov     drive,dl

	mov     AX,3000h
	int     21h
	
	cmp     AL,04
	je      LookInCDS

	mov     AX,4409h                ; IOCTL is redirected? function 
	mov     BL,Drive                ; BL = drive number
	int     21h
	jc      Notsupp                 ; If not supported then can't be net

	mov     AX,LOCAL_DRIVE          ; Assume it is a local drive


	test    DX,NET_MAP              ; See if any funny bits are set
	jz      IsLocalExit             ; No funny bits so must be local

	mov     ax,NON_LOCAL_DRIVE
	jmp     short IsLocalExit

LookInCDS:
	mov     AH,52h                  ; Get internal DOS DATA Segment
	int     21h                     ; ES:BX --> DOS interal varibles

	mov     CL,Drive                ; First make sure that this drive
	dec     CL                      ; is not greater than last drive
					; If CL is zero it will fail the
	cmp     CL,ES:[BX].SYSI_NCDS    ; compare by doing  unsigned cmp
	mov     AX,LOCAL_DRIVE
	jae     Notsupp

	les     BX,ES:[BX].SYSI_CDS     ; ES:BX --> First CDS entry

	mov     AX,SIZE CURDIR_LIST     ; Find offset of the entry indexed
	mul     CL                      ; by drive in CL
	add     BX,AX                   ; ES:BX --> CDS entry for drive
	test    es:[bx].CURDIR_FLAGS,CURDIR_inuse
	jz      Notsupp
	test    ES:[BX].CURDIR_FLAGS,CURDIR_isnet ; Test is net bit

	mov     ax,NON_LOCAL_DRIVE
	jnz     IsLocalExit             ; If true then jmp to exit
	mov     ax,LOCAL_DRIVE
	
IsLocalExit:
	pop     dx
	pop     es
	ret
Notsupp:
	mov     ax,NOTSUPP_DRIVE
	jmp     short islocalexit

IsLocalDrive ENDP

;                       
;input dl == drive letter,zero based
;
;output NZ -> magicdrv, BX & !80h = host drive letter
;       Z ->  not magicdrv
IsMagicDrive proc near
	push    dx
	push    es
	push    di
	push    si

	push    dx
	mov     ax,MAGICDRV_2F          ;is magicdrv installed?
	mov     bx,MAGICDRV_DETECT      
	int     2fh
	pop     dx
	cmp     bx,MD_STAMP
	jne     no_magic_drv_installed
	
	mov     ax,MAGICDRV_2F
	mov     bx,MAGICDRV_MAP
	int     2fh
	;;; bit 7 set means its a compressed drive
	;;; dl == bl without bit 7 means its a regular drive
	;;; dl != bl without bit 7 means its a swapped host drive
	test    bl,80h                  ;compressed drive?
return_magictest:
	pop     si
	pop     di
	pop     es
	pop     dx
	ret
no_magic_drv_installed:
	xor     ax,ax   ;zet zero
	jmp     short return_magictest
IsMagicDrive endp

;
;FUNCTION
;       detect drive type
;INPUT
;       dx = drive unit
;OUTPUT
;       ax = drive type
;               INVALID_TYPE    = 0
;               FLOPPY_TYPE     = 1
;               REMOTE_TYPE     = 2
;               HARDDISK_TYPE   = 3
;               RAMDRIVE_TYPE   = 4 
;               CDROM_TYPE      = 5
;               MEMORY_TYPE     = 6
;               MAGIC_TYPE      = 7
;
;USES
;       ALL except ES,DS
;
get_drive_type proc near
    call        IsMagicDrive
    jnz         dt_magic
    ;;;; first check is for CDROM
    inc dx              ;really want drive letter
    mov ax,0150bh
    xor bx,bx
    mov cx,dx
    dec cx
    int 2fh
    cmp bx,0ADADh
    jne notCDROM
    cmp ax,0
    je notCDROM
    mov ax,CDROM_TYPE
    jmp short dt_end
notCDROM:
    ;;;;
;    push dx
;    mov ax,440dh       ;generic ioctl get_system_info
;    mov cx,0873h
;    mov bx,dx
;    mov dx,offset ioctl_buffer
;    int 21h
;    pop dx
;    jc  not_memory_device
;    mov ax,MEMORY_TYPE
;    jmp dt_end
;not_memory_device:     

    call islocaldrive
    cmp  ax,LOCAL_DRIVE
    je   dt_local_check
    cmp  ax,NOTSUPP_DRIVE
    je   dt_not_found

    mov ax, REMOTE_TYPE
    jmp short dt_end

dt_local_check:
    mov bl,dl
    mov ax, 4408h
    int 21h
    ; ZZZZ
    ; WARNING!! assuming that drive is non-removable as call 1 succeeded!
    ; WINDOWS FM seems to do the same!
    ; jc        dt_not_found
    jc  dt_nonremovable
    or  ax, ax
    jz  dt_removable

dt_nonremovable:
					;WARNING dl is UNIT now
    push dx
    push ds                             ;
    push bx                             ;result of get_dpb in ds:bx
    mov  dx,bx
    call get_DPB
   jc   dt_inv1 ; if not a local drive, bail out w/ INVALID code
    cmp  ds:[bx].dpb_fat_count,1        ;only one fat means RAMDRIVE
    pop  bx
    pop  ds
    pop  dx     

    je  dt_ramdrive
    mov ax, HARDDISK_TYPE
    jmp short   dt_end

dt_ramdrive:
	
    mov ax,RAMDRIVE_TYPE
    jmp short dt_end

dt_removable:
    mov ax, FLOPPY_TYPE
    jmp short dt_end

dt_magic:
    mov ax, MAGIC_TYPE
    jmp short dt_end
dt_inv1:
	pop     bx
	pop     ds              ; restore stack
	pop     dx
dt_not_found:
    xor ax, ax

dt_end:
    add         ax,dosinfo
    ret
get_drive_type endp

;
;FUNCTION
;       initialize array which determines what drives to cache
;       to include all hard disks with read+write caching enabled
;
setup_default_drive_list proc near

	mov     cx,26
loop_units:
	mov     ax,MULT_BAMBI
	mov     bx,BAMBI_DONT_CACHE_DRIVE
	int     2fh
	cmp     ax,BAMBI_DONT_CACHE_DRIVE
	je      continue_loop_units
;;;
;;;New code added for Astro to detect magic drive 7/30/92 scottq
;;;
	push    cx
	push    es
	push    di
	push    si

	push    dx
	mov     ax,MAGICDRV_2F          ;is magicdrv installed?
	mov     bx,MAGICDRV_DETECT      
	push    cx
	int     2fh
	pop     cx
	pop     dx
	cmp     bx,MD_STAMP
	jne     no_magic_drv
	
	dec     cx

	mov     ax,MAGICDRV_2F
	mov     bx,MAGICDRV_MAP
	mov     dl,cl
	int     2fh
	;;; bit 7 set means its a compressed drive
	;;; dl == bl without bit 7 means its a regular drive
	;;; dl != bl without bit 7 means its a swapped host drive
	test    bl,80h                  ;compressed drive
	pop     si
	pop     di
	pop     es
	pop     cx
	jz      mightbehostdrv        
	jmp     short continue_loop_units
no_magic_drv:
	pop     si
	pop     di
	pop     es
	pop     cx
	jmp     short continue_drivecheck
mightbehostdrv:
	cmp     dl,bl
	jne     default_cache_hd        ;;bug bug scottq 7/30/92 write cache on mounted floppies
continue_drivecheck:
	push    cx                              ;check each drive
	mov     dx,cx                           ;to see if it is a hard disk
	dec     dx
	call    get_drive_type                  ;if so, settup global list
	mov     thisdrivetype,ax
	pop     cx
	sub     ax,dosinfo
	cmp     ax,HARDDISK_TYPE
	je      default_cache_hd
	cmp     ax,FLOPPY_TYPE
	je      default_cache_floppy
continue_loop_units:
	loop    loop_units
	
	call    detect_dont_cache_drives

	cmp     thisdrivetype,MEMORY_TYPE
	jna     baddisk
contbad:        

	ret
default_cache_hd:
	mov     bp,cx
	mov     drives_to_cache[bp-1],READ_CACHE or WRITE_CACHE ;read/write cache
	jmp     short   continue_loop_units
baddisk:

	cmp     first_instance,1
	jne     contbad

	mov     warnmsdosmessage,1      ;cant do it yet, since command line
	jmp     short contbad           ;has to be parsed.

default_cache_floppy:
	mov     bp,cx
	mov     drives_to_cache[bp-1],READ_CACHE        ;read cache only
	jmp     short   continue_loop_units

setup_default_drive_list endp

detect_dont_cache_drives proc near

 ;      int 1
	push    ds
	push    es
	push    bp
	push    si
	push    di

	mov     ah,52h
	int     21h
	add     bx,34
	;;;     es:bx->device header chain
	push    es
	pop     ds
assume ds:nothing
	mov     bp,bx
	;;;     ds:bp->device header chain

	xor     di,di                   ; di is index into new header table
next_chain:
	les     bx,es:[bx]
	cmp     bx,-1
	je      done_walking_device_chain
	test    word ptr es:[bx].sdevatt,8000h          ;block device?
	jnz     next_chain

;;
;; is it a squished drive? if so, do not cache.  We can detect
;; squish drive by checking to see if the devicename field is "SQUISH+"

	cmp     word ptr es:[bx].sdevname[1],'QS'
	jne     next_name
	cmp     word ptr es:[bx].sdevname[3],'IU'       
	jne     next_name
	cmp     word ptr es:[bx].sdevname[5],'HS'
	jne     next_name
	cmp     byte ptr es:[bx].sdevname[7],'+'
	jne     next_name
	jmp     short   dontcachethisdrive
next_name:

	cmp     word ptr es:[bx].sdevname[1],'TS'
	jne     next_chain
	cmp     word ptr es:[bx].sdevname[3],'CA'       
	jne     next_chain
	cmp     word ptr es:[bx].sdevname[5],'C-'
	jne     next_chain
	cmp     byte ptr es:[bx].sdevname[7],'D'
	jne     next_chain

dontcachethisdrive:


	push    es
	push    bx

	mov     si,bx                           ;di:si->driver header
	mov     di,es

	mov     dx,1                            ;find unit for driver
loop_dpb:
	call    get_DPB                         ;ds:bx -> dpb or unit dl
	jc      nodpb                           ;skip if error

	add     bx,cs:dos_3x                    ; adjust pointer for DOS 3.x dpb

	cmp     word ptr ds:[bx].dpb_driver_addr[0],si
	jne     do_next_dpb
	cmp     word ptr ds:[bx].dpb_driver_addr[2],di
	jne     do_next_dpb

	push    bp
	mov     bp,dx
	mov     drives_to_cache[bp-1],NO_CACHE  
	pop     bp
	jmp     short do_next_dpb
nodpb:
	pop     bx
	pop     es
	jmp     next_chain

do_next_dpb:
	inc     dl
	jmp     short loop_dpb

done_walking_device_chain:
	pop     di
	pop     si
	pop     bp
	pop     es
	pop     ds
	ret

detect_dont_cache_drives endp

zseg ends

end

