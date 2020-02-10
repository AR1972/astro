;**************************************************************
;*  APMEQU.INC - Include file for APM 
;*
;*  Microsoft Confidential
;*  Copyright (C) Microsoft Corporation 1991
;*  All Rights Reserved.
;*
;*  Revision History:
;*  ----------------
;*  08/1/91  NSM	Created
;*
;**************************************************************
;
;*************************************************************
; APM function codes 
;*************************************************************
;
APM_INSTALLCHK_FUNC	equ	5300h
;
APM_CONNECT_FUNC	equ	5301h	; real mode connect
APM_DISCONNECT_FUNC	equ	5304h
;
APM_CPUIDLE_FUNC	equ	5305h
APM_CPUBUSY_FUNC	equ	5306h
APM_SETPWSTATE_FUNC	equ	5307h	; set power state
APM_ENABLE_DISABLE_FUNC	equ	5308h
APM_REST_DEFLT_FUNC	equ	5309h	; restore power-on defaults
APM_GETPWSTATE_FUNC	equ	530ah	; get power state	
APM_GETPMEVENT_FUNC	equ	530bh


;*************************************************************
; values for APM device ids 
;*************************************************************
;
; SYSTEM clas
APM_SYSTEM_BIOS	equ	0000h
APM_SYSTEM_DEV	equ	0001h
APM_SYSTEM_CPU	equ	0002h
;display class
APM_DISPLAY_ALL	equ	0100h
; parallel ports
APM_PARALLEL_ALL	equ	0200h
; SERIAL ports
APM_SERIAL_ALL	equ	0300h
;
; global ID ; set containing all power managed devices
APM_ALL_DEVICES	equ	0ffffh
;
;*************************************************************
; sub-function codes for some of the above functions
;*************************************************************

; values for ENABLE_DISABLE_FUNC
APM_ENABLE_FUNC	equ	1
APM_DISABLE_FUNC	equ	0

;*************************************************************

; return values of some APM functions:

;*************************************************************

APM_SIGNATURE	equ	"PM"		; M003: APM signature is:
					; BH = 'P' and BL = 'M'
;
; PM events 
;
APM_STANDBY_REQ	equ	01
APM_SUSPEND_REQ	equ	02
APM_NORM_RESUME	equ	03
APM_CRIT_RESUME	equ	04
APM_BATT_LOW	equ	05


; APM_FLAGS masks (returned by APM detect call)
APM_SLOW_CLOCK	equ	0100B	; bit 2  = 1 if CPU_IDLE slows processor clock
APM_PWMGMT_DISABLED	equ	1000b	; bit3 = 1 if power mgmt. disabled


