IFDEF	DEBUG
rev_msg	db	"POWER Rev. 2 (11/14/91)",13,10,'$'
ENDIF

; error messages

bad_command_msg     db      "Invalid parameter",13,10,"$"
;
open_failed_msg     db      13,10,"Power Manager (POWER.EXE) not installed.",13,10,10,'$' ; M008
;
stats_failed_msg    db      "Error accessing power manager.",13,10,"$"


IFDEF	INCL_APM

;------------------------------------------------------------------------
; the following message will have one of OFFLINE /ONLINE
; as an insert between ACLine_Stat1 and ACLine_Stat2.
;	e.g.: "AC Line Status : OFFLINE "
;
ACLine_Stat1	db	13,10,"AC Line Status : ",'$'
ACLine_Stat2	db	13,10,'$'

AC_Offline_str	db	"OFFLINE",'$'
AC_Online_str	db	"ONLINE",'$'

;------------------------------------------------------------------------
; TRANSLATORS: NOTE:
; the following message will have one of battery_high/low/critical/charging
; as an insert between battery_status1 and battery_status2.
;	e.g.: "Battery status : Critical"
battery_status1	db	13,10,"Battery status : ",'$'
battery_status2	db	13,10,'$'

battery_high	db	"High",'$'
battery_low	db	"Low",'$'
battery_critical	db	"Critical",'$'
battery_charging	db	"Charging",'$'
;------------------------------------------------------------------------

;TRANSLATORS, NOTE:
; The next message is displayed together with a no between batter_life_str1
; and str2.
battery_life_str1	db	13,10,"Battery life (%) : "
battery_life_str2	db	13,10,'$'
;------------------------------------------------------------------------

ENDIF	; of INCL_APM

; other messages

;----------------------------------------------------------------------
;TRANSLATORS, NOTE:
; The next message is displayed together with a no between cpu_idle_msg1
; and msg2.
; e.g. "20% of CPU time idle"
cpu_idle_msg1   db      "$"
cpu_idle_msg2   db      "% of CPU time idle.",13,10,"$"
;----------------------------------------------------------------------
; TRANSLATORS: NOTE:
; One of the strings - on_msg,off_msg,noapp_msg - will be displayed
; between stat1_msg and stat2_msg.
; E.G.	CR,LF,"Power Manager : NOAPP",CR,LF

power_stat1_msg	db	13,10,"Power Manager : ",'$'
power_stat2_msg	db	13,10,'$'

; M089 BEGIN - UI changes
power_allon_msg	db	"ADV",'$'
power_off_msg	db	"OFF",'$'
power_noid_msg	db	"STD",'$'
;---------------------------------------------------------------------
help_text       db      "Reduces power used by your computer.",13,10,13,10
                db      "POWER [ADV | STD | OFF]",13,10				; M008
                db      "  ADV - Conserves power by monitoring applications"
		db	" and hardware devices.",13,10
		db	"  STD - Conserves power by monitoring hardware devices.",13,10
                db      "  OFF - Turns off power management.",13,10
; M089 END

IFDEF      DEBUG
                db      "  SOUND - toggle speaker during idle (debug version only).",13,10
ENDIF			; IFDEF DEBUG
                db      "$"


