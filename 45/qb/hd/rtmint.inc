;***
;rtmint.inc - 09/03/87 - runtime module communications interrupts
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; This file is defines the runtime module communications interrupts. Any entry
; exists in this file for each runtime entry point that can be called by
; compiled code.
;
; This file is used in two places. It is included in rtmint.asm to create a
; module which compiled programs FAR CALL into to get dispatched to the runtime
; module. It is also included in rtm86.asm (or an interpeter equivalent) to
; dispatch to the intended routines.
;
; The reason this file exists is so that the interrupt number assignments are
; more or less version independent (at the expense of a big maintenence
; hassle.) Also, there are very few conditionals because the generic and full
; versions of the runtime module should be interchangable (same full set of
; interrupts).
;
; The file number parameter was added to add granularity to non /o programs.
; Entry points are grouped together with those that are likely to be
; used along with each other.
; This grouping is approximate and may be changed at any time.
;
; The following system is now being used:
;	File 1:	Those functions required for a minimal "END" program
;	File 2:	B$SRUN.  This is the only function that uses __doexec,
;		which must be kept in the RTM LIBRARY.  __doexec is only
;		linked in if a program contains a RUN statement.
;	File 3:	"Common" entry points
;	File 4:	"Non-common" floating point entry points
;	File 5:	"graphics" entry points
;	File 6:	everything else (includes unused C startup entries)
;
;
;Summary of fields:
;
;	RTMENT	entryname,file,entryseg,startlabel,callflag,switches
;
;	    entryname - Name of routine
;	    file      - which file (RTMINT<file>.ASM) has the definition
;	    entryseg  - segment of <entryname> if not ???
;	    startlabel- Alternate name for routine
;	    callflag  - Blank: Function will ONLY be CALLed FAR.
;			'0'    Function can be JMP'ed to.  Will return.
;			'1'    Function can be JMP'ed to.  Will not return.
;	    switches  - set of MASM switches.  If defined and FALSE, there
;			will be no support for this routine, though the
;			postbyte will still be allocated.
;
;	NOTE: This is a SHARED INCLUDE FILE, used by both the RUNTIME and
;	NOTE: the interpreter projects.  Any changes made to one should
;	NOTE: be carried over (COPIED) to the other!!!
;
;******************************************************************************
;
; Entry points referenced by intepreter executors. (Must all be single post-
; bytes).
;


	RTMENT	B$ASSN,3
	RTMENT	B$BEEP,6
	RTMENT	B$BLOD,6
	RTMENT	B$BSAV,6
	RTMENT	B$CASP,5
	RTMENT	B$CDIR,6
	RTMENT	B$CHAN,3
	RTMENT	B$CHOU,3
	RTMENT	B$CIRC,5
	RTMENT	B$CLOS,3
	RTMENT	B$COLR,5
	RTMENT	B$CSCN,5
	RTMENT	B$CSRL,6
	RTMENT	B$CSTO,5
	RTMENT	B$CSTT,5
	RTMENT	B$DDIM,3
	RTMENT	B$DRAW,5
	RTMENT	B$DSEG,6
	RTMENT	B$DSG0,6
	RTMENT	B$DSKI,3
	RTMENT	B$DVI4,3
	RTMENT	B$DWID,6
	RTMENT	B$ENRD,3	
	RTMENT	B$ENSD,3	
	RTMENT	B$EPE0,6
	RTMENT	B$EPE1,6
	RTMENT	B$EPE2,6
	RTMENT	B$ERAS,6
	RTMENT	B$ERDS,6
	RTMENT	B$ERDV,6
	RTMENT	B$ESG0,6
	RTMENT	B$ESG1,6
	RTMENT	B$ESG2,6
	RTMENT	B$ETC0,6
	RTMENT	B$ETC1,6
	RTMENT	B$ETC2,6
	RTMENT	B$ETK0,6
	RTMENT	B$ETK1,6
	RTMENT	B$ETK2,6
	RTMENT	B$ETL0,6
	RTMENT	B$ETL1,6
	RTMENT	B$ETL2,6
	RTMENT	B$ETS0,6
	RTMENT	B$ETS1,6
	RTMENT	B$ETS2,6
	RTMENT	B$ETT0,6
	RTMENT	B$ETT1,6
	RTMENT	B$ETT2,6
	RTMENT	B$FASC,6
	RTMENT	B$FATR,6
	RTMENT	B$FCHR,3
	RTMENT	B$FCMD,3
	RTMENT	B$FCVD,3
	RTMENT	B$FCVI,3
	RTMENT	B$FCVL,3
	RTMENT	B$FCVS,3
	RTMENT	B$FDAT,3
	RTMENT	B$FEOF,3
	RTMENT	B$FEVI,6
	RTMENT	B$FEVS,6
	RTMENT	B$FHEX,6
	RTMENT	B$FICT,6
	RTMENT	B$FIEL,6
	RTMENT	B$FILS,6
	RTMENT	B$FINP,3
	RTMENT	B$FLDP,3
	RTMENT	B$FLEN,3
	RTMENT	B$FLOC,6
	RTMENT	B$FLOF,6
	RTMENT	B$FMDF,3
	RTMENT	B$FMID,3
	RTMENT	B$FMKD,3
	RTMENT	B$FMKI,3
	RTMENT	B$FMKL,3
	RTMENT	B$FMKS,3
	RTMENT	B$FMSF,3
	RTMENT	B$FOCT,6
	RTMENT	B$FPEN,6
	RTMENT	B$FPLY,3
	RTMENT	B$FPOS,6
	RTMENT	B$FREF,3
	RTMENT	B$FRI2,3
	RTMENT	B$FRSD,3
	RTMENT	B$FSCN,3
	RTMENT	B$FSEK,6	
	RTMENT	B$FSHL,6
	RTMENT	B$FSPC,3
	RTMENT	B$FSTG,3
	RTMENT	B$FTAB,3
	RTMENT	B$FTIM,3
	RTMENT	B$FVAL,3
	RTMENT	B$FWID,3
	RTMENT	B$GET1,6
	RTMENT	B$GET2,6
	RTMENT	B$GET3,6
	RTMENT	B$GET4,6
	RTMENT	B$GGET,5
	RTMENT	B$GPUT,5
	RTMENT	B$INKY,3
	RTMENT	B$INPP,3
	RTMENT	B$INS2,3
	RTMENT	B$INS3,3
	RTMENT	B$KFUN,3
	RTMENT	B$KILL,6
	RTMENT	B$KMAP,3
	RTMENT	B$LBND,3
	RTMENT	B$LCAS,3
	RTMENT	B$LDFS,3
	RTMENT	B$LEFT,3
	RTMENT	B$LINE,5
	RTMENT	B$LNIN,3
	RTMENT	B$LOCK,6
	RTMENT	B$LOCT,3
	RTMENT	B$LPOS,3
	RTMENT	B$LPRT,3
	RTMENT	B$LSET,3
	RTMENT	B$LTRM,3
	RTMENT	B$LWID,3
	RTMENT	B$MCVD,3
	RTMENT	B$MCVS,3
	RTMENT	B$MDIR,6
	RTMENT	B$MUI4,3
	RTMENT	B$N1I2,5
	RTMENT	B$N1R4,5
	RTMENT	B$N2I2,5
	RTMENT	B$N2R4,5
	RTMENT	B$NAME,6
	RTMENT	B$ONCA,6
	RTMENT	B$ONKA,6
	RTMENT	B$ONLA,6
	RTMENT	B$ONPA,6
	RTMENT	B$ONSA,6
	RTMENT	B$ONSG,6
	RTMENT	B$ONTA,6
	RTMENT	B$OOPN,3
	RTMENT	B$OPEN,3
	RTMENT	B$PAIN,5
	RTMENT	B$PAL0,5
	RTMENT	B$PAL2,5
	RTMENT	B$PALU,5
	RTMENT	B$PCI2,3
	RTMENT	B$PCI4,3
	RTMENT	B$PCPY,5
	RTMENT	B$PCR4,3
	RTMENT	B$PCR8,3
	RTMENT	B$PCSD,3
	RTMENT	B$PEEK,6
	RTMENT	B$PEI2,3
	RTMENT	B$PEI4,3
	RTMENT	B$PEOS,3
	RTMENT	B$PER4,3
	RTMENT	B$PER8,3
	RTMENT	B$PESD,3
	RTMENT	B$PINP,6	; INP and OUT are only needed for the
	RTMENT	B$POUT,6	; Interpreters
	RTMENT	B$PMAP,5
	RTMENT	B$PNI2,5
	RTMENT	B$PNR4,5
	RTMENT	B$PNT1,5
	RTMENT	B$PNTC,5
	RTMENT	B$POKE,6
	RTMENT	B$PRST,5
	RTMENT	B$PSET,5
	RTMENT	B$PSI2,3
	RTMENT	B$PSI4,3
	RTMENT	B$PSR4,3
	RTMENT	B$PSR8,3
	RTMENT	B$PSSD,3
	RTMENT	B$PSTC,5
	RTMENT	B$PUT1,6
	RTMENT	B$PUT2,6
	RTMENT	B$PUT3,6
	RTMENT	B$PUT4,6
	RTMENT	B$RDI2,3
	RTMENT	B$RDI4,3
	RTMENT	B$RDIM,6
	RTMENT	B$RDIR,6
	RTMENT	B$RDR4,3
	RTMENT	B$RDR8,3
	RTMENT	B$RDSD,3
	RTMENT	B$REST,3
	RTMENT	B$RGHT,3
	RTMENT	B$RMI4,3
	RTMENT	B$RND0,6
	RTMENT	B$RND1,6
	RTMENT	B$RNZ0,6
	RTMENT	B$RNZP,6
	RTMENT	B$RSET,3
	RTMENT	B$RTRM,3
	RTMENT	B$S1I2,5
	RTMENT	B$S1R4,5
	RTMENT	B$S2I2,5
	RTMENT	B$S2R4,5
	RTMENT	B$SADD,3
	RTMENT	B$SASS,3
	RTMENT	B$SCAT,3
	RTMENT	B$SCLS,3
	RTMENT	B$SCPY,3
	RTMENT	B$SDAT,3
	RTMENT	B$SENV,6
	RTMENT	B$SERR,6
	RTMENT	B$SETM,6
	RTMENT	B$SICT,6
	RTMENT	B$SMID,3
	RTMENT	B$SOND,3
	RTMENT	B$SPAC,3
	RTMENT	B$SPLY,3
	RTMENT	B$SSEK,6	
	RTMENT	B$SSHL,6
	RTMENT	B$STDL,3 	
	RTMENT	B$STI2,3
	RTMENT	B$STI4,3
	RTMENT	B$STIK,6
	RTMENT	B$STIM,6
	RTMENT	B$STR4,3
	RTMENT	B$STR8,3
	RTMENT	B$STRI,3
	RTMENT	B$STRS,3
	RTMENT	B$SWP2,3
	RTMENT	B$SWP4,3
	RTMENT	B$SWP8,3
	RTMENT	B$SWPN,3
	RTMENT	B$TIMR,3
	RTMENT	B$UBND,3
	RTMENT	B$UCAS,3
	RTMENT	B$USNG,3
	RTMENT	B$VARP,3
	RTMENT	B$VEW0,5
	RTMENT	B$VIEW,5
	RTMENT	B$VWPT,5
	RTMENT	B$WAIT,3	; WAIT is only needed by the Interpreter
	RTMENT	B$WIDT,3
	RTMENT	B$WIN0,5
	RTMENT	B$WIND,5
	RTMENT	B$WRIT,3
;
; Entry points not referenced by interpreter executors. (May be multiple post-
; bytes). These MAY be referenced by the interpeter, but not in a manner that
; requires a single post-byte.
;
	RTMENT	B$?EVT,3
	RTMENT	B$CEND,1
	RTMENT	B$CENP,1
	RTMENT	B$CPI4,3
	RTMENT	B$ENFA,3
	RTMENT	B$ENRA,3
	RTMENT	B$ENSA,3 	
	RTMENT	B$EVCK,3
	RTMENT	B$EXFA,3
	RTMENT	B$EXSA,3
	RTMENT	B$FBIN,3
	RTMENT	B$FERL,3
	RTMENT	B$FERR,3
	RTMENT	B$GOSA,3
	RTMENT	B$HARY,3
	RTMENT	B$LINA,3
	RTMENT	B$OEGA,3
	RTMENT	B$OGSA,3
	RTMENT	B$OGTA,3
	RTMENT	B$RELA,6
	RTMENT	B$RES0,6
	RTMENT	B$RESA,6
	RTMENT	B$RESN,6
	RTMENT	B$RETA,3
	RTMENT	B$RSTA,3
	RTMENT	B$RSTB,3
	RTMENT	B$RUNL,6 	
	RTMENT	B$SCHN,3
	RTMENT	B$SCLR,3
	RTMENT	B$SCMP,3
	RTMENT	B$SCPF,3 	
	RTMENT	B$SRUN,2	; MUST have its own file
	RTMENT	B$STOP,3
	RTMENT	B$SWSD,3 	
	RTMENT	B$CMI4,3	
	RTMENT	B$TROF,6
	RTMENT	B$TRON,6
	RTMENT	B$Undef1,6,,,,FALSE ;These four entrypoints were used in QB4
	RTMENT	B$Undef2,6,,,,FALSE ;but are no longer used (they were for
	RTMENT	B$Undef3,6,,,,FALSE ;ISAM support).  They may be used for any
	RTMENT	B$Undef4,6,,,,FALSE ;Product, but the slots must exist.


;
; The following entries must be grouped together. They are all in segment _TEXT
;
	RTMENT	B$ATN4,4,_TEXT,_TEXT_START	
	RTMENT	B$ATN8,4,_TEXT
	RTMENT	B$COS4,4,_TEXT
	RTMENT	B$COS8,4,_TEXT
	RTMENT	B$EXP4,4,_TEXT
	RTMENT	B$EXP8,4,_TEXT
	RTMENT	B$FCMP,3,_TEXT		; floating point compare
	RTMENT	B$FIL2,4,_TEXT
	RTMENT	B$FILD,4,_TEXT
	RTMENT	B$FIS2,4,_TEXT
	RTMENT	B$FIST,4,_TEXT
	RTMENT	B$FIX4,4,_TEXT
	RTMENT	B$FIX8,4,_TEXT
	RTMENT	B$FUST,4,_TEXT
	RTMENT	B$INT4,4,_TEXT
	RTMENT	B$INT8,4,_TEXT
	RTMENT	B$LOG4,4,_TEXT
	RTMENT	B$LOG8,4,_TEXT
	RTMENT	B$POW4,4,_TEXT
	RTMENT	B$POW8,4,_TEXT
	RTMENT	B$SGN4,4,_TEXT
	RTMENT	B$SGN8,4,_TEXT
	RTMENT	B$SIN4,4,_TEXT
	RTMENT	B$SIN8,4,_TEXT
	RTMENT	B$TAN4,4,_TEXT
	RTMENT	B$TAN8,4,_TEXT

	; Definition of 5th parameter:
	;	Blank:	function will ONLY be CALLed FAR.
	;	'0'	Function can be JMP'ed to directly/indirectly.  Macro
	;		will generate a far call to the INT 3F, followed by a
	;		RETF.
	;	'1'	Function can be JMP'ed to directly/indirectly.  Macro
	;		will generate a far call to the INT 3F, but will not
	;		follow it with a RETF, since routine does not return.


					; C startup functions in the RTM
					; and in QB.EXE
	RTMENT	__fpreset,1,_TEXT	;math pack reset
	RTMENT	__chkstk,1,_TEXT	; publics in CHKSTK
	RTMENT	__nullcheck,1,_TEXT	; publics in CHKSUM
	RTMENT	__fptrap,1,_TEXT,,1	; publics in CRT0FP
	RTMENT	__FMSG_TEXT,1,_TEXT,,0	; publics in FMSGHDR
	RTMENT	__amsg_exit,1,_TEXT,,1	; publics in CRT0
	RTMENT	__cintDIV,1,_TEXT,,1	


;
; NEW functions added after QB4 shipped.  Moved here with revision [27]
; to ensure that the QB shipped with BASCOM 6 remains compatable with the
; QB4.00 already shipped.
;
; They can not be moved into RTM_UNDEFINED slots, as that would break
; break backward compatibility with QB4.00B.
;
	RTMENT	B$EUE0,6,,_TEXT_END	; label to mark end of _TEXT entries
	RTMENT	B$EUE1,6		
	RTMENT	B$EUE2,6		
	RTMENT	B$ONUE,6		
	RTMENT	B$SLEP,6		
