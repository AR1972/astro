;***
;pointers.inc - 30-Dec-87 - pointer reference macros
;***
	IFNDEF	LI_EXPAND
	.XLIST
	ENDIF
;***
;
;	Copyright <C> 1987, Microsoft Corporation
;
;Purpose:
;	To provide a set of macros for pointer reference whether they are
;	NEAR (1-word) or FAR (2-word).	FAR pointers may also have an SB
;	index instead of a true physical segment.
;
;	NOTE: This is a SHARED INCLUDE FILE, used by both the RUNTIME and
;	NOTE: the interpreter projects.  Any changes made to one should
;	NOTE: be carried over (COPIED) to the other!!!
;
;
;******************************************************************************

POINTERS_INC = -1   ;needed by QBI sources which also include this file.

;FV_SBPTR determines the type of 2-word pointers used in manipulating
;Far Heap and some Near Heap accesses.	When OFF, these pointers contain
;an actual physical segment.  When ON, they contain an SB (Segment Base)
;which is an index to a table containing the actual physical segment.
;The following MACRO's attempt to hide these differences.

;FV_SBSWAP is ON if an SB can be swapped out of active memory.  (Swapped
;to disk or out of active EMM.)

;Added with [11]
;***
;GetCodeIntoDs
;
;Purpose:
;	Loads DS with the segment of the executors.
;Input:
;	Optional - name of current segment.  Causes a call to TxtSegCur 
;	if OE_WIN.
;Output:
;	No registers affected (except DS), although memory movement may occur if 
;	OE_WIN, invalidating ES.  ES will be restored to the text segment 
;	if name of current segment is given.
;	Flags destroyed if OE_WIN.

sBegin	DATA
	extrn	SegCode:word
sEnd	DATA

GetCodeIntoDs	MACRO	parm
	mov	ds,[SegCode]
	ENDM
;End of [11]

;***
;GETSB_SEG - macro to get an SB's physical segment
;
;Purpose:
;	Added with [2].
;	This macro allows us to access the physical segment of an SB.
;Entry:
;	dest -	register in which the SB physical segment is to be placed
;	srce -	the SB index
;	temp -	a temporary register is required and should be used only when:
;		    SPEED, NOLOAD, (NOT FIXED) temp must be index reg
;		    SPEED, LOAD,   (NOT FIXED) temp must be index reg != dest
;		    SPEED, LOAD,   FIXED
;		    SIZE,  NOLOAD, FIXED
;		    SIZE,  LOAD,   FIXED
;	opt  -	any combination of the following options:
;		    SIZE to optimize macro code for size
;		    SPEED to optimize macro code for speed (default)
;		    LOAD tests for and reloads non-resident blocks
;		    NOLOAD no test or reload (default)
;		    FIXED if 'srce' is a fixed SB constant
;			(FIXED is also implied if srce is a constant)
;		    NOFLUSH to prohibit the flushing of all EMM blocks in
;			non-release versions
;		    FLUSH to cause EMM flushing (default)
;
;Exit:
;	register 'dest' contains the SB physical segment of 'srce'.
;Notes:
;	ES and DS are always preserved (when != dest) for SPEED and NOLOAD.
;	ES and DS are preserved (when != dest) for SIZE or LOAD if:
;	    -- it is psCur, or
;	    -- it is locked, or
;	    -- it is not-EMM and (non-swappable OR non-discardable)
;	When a desired option is important it should be specified rather
;	    than defaulted so that the defaults may be changed.
;Examples:
;	GETSB_SEG   es,[grs.GRS_sbVar],,<SIZE,LOAD>
;	GETSB_SEG   ds,bx,bx,<SPEED,NOLOAD>
;
;**************************************************************************
GETSB_SEG   MACRO   dest,srce,temp,opt
	LOCAL	NoReload
	?sz = 0 			;;default SPEED (not SIZE)
	?ld = 0 			;;default NOLOAD (not LOAD)
	?fx = 0 			;;default not FIXED
	?fl = 1 			;;default FLUSH

	irp x,<opt>
	  ifidni <x>,<SIZE>		;;if SIZE,
	    ?sz=1			;;  set size flag
	  elseifidni <x>,<SPEED>	;;if SPEED,
	    ?sz=0			;;  clr size flag
	  elseifidni <x>,<LOAD> 	;;if LOAD,
	    ?ld=1			;;  set load flag
	  elseifidni <x>,<NOLOAD>	;;if NOLOAD
	    ?ld=0			;;  clr load flag
	  elseifidni <x>,<FIXED>	;;if FIXED
	    ?fx=1			;;  set fixed flag
	  elseifidni <x>,<FLUSH>	;;if FLUSH
	    ?fl=1			;;set flush flag
	  elseifidni <x>,<NOFLUSH>	;;if NOFLUSH
	    ?fl=0			;;clr flush flag
	  else
	    if1
	      %out invalid GETSB_SEG option: &x
	    endif
	    .err ;invalid GETSB_SEG option
	  endif
	endm

	?sz = 0 			;;  always SPEED
	?ld = 0 			;;  and NOLOAD
	?fl = 0 			;;and NOFLUSH

	?fl = 0 			;;NOFLUSH in RELEASE versions

if (.type srce) and 04H 		;;if srce is a constant
	?fx = 1 			;;  must be a FIXED SB
endif

if ?sz AND ?fx AND NOT ?ld		;;SIZE & FIXED & !LOAD is actually
	?sz = 0 			;;  smaller as !SIZE
endif

if ?sz					;;SIZE
  if ?fx				;;  FIXED
	mov	temp,srce		;;    can't push constant
	push	temp
  else					;;  not FIXED
	push	srce			;;    sb arg to call on stack
  endif

  if ?fl				;;if EMM flushing desired
      % extrn	Flush_&&?segname:near	;;then do it
      % call	Flush_&&?segname	;
  endif 				;
      % extrn	DerefSb_&&?segname:near ;
      % call	DerefSb_&&?segname	;;top of stack = phys seg
	pop	dest			;;dest = phys seg
else					;;SPEED version
  if ?fx				;;  FIXED

    if ?fl				;;if EMM flushing desired
      % extrn	Flush_&&?segname:near	;;then do it
      % call	Flush_&&?segname	;
    endif				;

	mov	dest,mpsbps[srce*2]	;;    fetch seg direct from sb table
  else					;;  not FIXED
    ifdifi  <temp>,<srce>		;;    if temp != srce
	mov	temp,srce		;;	temp = srce  (SB)
    endif

    if ?fl				;;if EMM flushing desired
      % extrn	Flush_&&?segname:near	;;then do it
      % call	Flush_&&?segname	;
    endif				;

	shl	temp,1			;;    shift to make it a table index
	mov	dest,mpsbps[temp]	;;    fetch seg from sb table
  endif
  if ?ld				;;  LOAD
    ifidni  <dest>,<ds> 		;;if dest == ds
	?over	EQU <BYTE PTR SS>	;;  force SS override to DGROUP
    else				;;else
	?over	EQU <BYTE PTR DGROUP>	;;  insure DGROUP
    endif				;
    if ?fx				;
	test	?over:mpsbps[srce*2],1	;;test for resident segment
    else				;
	test	?over:mpsbps[temp],1	;;test for resident segment
      ifidni <dest>,<temp>		;;temp must be != dest here
	if1				;
	  %out temp == dest not allowed here;
	endif				;
	.err ;temp == dest not allowed here   [10]
      endif				;
    endif				;
	jnz	NoReload		;;skip reload if resident
    if ?fx
	mov	temp,srce		;
    else				;
	shr	temp,1			;;restore SB index
    endif
	push	temp			;
      % extrn	DerefSb_&&?segname:near ;
      % call	DerefSb_&&?segname	;;top of stack = phys seg
	pop	dest			;;dest = phys seg

NoReload:				;
  endif   ;;LOAD
endif	;;SIZE/SPEED
	ENDM


DGROUPSEG   EQU <SS>	;DGROUP is in SS


;***
;GETSEG - macro to access the physical segment of a far heap descriptor.
;
;Purpose:
;	Rewritten with [2].
;	This macro allows us to access the segment of a far heap descriptor
;	in different ways, depending on our far heap code support scheme.
;Entry:
;	see GETSB_SEG.
;Exit:
;	register 'dest' contains the physical segment address.
;
;**************************************************************************
GETSEG	MACRO	dest,srce,temp,opt

    ifdifi <dest>,<srce>
	mov	dest,srce		;;fetch seg address directly
endif
	ENDM

;***
;GETPTR - macro for FAR/HUGE pointer reference
;
;Purpose:
;	Rewritten with [2].
;
;	This macro allows us to use a pointer whether it be FAR (with actual
;	physical segment) or HUGE (with an SB segment).
;	HUGE pointers are used when FV_SBPTR is on.
;
;	The result will always be a FAR pointer (with actual physical segment)
;	in a register pair.
;
;Entry:
;	alias - the name to use for subsequent pointer reference.
;	seg   - register to receive the physical segment.
;		seg must be a segment register to use alias.
;	reg   - register to receive the address offset of the pointer.
;		reg must be an index register to use alias.
;	srce  - source of the pointer.  Must be defined with parmDP or localDP.
;	temp  - the temporary register to use for dereferencing the SB.
;		If not provided, reg will be used (see GETSEG).
;	opt   - see GETSB_SEG.
;Exit:
;	seg:[reg] - contains the physical pointer
;	alias	  - is a name (equated to seg:[reg]) to use when referencing
;		    the pointer, so long as seg and reg remain undisturbed.
;**************************************************************************
GETPTR	MACRO alias,seg,reg,srce,temp,opt
ifnb	<alias>
alias	equ	seg:[reg]		;;  set up alias
endif
  ifidni <seg>,<es>
	les	reg,srce		;;  optimize segreg ES load wo/SB
  elseifidni <seg>,<ds>
	lds	reg,srce		;;  optimize segreg DS load wo/SB
  else
	mov	seg,SEG_&srce		;;  load seg (not a segreg)
	mov	reg,OFF_&srce		;;  and get offset
  endif
	ENDM
