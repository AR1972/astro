;***
; RMACROS.INC - June 3, 1986 - Common Runtime Common Macros
;***
	.XLIST
;***
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; Contains macros and other definitions which are used prior to including
; other include files to control their definitions.
;
;Notes:
; This include file may create symbols which are referenced by other include
; files. All of these symbols begin with _?rt_.
;
;******************************************************************************

;***
;
;	useSeg
;	declares that a module uses a particular segment. This then
;	turns on the segment declaration for the segment in SEG.INC,
;	which is included after this file.
;
;***
useSeg	MACRO	sg
	IRP	s,<sg>		;;For each segment specified
_?rt_seg_&&s	EQU	TRUE	;;Tell SEG.INC to define it
	.XCREF	_?rt_seg_&&s	;;Keep the symbol table dump clean
	ENDM			;;IRP s
	ENDM			;;useSeg MACRO

;***
;
;	pubmac
;	used in place of the PUBLIC psuedo op, it defines the item PUBLIC,
;	and also defines a unique symbol to indicate that the item is defined
;	here.
;
;	This is a kludge to get the runtime interface mapper up and running.
;	The intent is to remove this kludge eventually.
;
;***
pub_dummy MACRO list		;;Used because PUBMAC could get a list
	IRP	p,<list>	;;For each name in list
	IFIDN	<p>,<>		;;If we're done
	EXITM			;;Exit the macro
	ENDIF
	PUBLIC	p		;;Else perform original function
p&&_DEFINED_HERE = 1		;; and set the defined flag
	ENDM
	ENDM

pubmac	MACRO	a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z
	pub_dummy <a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z>
	ENDM

	.LIST
