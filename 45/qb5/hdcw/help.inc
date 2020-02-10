; 
;* help.h
;*
;* typedefs & definitions used in the help system and by those who use it.
;*
;* define:
;*  HOFFSET	- to define buffer pointers (PB's) as handle/offset, else
;*		  they are defined as void far *.
; 
; 
;* lineattr
;* external representation of line attributes, as returned by HelpGetLineAttr
; 
lineattr struc
    attrLA          dw  0
    cbLA            dw  0
lineattr ends
; 
;* mh
;* a memory handle is defined for use with systems that use dynamic, moveable
;* memory. It is long, so that in simple cases where memory is NOT moveable,
;* the handle can contain the far pointer to the base.
; 
; 
;* nc
;* a context number is a unique id associated with each context string.
; 
; 
;* topichdr
;* header placed (by HelpDecomp) at the begining of every decompressed topic
; 
topichdr struc
    appCharTH       db  0
    linCharTH       db  0
    ftypeTH         db  0
    lnCurTH         dw  0
    lnOffTH         dw  0
topichdr ends
; 
;* hotspot
;* defines the position of an embedded cross reference, or "hotspot". Used by
;* HelpHlNext and HelpXRef
; 
hotspot struc
    lineHS          dw  0
    colHS           dw  0
    ecolHS          dw  0
    pXrefHS         dd  0
hotspot ends
;******************************************************************************
;*
;* Some versions of the help engine run with SS!=DS, and thus require the
;* _loadds attribute on function calls.
; 
ifdef DSLOAD
else
LOADDS              equ 1
endif
;******************************************************************************
;*
;* PB
;* pointer to a buffer. Based on the switch HOFFSET, it is either a
;* handle-offset or a far pointer. In the handle/offset case, the high word
;* contains a memory handle which must be locked, to get a "real" address, to
;* which the offset is added.
; 
ifdef HOFFSET
else
endif
;******************************************************************************
;*
;* Forward declarations
; 
;******************************************************************************
;*
;* constant declarations
;*
;* Character attribute bits. These bits are order together to form attribute
;* indecies. Data in the help file has associated with it attribute information
;* encoded in length/index pairs. Each index is simply a constant which
;* indicates which of several attributes should be applied to the characters in
;* that portion of the line.
; 
A_PLAIN             equ 0
A_BOLD              equ 1
A_ITALICS           equ 2
A_UNDERLINE         equ 4
;******************************************************************************
;*
;* Help Error Codes.
;*
;* Return values greater than HELPERR_MAX are valid nc's.
; 
HELPERR_FNF         equ 1
HELPERR_READ        equ 2
HELPERR_LIMIT       equ 3
HELPERR_BADAPPEND   equ 4
HELPERR_NOTHELP     equ 5
HELPERR_BADVERS     equ 6
HELPERR_MAX         equ 10
