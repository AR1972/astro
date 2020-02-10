;***
; array.inc - 12-Nov-86 - far memory and array descriptors
;
;	Copyright <C> 1986, Microsoft Corporation
;
;Purpose:
; Contains the definition of the array and far heap descriptors.
; 
; NOTE: If you modify this file, notify the QBI group, as they include it too.
;
;Array descriptor fields:
;
; AD_oAdjusted: This field contains the constant portion of the array indexing
;   expression, which takes into account the cumulative affect of all the lower
;   bounds for the array dimensions. Given:
;
;	DIM ARY(l1 to u1, l2 to u2, l3 to u3)
;		  \  /	    \  /      \  /
;		   \/	     \/        \/
;		   c1	     c2        c3	(count of elements)
;
;   The address of any array element can be calculated as:
;
;	(((k - l3) * c2 + j - l2) * c1 + i - l1) * elementsize + offset
;
;   This can be rearranged to isolate the items dealing with the lower bounds:
;
;	(((k * c2 + j) * c1) + i) * elementsize
;	    - (l3 * c2 * c1 + l2 * c1 + l1) * elementsize
;	    + offset
;
;   The second and third lines of this equation are known at DIM time, and are
;   placed into the AD_oAdjusted field. To access an array element then, the
;   calculation required is:
;
;	(((k * c2 + j) * c1) + i) * elementsize + AD_oAdjusted
;
;	NOTE: This is a SHARED INCLUDE FILE, used by both the RUNTIME and
;	NOTE: the interpreter projects.  Any changes made to one should
;	NOTE: be carried over (COPIED) to the other!!!
;
;******************************************************************************

ARRAY_INC = -1	;needed by QBI sources which also include this file.

AHD	STRUC			;[6]Array header - in front of data in far space
AHD	ENDS			;[6]

;		FHD Structure - Far heap descriptor

FHD		STRUC
FHD_oData	DW	?	;[1]Offset of data start
FHD_hData	DW	?	;[1]Segment of data start
FHD_pNext	DW	?	;DGROUP offset of next FHD
FHD_cPara	DW	?	;Para count of entry (bytes for non-LNA)
FHD		ENDS

;		DM Structure - Array dimension information

DM		STRUC
DM_cElements	DW	?	;Element count for this dimension
DM_iLbound	DW	?	;Lower bound for this dimension
DM		ENDS

;		AD Structure - Array descriptor

AD		STRUC
AD_fhd		DB	SIZE FHD DUP(?) ;FHD for this AD

AD_cDims	DB	?	;Dimension count
AD_fFeatures	DB	?	;Feature flags

AD_oAdjusted	DW	?	;[3] Adjusted offset
AD_cbElement	DW	?	;Byte count per element

AD_tDM		DB	?	;First byte of first DM structure
AD		ENDS

;		AD_fFeatures flag definitions

FADF_NEAR	EQU	0	;[1]near allocation
FADF_FAR	EQU	1	;[1]far allocation
FADF_HUGE	EQU	2	;[1]huge allocation
FADF_ALCMSK	EQU	3	;[1]allocation mask
FADF_STATIC	EQU	40h	;[1]Array is a $STATIC array
FADF_SD 	EQU	80h	;[1]Array is a string array
