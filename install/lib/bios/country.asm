; ========================================================

COMMENT #

	COUNTRY.ASM

	Copyright (c) 1991 - Microsoft Corp.
	All rights reserved.
	Microsoft Confidential


	Fills in a country information structure

	GetCountryInfo( struct COUNTRY_INFO *Cntry );

	ARGUMENTS: Cntry - Ptr to 34 byte buffer
	RETURNS:   TRUE if successful, FALSE if not

	johnhe - 02/24/90

END COMMENT #

;========================================================

include BIOS_IO.INC
include	MODEL.INC


; ========================================================

.CODE

; ========================================================

IF @DataSize
 GetCountryInfo PROC USES DS, Buffer:PTR
ELSE	
 GetCountryInfo PROC Buffer:PTR
ENDIF
	mov	AX,3800h		; Get country info function
	LoadPtr	DS,DX,Buffer		; Address of buffer
	int	21h
        mov     ax, 01h
        jnc     gci1
        xor     ax, ax                  ; Error, so return FALSE
    gci1:
	ret

GetCountryInfo ENDP

; ========================================================

	END

; ========================================================
