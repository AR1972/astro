; Default command line for BASIC:    MASM /Dmodel=medium /Dlang=BASIC power2;
; Default command line for C:        MASM /MX /Dmodel=small /Dlang=C power2;
; Default command line for FORTRAN:  MASM /Dmodel=large /Dlang=FORTRAN power2;
; Default command line for Pascal:   MASM /Dmodel=large /Dlang=Pascal power2;

%         .MODEL  model,lang
          INCLUDE mixed.inc

%         IFIDNI  <lang>,<BASIC>
reference EQU     1
%         ELSEIFIDNI <lang>,<FORTRAN>
reference EQU     1
          ENDIF

          .CODE

; Function for C, FORTRAN, Pascal, Version 4 of QuickBASIC, and
;   future versions of Microsoft and IBM BASIC Compilers

          IFDEF   reference          ; Pass by reference for BASIC or FORTRAN
Power2    PROC    Value:PTR WORD, Count:PTR WORD

          pLes    bx,Value           ; Load arguments passed by reference
          mov     ax,FP[bx]
          pLes    bx,Count
          mov     cx,FP[bx]

          ELSE                       ; Pass by value for C or Pascal
Power2    PROC    Value, Count

          mov     ax,Value           ; Load arguments passed by value
          mov     cx,Count
          ENDIF

          shl     ax,cl              ; AX = AX * (2 to power of CL)
                                     ; Return result in AX
          ret
Power2    ENDP

          IFIDNI  <lang>,<BASIC>

; Subprogram for QuickBASIC, Versions 1, 2, and 3;
;     for the Microsoft BASIC Compiler through Version 5.36
;     for the IBM BASIC Compiler through Version 2.02

Power2S   PROC    Value:PTR WORD, Count:PTR WORD, RetVal:PTR WORD

          pLes    bx,Value           ; Load BASIC arguments
          mov     ax,FP[bx]          ;   passed by reference
          pLes    bx,Count
          mov     cx,FP[bx]

          shl     ax,cl              ; AX = AX * (2 to power of CL)

          pLes    bx,RetVal          ; Load return address
          mov     FP[bx],ax          ;   and store result in it

          ret
Power2S   ENDP
          ENDIF   ; BASIC
          END

