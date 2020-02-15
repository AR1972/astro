
; ----------------------------------------------------------

        PUBLIC  $M_MSGSERV_2
        IF1
        %out    ... Including message Class 2
        ENDIF

; ----------------------------------------------------------

$M_CLASS_2_STRUC LABEL BYTE
        $M_CLASS_ID <002H,EXPECTED_VERSION,Class_2_MessageCount>

; ----------------------------------------------------------


$M_2_00003H_STRUC LABEL BYTE
        $M_ID   <00003H,$M_2_00003H_MSG-$M_2_00003H_STRUC>

$M_2_00006H_STRUC LABEL BYTE
        $M_ID   <00006H,$M_2_00006H_MSG-$M_2_00006H_STRUC>

$M_2_00009H_STRUC LABEL BYTE
        $M_ID   <00009H,$M_2_00009H_MSG-$M_2_00009H_STRUC>

; ----------------------------------------------------------

$M_2_FF_STRUC LABEL BYTE
        $M_ID <0FFFFH,$M_2_FF_MSG-$M_2_FF_STRUC>

; ----------------------------------------------------------


$M_2_00003H_MSG LABEL BYTE
        DB      $M_2_00003H_END-$M_2_00003H_MSG-1
        DB      "Invalid switch"
$M_2_00003H_END LABEL BYTE
  
$M_2_00006H_MSG LABEL BYTE
        DB      $M_2_00006H_END-$M_2_00006H_MSG-1
        DB      "Parameter value not in allowed range"
$M_2_00006H_END LABEL BYTE
  
$M_2_00009H_MSG LABEL BYTE
        DB      $M_2_00009H_END-$M_2_00009H_MSG-1
        DB      "Parameter format not correct"
$M_2_00009H_END LABEL BYTE
  
; ----------------------------------------------------------

$M_2_FF_MSG LABEL BYTE
        DB      $M_2_FF_END-$M_2_FF_MSG-1
        DB      "Parse Error %1"
$M_2_FF_END LABEL BYTE
  
; ----------------------------------------------------------

Class_2_MessageCount EQU     4

; ----------------------------------------------------------

        IF      FARmsg
$M_MSGSERV_2 PROC FAR
        ELSE
$M_MSGSERV_2 PROC NEAR
        ENDIF

        PUSH    CS
        POP     ES
        LEA     DI,$M_CLASS_2_STRUC
        ADD     CX,$-$M_CLASS_2_STRUC
        RET

$M_MSGSERV_2 Endp

; ----------------------------------------------------------

