;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
.xlist
include version.inc
include cmacros.inc
.list

sBegin  code
assumes cs,code

;
; c = IToupper (c, routine);
;
;       c is char to be converted
;       routine is case map call in international table
;

cProc   IToupper,<PUBLIC>
parmW   c
parmD   routine
cBegin
        mov     ax,c
        or      ah,ah
        jnz     donothing
        cmp     al,'a'
        jb      noconv
        cmp     al,'z'
        ja      noconv
        sub     al,20H
noconv:
        call    routine
donothing:
cEnd


sEnd

end
