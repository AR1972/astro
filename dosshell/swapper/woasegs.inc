;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1985-1991
; *                      All Rights Reserved.
; */


;----------------------------------------------------------------------------;
; defines the exact segment ordering that we want to have. It is very very   ;
; important that the StubSegment occurs at the end.			     ;
;----------------------------------------------------------------------------;

 createSeg _DATA,Data,word,public,DATA,DGROUP
 defgrp DGROUP,Data
 createSeg _TEXT,Code,word,public,Code
 createSeg _WOARLMSEG,StubSeg,word,public,Code

;----------------------------------------------------------------------------;
