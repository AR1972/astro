;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1981-1991
; *                      All Rights Reserved.
; */
	page	,160

MULTI_CONFIG equ 1

	include	version.inc	; set build flags
	include biosseg.inc	; establish bios segment structure

sysinitseg segment

	public	baddblspace
	public	badopm,crlfm,badsiz_pre,badld_pre,badcom,badcountry
	public	badmem,badblock,badstack
	public	insufmemory,badcountrycom
	public	badorder,errorcmd
	public	badparm
ifdef   MULTI_CONFIG
        public  OnOff,StartMsg,$PauseMsg,$CleanMsg,$InterMsg
        public  $MenuHeader,$MenuPrmpt,$StatusLine,$InterPrmpt,$YES,$NO,$TimeOut
        public  BadComPrmpt,$AutoPrmpt
endif
        public  toomanydrivesmsg

	include msbio.cl3

sysinitseg	ends
	end
