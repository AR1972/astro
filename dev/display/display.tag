;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */
M001 02/25/91 SHK	INT10COM.INC	Bug #5789: changed some comments and
					modifed an instruction so that
					an int 10, ax=1100 will be done
					instead of one with AX=1100, when
					one uses the 8X16 font! (al = 16decimal)
			INT2fCOM.INC	Bug #5789: Fn CON_INVOKE fixed so
					that it calls INVOKE_DATA with the
					new CP rather than the old one. I hope
					this fix is right. Fn, INVOKE_CP does
					it in this manner and I cloned it. BTW,
					MODE CON CP SELECT=value works and
					hence I believe that this cloning is
					OK!

M002 03/12/91 MD        int10com.inc    Reversed change made in M001,
                                        which introduced a new bug.
