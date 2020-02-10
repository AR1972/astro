;filename.inc - This file defines all the _ASM switch names.
;it is ONLY included by switch.inc.  No make file contains dependencies on
;filename.inc, because any change that would affect one of these switch names
;will change a file that the makefile will re-assemble.  These switch names
;are in filename.inc (and not switch.inc) so that unnecessary changes won't
;cause all files to be re-assembled & so that switch.inc won't be overloaded
;with a lot of switch names (which contain little value).
;

;These switches are used to selectively switch out EXTRN's from include files.
;For example, nammgr.asm would want to switch out the EXTRN's in names.inc that
;are defined in nammgr.asm.  The EXTRN's are grouped in the .inc files so that
;the routines can easily be changed from FAR to NEAR without having to check
;out and edit all the files that reference them.  This also allows modules to 
;call routines without having to put EXTRNS in the source file. 
ifndef	BINSAV_ASM
	BINSAV_ASM = OFF
endif
ifndef	BDDEBUG_ASM
	BDDEBUG_ASM = OFF
endif
ifndef	BDMGR_ASM
	BDMGR_ASM = OFF
endif
ifndef	CMDEQV_ASM
	CMDEQV_ASM = OFF
endif
ifndef	CONMISC_ASM
	CONMISC_ASM = OFF
endif
ifndef	CONTEXT_ASM
	CONTEXT_ASM = OFF
endif
ifndef	EXAID_ASM
	EXAID_ASM = OFF
endif
ifndef	EXARRAY_ASM
	EXARRAY_ASM = OFF
endif
ifndef	EXARYUTL_ASM
	EXARYUTL_ASM = OFF
endif
ifndef	EXMATHOP_ASM
	EXMATHOP_ASM = OFF
endif
ifndef	EXCONTXT_ASM
	EXCONTXT_ASM = OFF
endif
ifndef	EXCONV_ASM
	EXCONV_ASM = OFF
endif
ifndef	EXDEBUG_ASM
	EXDEBUG_ASM = OFF
endif
ifndef	EXEVENT_ASM
	EXEVENT_ASM = OFF
endif
ifndef	EXFNMISC_ASM
	EXFNMISC_ASM = OFF
endif
ifndef	EXFOR_ASM
	EXFOR_ASM = OFF
endif
ifndef	EXGOTO_ASM
	EXGOTO_ASM = OFF
endif
ifndef	EXGRAPH_ASM
	EXGRAPH_ASM = OFF
endif
ifndef	EXIDSTUB_ASM
	EXIDSTUB_ASM = OFF
endif
ifndef	EXID_ASM
	EXID_ASM = OFF
endif
ifndef	EXIF_ASM
	EXIF_ASM = OFF
endif
ifndef	EXIO_ASM
	EXIO_ASM = OFF
endif
ifndef	EXLIT_ASM
	EXLIT_ASM = OFF
endif
ifndef	EXMATHOP_ASM
	EXMATHOP_ASM = OFF
endif
ifndef	EXMISC_ASM
	EXMISC_ASM = OFF
endif
ifndef	EXOS_ASM
	EXOS_ASM = OFF
endif
ifndef	EXPRINT_ASM
	EXPRINT_ASM = OFF
endif
ifndef	EXPROC_ASM
	EXPROC_ASM = OFF
endif
ifndef	EXREC_ASM
	EXREC_ASM = OFF
endif
ifndef	EXSTMISC_ASM
	EXSTMISC_ASM = OFF
endif
ifndef	EXSTRING_ASM
	EXSTRING_ASM = OFF
endif
ifndef	EXSTSTUB_ASM
	EXSTSTUB_ASM = OFF
endif
ifndef	EXSTUB1_ASM
	EXSTUB1_ASM = OFF
endif
ifndef	EXTORT_ASM
	EXTORT_ASM = OFF
endif
ifndef	EXUIDEB_ASM
	EXUIDEB_ASM = OFF
endif
ifndef	HENGINE_ASM
	HENGINE_ASM = OFF
endif
; only needed for EB
ifndef	INIT_ASM 
	INIT_ASM = OFF
endif
; not needed for EB
ifndef	INITTERM_ASM 
	INITTERM_ASM = OFF
endif
ifndef	LSID_ASM 
	LSID_ASM = OFF
endif
ifndef	LSOPTAB_ASM 
	LSOPTAB_ASM = OFF
endif
ifndef	LSMAIN_ASM 
	LSMAIN_ASM = OFF
endif
ifndef	LSRULES_ASM 
	LSRULES_ASM = OFF
endif
ifndef	LSSTEP2_ASM 
	LSSTEP2_ASM = OFF
endif
ifndef	LSUTIL_ASM 
	LSUTIL_ASM = OFF
endif
ifndef	MEMEMUL_ASM
	MEMEMUL_ASM = OFF
endif
ifndef	NAMDEBUG_ASM
	NAMDEBUG_ASM = OFF
endif
ifndef	NAMMGR_ASM
	NAMMGR_ASM = OFF
endif
ifndef	OPNAMATR_ASM
	OPNAMATR_ASM = OFF
endif
ifndef	OPNAMES_ASM
	OPNAMES_ASM = OFF
endif
ifndef	OPTABLE_ASM
	OPTABLE_ASM = OFF
endif
ifndef	PRSCTL_ASM
	PRSCTL_ASM = OFF
endif
ifndef	PRSEXP_ASM
	PRSEXP_ASM = OFF
endif
ifndef	PRSCG_ASM
	PRSCG_ASM = OFF
endif
ifndef	PRSID_ASM
	PRSID_ASM = OFF
endif
ifndef	PRSLEX_ASM
	PRSLEX_ASM = OFF
endif
ifndef	PRSMAIN_ASM
	PRSMAIN_ASM = OFF
endif
ifndef	PRSNT_ASM
	PRSNT_ASM = OFF
endif
ifndef	PRSNT1_ASM
	PRSNT1_ASM = OFF
endif
ifndef	PRSRWT_ASM
	PRSRWT_ASM = OFF
endif
ifndef	PRSSTMT_ASM
	PRSSTMT_ASM = OFF
endif
ifndef	PRSSTATE_ASM
	PRSSTATE_ASM = OFF
endif
ifndef	PRSUTIL_ASM
	PRSUTIL_ASM = OFF
endif
ifndef	QBLIST_ASM
	QBLIST_ASM = OFF
endif
ifndef	QBIMSGS_ASM
	QBIMSGS_ASM = OFF
endif
ifndef	RSALPHA_ASM
	RSALPHA_ASM = OFF
endif
ifndef	RTERROR_ASM
	RTERROR_ASM = OFF
endif
ifndef	RTTEMP_ASM
	RTTEMP_ASM = OFF
endif
ifndef	SSOPTAB_ASM
	SSOPTAB_ASM = OFF
endif
ifndef	SSDECLARE_ASM
	SSDECLARE_ASM = OFF
endif
ifndef	SSDESCAN_ASM
	SSDESCAN_ASM = OFF
endif
ifndef	SSBINFUN_ASM
	SSBINFUN_ASM = OFF
endif
ifndef	SSBOS_ASM
	SSBOS_ASM = OFF
endif
ifndef	SSDATA_ASM
	SSDATA_ASM = OFF
endif
ifndef	SSDECLARE_ASM
	SSDECLARE_ASM = OFF
endif
ifndef	SSAID_ASM
	SSAID_ASM = OFF
endif
ifndef	SSID_ASM
	SSID_ASM = OFF
endif
ifndef	SSIF_ASM
	SSIF_ASM = OFF
endif
ifndef	SSLIT_ASM
	SSLIT_ASM = OFF
endif
ifndef	SSMISC_ASM
	SSMISC_ASM = OFF
endif
ifndef	SSMOPEXE_ASM
	SSMOPEXE_ASM = OFF
endif
ifndef	SSOPERAT_ASM
	SSOPERAT_ASM = OFF
endif
ifndef	SSPROC_ASM
	SSPROC_ASM = OFF
endif
ifndef	SSREC_ASM
	SSREC_ASM = OFF
endif
ifndef	SSREFARG_ASM
	SSREFARG_ASM = OFF
endif
ifndef	SSRUDE_ASM
	SSRUDE_ASM = OFF
endif
ifndef	SSRULES_ASM
	SSRULES_ASM = OFF
endif
ifndef	SSSCAN_ASM
	SSSCAN_ASM = OFF
endif
ifndef	SSTXUTIL_ASM
	SSTXUTIL_ASM = OFF
endif
ifndef	TXTDATA_ASM
	TXTDATA_ASM = OFF
endif
ifndef	TXTDEL_ASM
	TXTDEL_ASM = OFF
endif
ifndef	TXTDEB_ASM
	TXTDEB_ASM = OFF
endif
ifndef	TXTDIR_ASM
	TXTDIR_ASM = OFF
endif
ifndef	TXTFIND_ASM
	TXTFIND_ASM = OFF
endif
ifndef	TXTLOAD_ASM
	TXTLOAD_ASM = OFF
endif
ifndef	TXTMGR_ASM
	TXTMGR_ASM = OFF
endif
ifndef	TXTMOVE_ASM
	TXTMOVE_ASM = OFF
endif
ifndef	TXTSAVE_ASM
	TXTSAVE_ASM = OFF
endif
ifndef	TXTTHR_ASM
	TXTTHR_ASM = OFF
endif
ifndef	TXTUTIL_ASM
	TXTUTIL_ASM = OFF
endif
ifndef	TYPMGR_ASM
	TYPMGR_ASM = OFF
endif
ifndef	UIDEBUG_ASM
	UIDEBUG_ASM = OFF
endif
ifndef	UIFILEIO_ASM
	UIFILEIO_ASM = OFF
endif
ifndef	UIMAIN_ASM
	UIMAIN_ASM = OFF
endif
ifndef	UIMEMORY_ASM
	UIMEMORY_ASM = OFF
endif
ifndef	UIMISC_ASM
	UIMISC_ASM = OFF
endif
ifndef	UINHELP_ASM
	UINHELP_ASM = OFF
endif
ifndef	UIRS_ASM
	UIRS_ASM = OFF
endif
ifndef	UIRSRC_ASM
	UIRSRC_ASM = OFF
endif
ifndef	UIUTIL_ASM
	UIUTIL_ASM = OFF
endif
ifndef	UIVARHLP_ASM
	UIVARHLP_ASM = OFF
endif
ifndef	UIWIND_ASM
	UIWIND_ASM = OFF
endif
ifndef	UTIL_ASM
	UTIL_ASM = OFF
endif
ifndef	VARMGR_ASM
	VARMGR_ASM = OFF
endif
ifndef	VARUTIL_ASM
	VARUTIL_ASM = OFF
endif

ifndef	UIEDIT_ASM
	UIEDIT_ASM = OFF
endif

ifndef	UIWORD_ASM
	UIWORD_ASM = OFF
endif
