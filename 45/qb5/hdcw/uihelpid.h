/*
 * Help IDs for the dialogs.  These IDs are hardcoded into the help file
 * The only restriction to the numbers used is that they must not conflict
 * with any numbers in QBIMSGS.TXT, which is why I started the numbers
 * at 900.
 *
 * NOTE: all defines in this file must not have a space between the
 *	 # and the define.  No other constructs besides defines and
 *	 comments should be in the file, as it is mechanically converted
 *	 into a different form to be used with converting help files.
 *
 *	 The format must be identical to qbimsgs.h for the makehelp.sed
 *	 script to work properly.
 *
 */

#define hidFileSave		902 /* File Save Dialog */
#define hidFilePrint		904 /* File Print Dialog */
#define hidPrintSet		905 /* File Print Dialog */
#define hidSearchChange 	907 /* Search & Change Dialog */
#define hidSearchFind		908 /* Search/Find Dialog */
#define hidConfirm		909 /* Confirm Search & Change Dialog */
#define hidViewSubs		911 /* View Subs Dialog */
#define hidNewProc		912 /* New PRocedure Dialog */
#define hidOptnsDisplay 	913 /* Options Display Dialog */
#define hidOptnsPaths		914 /* Options Path Dialog */
#define hidGreetingBox 		916 /* Greeting Box Dialog */

#define hidFileOpen		917 /* File Open Dialog (Not in .DES file) */

#define hidFileNew 		919 /* File New Dialog */

#define hidPrintSetup   920   /* Print Setup Dialog */

#define helpOnHelpId		9998 /* HelpOnHelp Menu Item */
#define helpIndexId		9997 /* HelpIndex Menu Item */
#define helpTableId		9996 /* Help Table of Contents Menu Item */
#define helpSurvivalId		9995 /* QB survival guide */

#define helpKeyboardId		9994 /* Help.Keyboard */
#define helpStartedId		9993 /* Help.Getting Started */
#define helpEditorHelp		9992 /* SHIFT+F1 in editor */
#define helpHowToUseId		9991 /* QHELP How To Use... */
#define helpQHContentsId        9990 /* QHELP Contents */


#define QB_ER_NF		 2001 /* NEXT without FOR */
#define QB_ER_SN		 2002 /* Syntax error */
#define QB_ER_RG		 2003 /* RETURN without GOSUB */
#define QB_ER_OD		 2004 /* Out of DATA */
#define QB_ER_FC		 2005 /* Illegal function call */
#define QB_ER_OV		 2006 /* Overflow */
#define QB_ER_OM		 2007 /* Out of memory */
#define QB_ER_UL		 2008 /* Label not defined */
#define QB_ER_SOR		 2009 /* Subscript out of range */
#define QB_ER_DD		 2010 /* Duplicate definition */
#define QB_ER_DV0		 2011 /* Division by zero */
#define QB_ER_ID		 2012 /* Illegal in direct mode */
#define QB_ER_TM		 2013 /* Type mismatch */
#define QB_ER_OS		 2014 /* Out of string space */
#define QB_ER_BS		 2016 /* String formula too complex */
#define QB_ER_CN		 2017 /* Cannot continue */
#define QB_ER_UF		 2018 /* Function not defined */
#define QB_ER_NR		 2019 /* No RESUME */
#define QB_ER_RE		 2020 /* RESUME without error */
#define QB_ER_DTO		 2024 /* Device timeout */
#define QB_ER_DF		 2025 /* Device fault */
#define QB_ER_FN		 2026 /* FOR without NEXT */
#define QB_ER_OP		 2027 /* Out of paper */
#define QB_ER_WH		 2029 /* WHILE without WEND */
#define QB_ER_WE		 2030 /* WEND without WHILE */
#define QB_ER_DL		 2033 /* Duplicate label */
#define QB_ER_US		 2035 /* Subprogram not defined */
#define QB_ER_AC		 2037 /* Argument-count mismatch */
#define QB_ER_UA		 2038 /* Array not defined */
#define QB_ER_CaseElse		 2039 /* CASE ELSE expected */
#define QB_ER_VarReq		 2040 /* Variable required */
#define QB_ER_FOV		 2050 /* FIELD overflow */
#define QB_ER_IER		 2051 /* Internal error */
#define QB_ER_BFN		 2052 /* Bad file name or number */
#define QB_ER_FNF		 2053 /* File not found */
#define QB_ER_BFM		 2054 /* Bad file mode */
#define QB_ER_FAO		 2055 /* File already open */
#define QB_ER_FSA		 2056 /* FIELD statement active */
#define QB_ER_IOE		 2057 /* Device I/O error */
#define QB_ER_FAE		 2058 /* File already exists */
#define QB_ER_BRL		 2059 /* Bad record length */
#define QB_ER_DFL		 2061 /* Disk full */
#define QB_ER_RPE		 2062 /* Input past end of file */
#define QB_ER_BRN		 2063 /* Bad record number */
#define QB_ER_IFN		 2064 /* Bad file name */
#define QB_ER_TMF		 2067 /* Too many files */
#define QB_ER_DNA		 2068 /* Device unavailable */
#define QB_ER_CBO		 2069 /* Communication-buffer overflow */
#define QB_ER_PRM		 2070 /* Permission denied */
#define QB_ER_DNR		 2071 /* Disk not ready */
#define QB_ER_DME		 2072 /* Disk-media error */
#define QB_ER_ADF		 2073 /* Advanced feature unavailable */
#define QB_ER_RAD		 2074 /* Rename across disks */
#define QB_ER_PAE		 2075 /* Path/File access error */
#define QB_ER_PNF		 2076 /* Path not found */
#define QB_ER_UE		 2077 /* Unprintable error */
