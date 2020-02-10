#--------------------------------------------------------------------
# File: ..\release\makefile:
#
# This makefile contains make information for making either a release
# or test version of QB5 using either the cow or twin user interface.
# This file should be equivalent to the ..\test\makefile.
#
# The following macros are recognized from the command line:
#   TWIN=	build a twin version of qb (default is cow)
#   CC_USR	added cl command line args (ie: to generate listings)
#   MASM_USR	added masm command line args
#
# This file is used in conjunction with:
#   \qb\ir\makefile	version independent interpreter source
#   ..\ir\makefile	QB5 specific interpreter source
#   ..\qb\makefile	QBI version dependent source
#   ..\qb\maketwin	twin user interface code
#   ..\ul\makefile	cow user interface code
#--------------------------------------------------------------------

QB5	=	\45\qb5
QB	=	$(QBD)\45\qb

PROD	=	qb

# Macros specific to release version
#
REL	 =
MKDIR	 = $(QB5)\qbas
CC_VER	 = -Os
MASM_VER = -W2

# Set up twin/cow macros
#
!ifdef TWIN
UIBLD = twin
QBIMSGS_TXT = $(QB5)\ir\qbtwmsgs.txt

!else
UIBLD = cow
QBIMSGS_TXT = $(QB5)\ir\qbasmsgs.txt
UIMSGBOX = uimsgbox
!endif

# Default compilation switches and include directories
#
CC_INCL = -I. -I$(QB5)\hd -I$(QB)\hd -I$(TL)\..\inc
CC = -W3 -c -AM -Gcs $(CC_INCL) $(CC_MOD) $(CC_VER) $(CC_USR)

# Default assembly switches and include directories
#
MASM_INCL = -E -I. -I$(QB5)\hd -I$(QB)\hd -I$(TL)\..\inc
MASM = $(MASM_INCL) $(MASM_MOD) $(MASM_VER) $(MASM_USR)

# VarMgr-specific compilation switches
#
VARFLAGS = 
DEBFLAGS =
PRSFLAGS =
TXTFLAGS =
EXFLAGS  =

PEROPCOD_TXT = $(QB)\ir\peropcod.txt
BNF_PRS = $(QB5)\ir\qbasbnf.prs
MAKEMSGS_EXE = $(TL)\makemsgs.exe

# Standard LINK macros
#
MAPNAME =	/M:5000
CLIB	=	LIBH+MLIBCR/NOE
RTLIB	=	BQB50.LIB
MATHLIB =	QBBLIBFP+LQBEM

# Other makefiles
#
!include $(QB5)\ir\makefile
!include $(QB)\ir\makefile
!include $(QB5)\uq\makefile
!include $(QB5)\qb\makefile.qbs
