.asm.obj:
	$(asm) -DMODEL_COMPACT $(ainc) $(aflags) $*.asm,$*.obj;
!IFDEF BUILD_L
	$(asm) -DMODEL_LARGE $(ainc) $(aflags) $*.asm,large\$*.obj;
!ENDIF
!IFDEF BUILD_M
	$(asm) -DMODEL_MEDIUM $(ainc) $(aflags) $*.asm,medium\$*.obj;
!ENDIF

.c.obj:
	$(cc) -AC $(cflags) -Fo$*.obj $*.c
!IFDEF BUILD_L
	$(cc) -AL $(cflags) -Folarge\$*.obj $*.c
!ENDIF
!IFDEF BUILD_M
	$(cc) -AM $(cflags) -Fomedium\$*.obj $*.c
!ENDIF


include	= ..\..\include
extasw	= -I.. -I..\..
extcsw	= -Os -Gs -I. -I$(include) -c



STRLIB.LIB:	BLDPATH.OBJ  \
		CHRINS.OBJ   \
		CHRREPL.OBJ  \
		FINDPARM.OBJ \
		DIRTOFN.OBJ  \
		HUGEADD.OBJ  \
		HUGENORM.OBJ \
		ISWHITE.OBJ  \
		MATCHEXT.OBJ \
		MAXHUGE.OBJ  \
		MAXINT.OBJ   \
		NEXTWORD.OBJ \
		NONWHITE.OBJ \
		NUMSTRS.OBJ  \
		PARSEFN.OBJ  \
		PATHCHR.OBJ  \
		PATHS.OBJ    \
		RMTRAIL.OBJ  \
		SKIPSPC.OBJ  \
		SKIPWORD.OBJ \
		STRMAX.OBJ   \
		STRPAD.OBJ   \
		STRRMSPC.OBJ \
		STRSHL.OBJ   \
		STRSRCH.OBJ  \
		DBCS.OBJ
	del strlib.lib
	lib @libfile
!IFDEF BUILD_L
	cd  large
	del strlib.lib
	lib @..\libfile
	cd  ..
!ENDIF
!IFDEF BUILD_M
	cd  medium
	del strlib.lib
	lib @..\libfile
	cd  ..
!ENDIF



BLDPATH.OBJ:	BLDPATH.C   $(include)\strlib.h

CHRINS.OBJ:	CHRINS.C    $(include)\strlib.h

CHRREPL.OBJ:	CHRREPL.C   $(include)\strlib.h

DIRTOFN.OBJ:	DIRTOFN.C   $(include)\strlib.h

FINDPARM.OBJ:	FINDPARM.C  $(include)\strlib.h

HUGEADD.OBJ:	HUGEADD.C   $(include)\strlib.h

HUGENORM.OBJ:	HUGENORM.C  $(include)\strlib.h

ISWHITE.OBJ:	ISWHITE.C   $(include)\strlib.h

MATCHEXT.OBJ:	MATCHEXT.C  $(include)\strlib.h

MAXHUGE.OBJ:	MAXHUGE.C   $(include)\strlib.h

MAXINT.OBJ:	MAXINT.C    $(include)\strlib.h

NEXTWORD.OBJ:	NEXTWORD.C  $(include)\strlib.h

NONWHITE.OBJ:	NONWHITE.C  $(include)\strlib.h

NUMSTRS.OBJ:	NUMSTRS.C   $(include)\strlib.h

PARSEFN.OBJ:	PARSEFN.C   $(include)\strlib.h

PATHCHR.OBJ:	PATHCHR.C   $(include)\strlib.h

PATHS.OBJ:	PATHS.C     $(include)\strlib.h

RMTRAIL.OBJ:	RMTRAIL.C   $(include)\strlib.h

SKIPSPC.OBJ:	SKIPSPC.C   $(include)\strlib.h

SKIPWORD.OBJ:	SKIPWORD.C  $(include)\strlib.h

STRMAX.OBJ:	STRMAX.C    $(include)\strlib.h

STRPAD.OBJ:	STRPAD.C    $(include)\strlib.h

STRRMSPC.OBJ:	STRRMSPC.C  $(include)\strlib.h

STRSHL.OBJ:	STRSHL.C    $(include)\strlib.h

STRSRCH.OBJ:	STRSRCH.C   $(include)\strlib.h

DBCS.OBJ:	DBCS.C

