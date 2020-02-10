;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****	icons.h - declarations for gloabally accessible icons
**
**   Date      Author	Modification
** --------   --------	------------------------------------------------
**  7/18/89   scottq	Created icons
*/

extern BITMAP bigUpArrow;
extern BITMAP bigDownArrow;
extern BITMAP bigProgramIcon;
extern BITMAP bigProgramIconInvert;
extern BITMAP bigFileIcon;
extern BITMAP bigFileIconInvert;

extern BITMAP bigFloppyIcon;
extern BITMAP bigFloppyIconInvert;

extern BITMAP bigHDIcon;
extern BITMAP bigHDIconInvert;

extern BITMAP bigRemoteIcon;
extern BITMAP bigRemoteIconInvert;

extern BITMAP bigRamDriveIcon;
extern BITMAP bigRamDriveIconInvert;

extern BITMAP bigCDRomIcon;
extern BITMAP bigCDRomIconInvert;

extern BITMAP bigRadioButtonUnSel;
extern BITMAP bigRadioButtonSel;

extern BITMAP bigDirExpansionIcon;
extern BITMAP bigDirCollapseIcon;
extern BITMAP bigDirNoZoomIcon;

extern BITMAP smallUpArrow;
extern BITMAP smallDownArrow;
extern BITMAP smallProgramIcon;
extern BITMAP smallProgramIconInvert;
extern BITMAP smallFileIcon;
extern BITMAP smallFileIconInvert;

extern BITMAP smallFloppyIcon;
extern BITMAP smallFloppyIconInvert;

extern BITMAP smallHDIcon;
extern BITMAP smallHDIconInvert;

extern BITMAP smallRemoteIcon;
extern BITMAP smallRemoteIconInvert;

extern BITMAP smallRamDriveIcon;
extern BITMAP smallRamDriveIconInvert;

extern BITMAP smallCDRomIcon;
extern BITMAP smallCDRomIconInvert;


extern BITMAP  smallRadioButtonUnSel;
extern BITMAP  smallRadioButtonSel;


extern BITMAP smallDirExpansionIcon;
extern BITMAP smallDirCollapseIcon;
extern BITMAP smallDirNoZoomIcon;

extern struct BITMAP	bigProgManIcon;
extern struct BITMAP	bigProgManIconInvert;
extern struct BITMAP	smallProgManIcon;
extern struct BITMAP	smallProgManIconInvert;
#define GroupIcon		(CHEIGHT > SMALLHEIGHT?&bigProgManIcon:&smallProgManIcon)
#define GroupIconInvert	(CHEIGHT > SMALLHEIGHT?&bigProgManIconInvert:&smallProgManIconInvert)

#define UpArrow 	    (CHEIGHT > SMALLHEIGHT?&bigUpArrow:&smallUpArrow)
#define DownArrow	    (CHEIGHT > SMALLHEIGHT?&bigDownArrow:&smallDownArrow)
#define ProgramIcon	    (CHEIGHT > SMALLHEIGHT?&bigProgramIcon:&smallProgramIcon)
#define ProgramIconInvert   (CHEIGHT > SMALLHEIGHT?&bigProgramIconInvert:&smallProgramIconInvert)
#define FileIcon	    (CHEIGHT > SMALLHEIGHT?&bigFileIcon:&smallFileIcon)
#define FileIconInvert	    (CHEIGHT > SMALLHEIGHT?&bigFileIconInvert:&smallFileIconInvert)
#define FloppyIcon	    (CHEIGHT > SMALLHEIGHT?&bigFloppyIcon:&smallFloppyIcon)
#define FloppyIconInvert    (CHEIGHT > SMALLHEIGHT?&bigFloppyIconInvert:&smallFloppyIconInvert)
#define HDIcon		    (CHEIGHT > SMALLHEIGHT?&bigHDIcon:&smallHDIcon)
#define HDIconInvert	    (CHEIGHT > SMALLHEIGHT?&bigHDIconInvert:&smallHDIconInvert)
#define RemoteIcon	    (CHEIGHT > SMALLHEIGHT?&bigRemoteIcon:&smallRemoteIcon)
#define RemoteIconInvert    (CHEIGHT > SMALLHEIGHT?&bigRemoteIconInvert:&smallRemoteIconInvert)
#define RamDriveIcon		(CHEIGHT > SMALLHEIGHT?&bigRamDriveIcon:&smallRamDriveIcon)
#define RamDriveIconInvert	  (CHEIGHT > SMALLHEIGHT?&bigRamDriveIconInvert:&smallRamDriveIconInvert)
#define CDRomIcon		(CHEIGHT > SMALLHEIGHT?&bigCDRomIcon:&smallCDRomIcon)
#define CDRomIconInvert	  (CHEIGHT > SMALLHEIGHT?&bigCDRomIconInvert:&smallCDRomIconInvert)

#define RadioButtonUnSel	(CHEIGHT > SMALLHEIGHT?&bigRadioButtonUnSel:&smallRadioButtonUnSel)
#define RadioButtonSel	    (CHEIGHT > SMALLHEIGHT?&bigRadioButtonSel:&smallRadioButtonSel)
#define DirExpansionIcon	(CHEIGHT > SMALLHEIGHT?&bigDirExpansionIcon:&smallDirExpansionIcon)
#define DirCollapseIcon     (CHEIGHT > SMALLHEIGHT?&bigDirCollapseIcon:&smallDirCollapseIcon)
#define DirNoZoomIcon	    (CHEIGHT > SMALLHEIGHT?&bigDirNoZoomIcon:&smallDirNoZoomIcon)


#define ProgManIcon ProgramIcon
#define ProgManIconInvert ProgramIconInvert


#if 0
/* ZZZZZZZZZZZZZZZZZZZZZZZZZZZ Unused icons ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ */

extern BITMAP bigRightArrow;
extern BITMAP bigLeftArrow;
extern BITMAP bigColorLeft;
extern BITMAP bigColorRight;

extern BITMAP  smallRightArrow;
extern BITMAP  smallLeftArrow;

extern BITMAP bigPgUpIcon;
extern BITMAP bigPgDnIcon;
extern BITMAP  smallPgUpIcon;
extern BITMAP  smallPgDnIcon;

#define RightArrow		(CHEIGHT > SMALLHEIGHT?&bigRightArrow:&smallRightArrow)
#define LeftArrow	    (CHEIGHT > SMALLHEIGHT?&bigLeftArrow:&smallLeftArrow)
#define ColorLeft	    (CHEIGHT > SMALLHEIGHT?&bigColorLeft:&bigColorLeft)
#define ColorRight	    (CHEIGHT > SMALLHEIGHT?&bigColorRight:&bigColorRight)
#define PgUpIcon	    (CHEIGHT > SMALLHEIGHT?&bigPgUpIcon:&smallPgUpIcon)
#define PgDnIcon	    (CHEIGHT > SMALLHEIGHT?&bigPgDnIcon:&smallPgDnIcon)

/* ZZZZZZZZZZZZZZZZZZZZZZZZZZZ Unused icons ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ */
#endif
