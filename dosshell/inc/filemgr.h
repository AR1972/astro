;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/****	filemgr.h - common header file for all file manager modules
**
**   Date      Author	Modification
** --------   --------	------------------------------------------------
**  8/08/89   t-jeffro	created header, made treehdr.CurDirBranch an array.
**  9/22/89   harikris	Modified formatting, added prototypes, SORT fileds.
*/

/*
**								Constants
*/

#ifndef EOS
  #define EOS		'\0'	/* end-of-string marker	*/
#endif

#define PATHCHAR	'\\'	/* path component separator */

//  BEGIN IPG - Different thousands separator for GER/FRN/SPA/POR/DUT/ITN/SWE
#if defined(GER) || defined(FRN) || defined(SPA) || defined(POR) || defined(ITN) || defined(DUT)
  #define DIGITSEP	  '.'		  /* The character seperating digits in a number */
#else
  #if defined(SWE)
      #define DIGITSEP	  ' '
  #else
      #define DIGITSEP	  ','
  #endif
#endif
//  END IPG - Different thousands separator for GER/FRN/SPA/POR/DUT/ITN/SWE

/* This takes care of stuff like I:\  etc where after true name expansion
 * this path could be pretty long in case we allow 66 for directories of
 * the form I:\sdgd\dhh...
 */
#define MAX_PATH	101	/* max path length	*/

/* TMC values for scottq-style listboxes.
*/
#define tmcFileList0	0
#define tmcTreeList0	0
#define tmcFileList1	1
#define tmcTreeList1	1

#define LISTBOXTOP		6
#define SCREENEXTRAWIDTH  (axMac-80)
#define TREEBOXWIDTH	(36+SCREENEXTRAWIDTH)
#define FILEBOXWIDTH	44
#define FLATLEFTWIDTH	(28+SCREENEXTRAWIDTH)
#define FLATRIGHTWIDTH	52
#define SEARCHLEFT 0
#define SEARCHWIDTH (80+SCREENEXTRAWIDTH)

/* Size of some tree elements
*/
#define BITSPERWORD	(8*sizeof(unsigned)) /* number of bits in a WORD */
#define MAXNODE		33	/* max depth of directory tree+1*/
#define NAMELEN 	8	/* filename length w/o EOS		*/
#define EXTLEN  	3	/* file extension len w/o EOS	*/
#define PAGESIZE	256	/* files per page: should be a	*/
				/* multiple of BITSPERWORD		*/

/* modes for pattern matching routine */
#define FULLNAME	1
#define	EXTENSION	2

/* Used by CountBranch()
*/
#define CB_NORMAL  1		/* search for files		*/
#define CB_SUBDIR  2		/* search for directories	*/
#define CB_DESCEND 4		/* search in subtrees		*/

/* Global Focus Values */
#define MENUFOCUS 0
#define DRIVELIST0 1
#define TREE0  2	/* first tree list box (single or multiple modes) */
#define FILE0  3	/* first file list box, or flat display list box  */
#define DRIVELIST1 4
#define GROUPBOX 4
#define TREE1  5	/* second tree list box, multiple mode only */
#define TASKBOX 5
#define FILE1  6	/* secon file tree list box, multiple mode only */
/* Tree modes: single tree, double tree, or system (treeless)
*/
#define TR_SINGLE 1
#define TR_DOUBLE 2
#define TR_SYSTEM 3

#define TR_SEARCH 4

#ifndef NOGROUPINFILE
#define  TR_SHARE 5
#endif

// operation types for DoFileOp()
#define OP_COPY		1
#define OP_DELETE	2
#define OP_MOVE		3
#define OP_RENAME	4
#define OP_VIEW		5
#define OP_CHANGE	6
#define OP_PRINT	7
#define OP_ASSOCIATE	8

// Types of message box in GetResponse()
#define BT_QUIT			 1
#define BT_SKIPQUIT		 2
#define BT_RETRYSKIPQUIT 3 
#define BT_MODIFYATTRS	 5
#define BT_SKIPRETRY	 7
#define BT_FILESKIPQUIT	 9
#define BT_FILERETRYSKIPQUIT	10
#define BT_UNUSED3	 4 
#define BT_UNUSED1	 6
#define BT_UNUSED2	 8

// result codes from GetResponse()
#define ACT_OK		1
#define ACT_FORCE	2
#define ACT_RETRY	3
#define ACT_SKIP	4
#define ACT_CANCEL	5
#define ACT_NOMEM	6

#define ACT_ALL		10
#define ACT_SINGLE	11

/* Codes for SortKey in glob_t data structure */
/* Don't change these codes -- this is the order in which they occur in the
	display options dialog box and their item codes are used directly in the
	dialog procedure display options.									*/
#define SORT_NAME	0
#define SORT_EXT	1
#define SORT_DATE	2
#define SORT_SIZE	3
#define SORT_DISK	4

#define DRIVESPERLINE 13
#define NUMDRIVELISTS 26 /* A ... Z*/

/* (X, Y) co-ordinate of the first drive icon to be displayed on screen --
 * This is drive icon "A".
 */
#define DRIVEICONSTARTX 1
#define DRIVEICONSTARTY 3

/* This the lateral shift (X shift) between adjacent drive icons. */
#define DRIVESHIFT	6


#define RECALCNUMMATCHES 0xFFFF /* nummatches field in tree is invalid */

// Funny characters
#ifdef JAPAN
#define MIDLEFT		0x0e	/* looks like |-			*/
#define HORIZLINE	0x08	/* horizontal line			*/
#define VERTLINE	0x09	/* vertical line			*/
#define SELCHAR		0x1f	/* selection character			*/
#define ESCAPE		27	/* Escape Character */
#define LLCORNER	0x0d /* lower left corner */
#else
#define MIDLEFT		195	/* looks like |-			*/
#define HORIZLINE	196	/* horizontal line			*/
#define VERTLINE	179	/* vertical line			*/
#define SELCHAR		'\020'	/* selection character			*/
#define ESCAPE		27	/* Escape Character */
#define LLCORNER	192 /* lower left corner */
#endif

// Collapse dir mode characters
#define COLLAPSE_LCHAR	'['
#define COLLAPSE_RCHAR	']'
#define EXPANSION_CHAR	'+'
#define COLLAPSE_CHAR	'-'
#define NOZOOM_CHAR		' '

#define FMGRSB		2	/* screen line of current dir	*/
#define TABSIZE		8	/* # of spaces in a tab		*/
#define MAXCOLS		90	/* number of cols in widest screen mode */
#define MAXROWS		60	/* number of rows in highest screen res */

/* WARNING !! these numbers should be anything but 0. The function
	 driveexists returns this code & 0 if drive doen't exist.	*/
#define FLOPPY_TYPE		1
#define REMOTE_TYPE		2
#define HARDDISK_TYPE	3
#define RAMDRIVE_TYPE	4
#define CDROM_TYPE   5

/*
**								Structures
*/

/* Names for some of the bit fields in structure below to improve readability */
#define  LASTDIR			xb.db.lastdir
#define  FIRSTFILEFOUND	xb.db.firstfilefound
#define  EXPANDED			xb.db.expanded
#define  DISPLAYED		xb.db.displayed
#define  HASSUBDIRS		xb.db.hassubdirs
#define  DIRSORTED		xb.db.dirsorted

#define  SELECTED			xb.fb.selected
#define  MATCHESPATTERN	xb.fb.matchespattern
#define  FIRSTDIRFILE	xb.fb.firstdirfile
#define  LASTDIRFILE		xb.fb.lastdirfile
#define  DELMARKER		xb.fb.delmarker
#define  FILEOPMARKER	xb.fb.fileopmarker

/*	file tree node */
/* ZZZZ */
/* Note that in the following the bit field 'displayed' for directories is */
/* probably not needed as it is equivalent to finding out if its parent is */
/* expanded or not. Finding the parent of a node involves walking the entire*/
/* sibling chain as we don't have a parent pointer -- might be expensive!! */

typedef struct fi_t {
	unsigned char	 name[NAMELEN+EXTLEN];		// name of file w/o EOS
	union {
	    struct{
			unsigned date;		// last modify date (file!)
			unsigned time;		// last modify time (file!)
	    } dt;
	    struct{
			BYTE	    level;	// depth of directory (5 bits max...)
			BYTE	  unused1;	// unused-you are free to define it
			WORD	  unused2;	// unused-you are free to define it
	    } lx;
	}dtlx;

	/* The reason for using "unsigned attribs:8" instead of "BYTE attribs"	*/
	/* is to save a byte in this structure. 'unsigned' bit fields by		*/
	/* default use up a word (size of an unsigned!). This way we use up a	*/
	/* total of 2 bytes instead of 1+2=3.									*/
	unsigned	attribs	: 8;	// file attributes -- 1 BYTE.

	unsigned nosib		: 1;	// no further siblings

	union {
		struct {
			unsigned lastdir		: 1; // last directory of its siblings?
			unsigned firstfilefound : 1; // first file of a directory found?
										 // implies dir has atleast 1 file
			unsigned expanded		: 1; // dir expanded?
			unsigned displayed		: 1; //	dir displayed in collapsed mode?
			unsigned hassubdirs		: 1; // dir has atleast 1 sub-dir?
			unsigned dirsorted		: 1; // has dir been sorted?
		} db ; /* directory bit fields */
		struct {
			unsigned selected		: 1; // file selected?
			unsigned matchespattern : 1; // name matches file name filter?
												  // Filter is "MatchPat" as well as the
												  // glob.DisplayHiddenFiles
			unsigned firstdirfile	: 1; // first file in directory in
										 // appropriate sorted order?
			unsigned lastdirfile	: 1; // similar to above but for 'last'
			unsigned delmarker		: 1; // used to temporarily mark a file
										 // as deleted for directory sort!
			unsigned fileopmarker : 1; // used to temporarily mark a file
												// to indicate whether to perform fileop
												// on it or not!
		} fb ; /* file bit fields */
	} xb ; /* mutually exclusive bit fields between files & dirs */

	unsigned unused1	: 1;	// unused-you are free to define it

	struct fi_t far *sibling;	// ptr to next sibling
	union {
		struct {
			long size;			// size of file
			struct fi_t far *snext;	// next in sort order
		} f;
		struct {
			struct fi_t far *child;	// first child dir
			struct fi_t far *dnext;	// next directory
		} d;
	} x;
} fileinfo;

typedef fileinfo far *PENTRY;

typedef struct fp_t {
	fileinfo files[PAGESIZE];			// current directory
	struct fp_t far  *next;			// next page
} filepage;

typedef filepage far *PPAGE;

#define LABELLENGTH 12   /* Volume label for each tree 11 characters + Null*/
/* Tree structure
*/
typedef struct th_t {
	char root[4];				// string: root path
																
	PENTRY head;				// head of tree

	PPAGE pagehead;				// addr of 1st page
	PPAGE pagetail;				// addr of last page
	unsigned int freeind;		// index into pagetail of first free PENTRY
	unsigned int holecount;		// number of holes -- caused by deleting 
								// files or directories.

	struct find_t finds[MAXNODE]; // state of each dir srch

	PENTRY FirstDirectory;		// first directory in sort order
	PENTRY LastDirectory;		// last directory found found on disk
	PENTRY FirstFile;			// head of file linked list

	PENTRY LastFile;		// hack to just stick files to the end
								// of the snext chain.
	
	// 'Diskfilecount' != 'filecount' iff tree is in compact mode.
	WORD	Diskfilecount;		// number of files actually on the disk
	WORD	filecount;			// number of files in tree

	WORD	DirCount;			// # of directories in tree

	WORD	VisibleDirCount;	// # of directories visible in collapsed tree.

	unsigned Started	  : 1;	// we've begun reading in tree
	unsigned Compacted	  : 1;	// contains only dir's and files displayed dir
	unsigned ContinueTree	  : 1;  // tree not completely read in  yet

	unsigned fdiskinfoknown	  : 1;	// disk size info known or not!
	unsigned SortRequired	  : 1;	// to prevent background sorting!

	unsigned DisplayHiddenFiles : 1; // whether hidden/system files displayed!
	unsigned unused2		  : 1;
	unsigned unused3		  : 1;

	struct th_t far *next;		// next tree in drive list

	/* These items are used by AddFile(), akin to static var's.
	*/
	PENTRY parent;			// last directory searched
	PENTRY lastlink;		// last entry added by AddFile (only)
	int level;				// its tree depth

	int DriveType ;	// Drive type -- remote, local removable/not

	/* information about the drive -- useful for system mode & show-info */
	char VolLabel[LABELLENGTH] ;
	unsigned long SizeTotal ;
	unsigned long SizeAvail ;

	char mpat[NAMELEN+EXTLEN+2] ; /* for match pattern used by this tree */
	WORD nummatches;	      /* number of items in the tree which
					 match mpat. RECALCNUMMATCHES if this
					 should be recalculated before used */
	int skey ; /* sort key used by this tree */
	int tmode ; /* tree mode in which sort was done */

	unsigned int NumSel ; /* Number of files in the tree that are selected */
	unsigned long SizeSel ; /* Sum of sizes of files selected */

							// We have 2 of each of the following because we can
							// have upto 2 listboxes.
	PENTRY SelDir;	// directory that is selected -- cwd for this tree.
	WORD SelLine;  // line with the checkmark in tree -- selected dir above
	
} treehdr;

typedef treehdr far *PTREE;

typedef struct
{
	BYTE IconX;
	BYTE IconY;
	PTREE tree;
} driveinfo;

typedef driveinfo drivelist[26];

typedef struct {
	unsigned UpdateDrives : 1;	// need to update the drive icons
	unsigned UpdateTree   : 1;	// need to update the tree display
	unsigned UpdateFiles  : 1;	// need to update the file display
	PENTRY files;			// directory being displayed
	PTREE tree;			// tree being displayed
} filelistinfo;

/*
**								Variables
*/

typedef struct {
	unsigned InFileMgr		: 1;	// tree etc is onscreen
	unsigned MaxTree		: 1;	// max valid idx of Tree
	unsigned FocusBox		: 1;	// idx of listbox with focus
	unsigned CrossDirSel	: 1;	// Cross-dir selection enabled
	unsigned VerifyDelete	: 1;	// verify each deletion
	unsigned VerifyOverwrite: 1;	// verify file overwrites
	unsigned SelRepInd: 1;	// Index of tree to be replaced -- We actually
							// only retain the selections of 2 trees in mem.
							// this field is related to SelTree[2] below.
	unsigned DisplayHiddenFiles: 1 ; // should hidden/system files be displayed
	unsigned MouseConfirm	: 1 ;	// whether to confirm on mouse copy/moves */
	unsigned unused1			: 1 ;
	unsigned unused2			: 1 ;
	unsigned unused3			: 1 ;
	unsigned unused4			: 1 ;
	unsigned unused5			: 1 ;
	unsigned unused6			: 1 ;
	unsigned unused7			: 1 ;
	
	int	TreeMode;	// see top of file for choices
	PTREE drives;		// list of treehdrs, one for each
				// extant drive
	int DriveCount;		// number of extant drives
	int SortKey;		/* key for sorting -- name, ext, date, etc */

	/* Following word is used as storage to store and restore the
	 * flag "gDescendingOrder" to/from Swap file. The field "gDescendingOrder"
	 * is present for optimizing the string compare functions!
	 */
	WORD TempDescendingOrder ; 
	WORD FocusId ;
	/* Following are for saving/restoring file list box focus attributes */
	WORD TempFileListFocusItem[2]; // focus line of swapped out file lists
	WORD TempFileListScrolled[2];  // number of lines scrolled

	char MatchPat[NAMELEN+EXTLEN+2] ; /* global file match pattern */

	/*
	 * These next are two element arrays because there are
	 * two trees (when in two tree mode!)
	 */
	WORD lineselected[2] ; //line with the checkmark

	PTREE SelTree[2] ; // the two trees that have selections being shown in
					//	system tree mode
	
} global_t;

extern global_t glob;

extern filelistinfo listinfo[2];	// info to associate listbox to tree

extern char *gpszFileOpCaption ; /* caption for the file op being performed */

extern int (*SortFnArr[])(PENTRY, PENTRY) ;
extern int (*SortCmp)(PENTRY, PENTRY) ; /* pointer to function that does the
									 compare based on the selected sort key */

/* storage to save current tree-mode, sort-order, match-pat before entering	*/
/* search mode.																*/
extern char gSavedMatchPat[] ;
extern int gSavedTreeMode ; 
extern int gSavedSortOrder ; 
extern WORD gSavedGlobalFocus ; 

extern BOOL gfSearchDisk ; /* whether to search entire disk or not! */

/* Status line stuff - under development by scott */
#define FMGRSTATUSMAX 92
extern char gFMGRStatus[FMGRSTATUSMAX];

/* Currently set up so that a status line won't get truncated on the largest
 * screen size of 90 columns too!
 */
#define STATUS_LEN	91	// max status line length in GetResponse, ViewFile, etc 

extern BOOL gBeeps_On ; /* Whether to beep on error, etc or simply ignore it */

/* 'gfStatusMsgUp' states whether a status message specifying a fileop is
	being performed has been put up. */
extern BOOL gfStatusMsgUp ;

/* whether flatleft info has been drawn as yet in system tree mode. */
extern BOOL gfFlatLeftInfoDrawn ;

#define DOTTED_LINE	3

#define m_fPerformingViewFile() (gpszFileOpCaption == szViewFileCaption)


#define KEY_TAB		9
#define KEY_BS		8


/* The maximum length allowable for a directory name including drive letters */
#define ALLOWED_PATH 66
// maximum path text allowable; allowed path+'\'+8+'.'+3
#define USERS_MAX_TYPEABLE_PATH (ALLOWED_PATH + 13)

/* This mask is used to change a -1 to a 1 and a 1 to a -1. If one XOR's this
 * mask with the values this happens! Actually this trick can be used to
 * exchange any value to any other -- MASK = A xor B.
 */
#define MAGIC_XCHG_MASK	0xFFFE

/* Flag specifying what the sort order is -- Ascending/Descending order
 * A value of 0 implies ascending order and 0xFFFE (MAGIC_XCHG_MASK) means
 * descending order.
 */
extern WORD gDescendingOrder ;

/* How often to update the files/dirs read count. A value of 25 means that
 * every time a total of 25 entries (files+dirs) is read in, the screen will
 * be updated.
 */
#define DEFAULT_READ_UPDATE_FREQ 25



