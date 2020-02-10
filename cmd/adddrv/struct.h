;/*
; *                      Microsoft Confidential
; *                      Copyright (C) Microsoft Corporation 1991
; *                      All Rights Reserved.
; */

/***********************************************************************
 *----------------------------------------------------------------------
 *  Modification history
 *----------------------------------------------------------------------
 *	MSKK20	Aug 25, 1990	RokaH
 *		support DEVICEHIGH
 *----------------------------------------------------------------------
 */

typedef struct {	/* process data block */
	WORD int20;		/* int 20h system terminate */
	BYTE block[3];		/* size of execution block */
	BYTE cpm[5];		/* ancient call to system */
	DWORD exit;		/* pointer to exit routine */
	DWORD ctrlc;		/* pointer to ^C routine */
	DWORD fatal;		/* pointer to fatal error routine */
	WORD parent;		/* PID of parent */
	BYTE jfn_tab[20];	/* indicate into system table */
	WORD environ;		/* segment of evironment */
	DWORD stack;		/* stack of self during system calls */
	WORD jfn_len;		/* number of handles allowed */
	DWORD jfn_pointer;	/* pointer to JFN table */
	DWORD next;		/* pointer to nested PDB's */
} PDB, far * LPPDB;

typedef struct {	/* memory control block */
	BYTE sign;		/* 'M' for valid item, 'Z' for last item */
	WORD owner;		/* owner of arena item */
	WORD size;		/* size in para of item */
	BYTE Reserved[3];						/* MSKK10 */
	BYTE ArenaName[8];	/* Name of memory arena			   MSKK10 */
} ARENA;

typedef struct {	/* command line */
	BYTE count;		/* number of character in buffer */
	char buf[127];		/* buffer area */
} COMLINE;

typedef struct {	/* config command table */
	char ind;		/* alias character of command */
	char *comstr;		/* command string */
} CONFTAB;

typedef struct {	/* buffer control */
	char far *ptr;		/* point to current position of buffer */
	unsigned short cnt;	/* number of character in buffer */
} BUF;

typedef struct _dev { /* device header */
	struct _dev far *next;	/* pointer to next device */
	WORD	attr;		/* device attribute */
	WORD	strategy;	/* starategy entry point */
	WORD	Interrupt;	/* interrupt entry point */
	BYTE	name[8];	/* device name or number of device */
} far * LPDEV;

#define	ISCIN	0x0001		/* if on, standard input device */
#define	ISCLOCK	0x0008		/* if on, clock device */
#define	ISCHAR	0x8000		/* if on, character device. else block */

typedef struct _sft {
	WORD	ref_count;	/* number of processes sharing entry */
	WORD	mode;		/* more of access or high bit on if FCB */
	BYTE	attr;		/* attribute of file */
	WORD	flags;		/* Bits 8-15
				 * Bit 15 = 1 if remote file
				 *        = 0 if local file or device
				 * Bit 14 = 1 if date/time is not to be
				 *   set from clock at CLOSE.
				 *   Set by FILETIMES and FCB_CLOSE.
				 *   Reset by other reseters of the dirty bit
				 *   (WRITE)
				 * Bit 13 = Pipe bit (reserved)
				 *
				 * Bits 0-7 (old FCB_devid bits)
				 * If remote file or local file, bit
				 * 6=0 if dirty Device ID number, bits
				 * 0-5 if local file.
				 * bit 7=0 for local file, bit 7
				 *      =1 for local I/O device
				 * If local I/O device, bit 6=0 if EOF (input)
				 *		Bit 5=1 if Raw mode
				 *		Bit 0=1 if console input device
				 *		Bit 1=1 if console output device
				 *		Bit 2=1 if null device
				 *		Bit 3=1 if clock device */
	LPDEV	devptr;		/* Points to DPB if local file, points
				 * to device header if local device,
				 * points to net device header if remote */
	WORD	firclus;	/* First cluster of file (bit 15 = 0) */
	WORD	time;		/* Time associated with file */
	WORD	date;		/* Date associated with file */
	DWORD	size;		/* Size associated with file */
	DWORD	position;	/* Read/Write pointer or LRU count for FCBs */
	WORD	cluspos;	/* Position of last cluster accessed */
	WORD	lstclus;	/* Last cluster accessed */
	WORD	dirsec;		/* Sector number of directory sector
				 * for this file */
	BYTE	dirpos;		/* Offset of this entry in the above */
	BYTE	name[11];	/* 11 character name that is in the
				 * directory entry.
				 * This is used by close to detect file deleted
				 * and disk changed errors. */
	struct _sft far * chain;		/* link to next SF */
	WORD	uid;
	WORD	pid;
	WORD	mft;
} SFT, far * LPSFT;

typedef struct _sf {
	struct _sf far *link;
	WORD	count;		/* number of entries */
	SFT	table;		/* beginning of array of the following */
} SF, far * LPSF;

typedef struct {	/* MSDOS system infomation block */
	DWORD	dpb;		/* DPB chain */
	LPSF	sft;		/* SFT chain */
	LPDEV	clock;		/* CLOCK device */
	LPDEV	con;		/* CON device */
	WORD	maxsec;		/* max sector size */
	DWORD	buf;		/* BUFFER chain */
	DWORD	cds;		/* CDS list */
	DWORD	fcb;		/* FCB chain */
	WORD	keep;		/* keep count */
	BYTE	numio;		/* number of block devices */
	BYTE	ncds;		/* number of CDS */
	LPDEV	dev;	/* DEVICE list */
} SYSINFO, far *LPSYSINFO;

typedef struct {	/* vector array area */
	DWORD vec[256];		/* interrupt vectors */
} VECTOR;

typedef struct {	/* device link info */
	LPDEV	newdev;		/* new device driver */
	LPSFT	sft;		/* changed sft. if 0 no change */
	LPDEV	olddev;		/* original device */
	int	dd_flag;	/* if 0 dd_func not support */
} DEVINFO, far * LPDEVINFO;

#define	MAXDEVINFO	10

typedef struct {	/* DIDD infomation block */
	char sign[16];		/* signature */
	VECTOR vector;		/* interrupt vectors */
	LPDEV dev;		/* device link */
	LPDEV con;		/* console link */
	LPDEV clock;		/* clock link */
	int devcount;		/* how many device installed */
	DEVINFO	devinfo[MAXDEVINFO];	/* sft info table */
	WORD umbseg;		/* umb segment block address */ /* MSKK20*/
	char msreserve[32];	/* reserve area for MS */
	char oemreserve[128];	/* reserve area for OEM */
} SAVEINFO, far * LPSAVEINFO;
