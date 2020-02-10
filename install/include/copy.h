/***************************************************************************/
/* 																								*/
/* COPY.H																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Function prototypes and structure definitions for used by callers of 	*/
/* functions in COPY.C. 																	*/
/* 																								*/
/* 																								*/
/* Created 11-01-89 johnhe																	*/
/* TABS = 3																						*/
/***************************************************************************/


#define	INDEX_LEN		2			/* encode string into position and length */
#define	RING_BUF_LEN	4096					/* size of ring buffer				*/
#define	MAX_STR_LEN 	(16 + INDEX_LEN)	/* upper limit for match_length	*/
#define	UNPACK_BUF_LEN (512 * 17)			/* Size of unpacked buffer 		*/

#define	LANGUAGE_COMPRESS	1
#define	WINDOWS_COMPRESS	2

/***************************************************************************/

struct FILE_NAME
{
	char			*Source; 						/* Ptr to source file name 		*/
	char			*Destin; 						/* Ptr to destination file name	*/
};

/***************************************************************************/

struct FILE_PATH
{
	char			*Source; 						/* Ptr to source file's path     */
	char			*Destin; 						/* Ptr to destin file's path     */
};

/***************************************************************************/

struct DRIVE_LETTER
{
	char			Source;
	char			Destin;
};


/***************************************************************************/
/* struct MULT_FILES 																		*/
/* 																								*/
/* Structure used by the multifile copy functions to identify a file to		*/
/* be copied and information about how much of the file has been copied 	*/
/* and where in the copy buffer the file's data is located.                */
/***************************************************************************/

struct MULT_FILES
{
	struct FILE_NAME			Name; 			/* Source & destin file names 	*/
	struct FILE_PATH			Path; 			/* Source & destin file paths 	*/
	struct DRIVE_LETTER		Drive;			/* Source & destin drv letter 	*/
	long							lSourceSize;	/* Size of source	file				*/
	long							lDestinSize;	/* Size of destination file		*/
	long							lRead;			/* Bytes read from source. 		*/
	long							lWritten;		/* Bytes written to destination	*/
	long							lUnpacked;		/* Num of packed bytes unpacked	*/
	int							DiskNumber; 	/* Distrib disk# for source file */
	int							UserDisk;		/* User disk # for destination	*/
	unsigned						Time;				/* File creation time				*/
	unsigned						Date;				/* File creation date				*/
	unsigned char				IsPacked;		/* Flags file is compressed		*/
	char far						*pchStart;		/* Ptr to file's data in cpy buf */
};

/***************************************************************************/

struct  PACKED_HEADER
{												/* File info structure		*/
	 UCHAR	MagicStr[8];				/* array of magic words 	*/
	 long 	lDestinSize;				/* uncompressed file size	*/
};

/***************************************************************************/
#define		WIN_STR	"SZDD\x88\xf0\x27\x33"	/* "SZDDˆð'3" */

struct	WIN_HEADER
{
	char				MagicStr[8];
	unsigned char	Algorithm;
	unsigned char	Version;
	long				lDestinSize;				/* uncompressed file size	*/
};

/***************************************************************************/

#define	PACKED_HEADER_SIZE	sizeof( struct PACKED_HEADER )

/***************************************************************************/


extern void		Xcopy					( struct MULT_FILES File[] );


