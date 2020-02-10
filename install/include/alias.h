/***************************************************************************/
/* 																								*/
/* ALIAS.H																						*/
/* 																								*/
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/* 																								*/
/* Global defines for the DOS 5.0 OEM and Retail install programs.			*/
/* 																								*/
/* Created 01/28/90	- johnhe																*/
/***************************************************************************/

/* #define	MEM_BUG	1 */

/***************************************************************************/
/* Normal variable typedefs. These type defs are compatible with OS2			*/
/* typedefs.																					*/
/***************************************************************************/

typedef		char					CHAR;
typedef		unsigned char		UCHAR;
typedef		int					INT;
typedef		unsigned int		UINT;
typedef		long					LONG;
typedef		unsigned long		UL;
typedef		float 				FLOAT;
typedef		double				DOUBLE;

/***************************************************************************/
/* Standard global constants.															  	*/
/* Don't change the TRUE define because some functions depend on it being	*/
/* 1 instead of !FALSE.																		*/
/***************************************************************************/

#ifndef		FALSE
   #define     FALSE          0
#endif

#ifndef		TRUE
   #define     TRUE           1
#endif

#ifndef		OFF
   #define     OFF            0
#endif

#ifndef		ON
   #define     ON             1
#endif

#ifndef		EOL
   #define     EOL            '\0'
#endif

#ifndef	  OK
   #define		OK			      0
#endif

#ifndef	  ABORT
   #define		ABORT			   -1
#endif

#ifndef	  ERROR
   #define		ERROR			   -1
#endif

#ifndef    ESC
	#define	  ESC					0x1b					/* ESC key ascii code		*/
#endif

#ifndef    PREVIOUS
	#define	  PREVIOUS			-2						/* Previous menu signal		*/
#endif

#ifndef	MAX_PATH
	#define		MAX_PATH					64 			/* Max DOS path length		*/
#endif

#ifndef	MAX_BLOCK
	#define		MAX_BLOCK				0xff00		/* Max file read/write		*/
#endif 														/* at one time size			*/


#define		MAX_PATH_LEN		128					/* Size of filepath buffers*/


/***************************************************************************/
/* Defines used by copy functions to determine the type of operation being	*/
/* performed.																					*/
/***************************************************************************/

#define		READ					0
#define		WRITE					1
#define 		DELETE				2
#define		RENAME				3
#define		CREATE				4
#define		CLEAR					5

/***************************************************************************/
/* Fatal error defines. This is an enumerated series which matches the 		*/
/* error messages in GLOBAL.TXT.															*/
/***************************************************************************/

#define		FATAL_MEMORY_ERROR		0
#define		FATAL_UNKOWN_ERROR		1
#define		FATAL_DOS_VERSION 		2
#define		FATAL_DISK_ERROR			3
#define		FATAL_DATA_READ_ERROR	4
#define		FATAL_HD_READ_ERROR		5
#define		FATAL_BPB_ERROR			6
#define		FATAL_HD_WRITE_ERROR 	7
#define		NO_ACT_PART					8
#define		BAD_MBR						9
#define		BAD_BPB						10
#define		BAD_SYS						11
#define		BAD_EBR						12
#define		BAD_RECOVERY_DISK 		13
#define		BAD_DOSDIR					14
#define		BAD_TMPDIR					15
#define		BAD_COMMAND_COM			16
#define		CORRUPT_DATA_FILE			17
#define		NO_COMSPEC_ERROR			18
#define		FATAL_SYSTEM_MISSING		19
#define		CORRUPT_DATA_ERROR		20
#define		ERROR_MOVING_FILE 		21
#define		ROOT_DIR_FULL				22
#define		ERROR_DELETING_TMP_FILE 23
#define		LIE_TABLE_UPDATE_ERROR	24
#define		NO_SUPPORTED_DISK_TYPES	25
#define		MAX_FATAL_ERROR			25

/* Compressed Disk defines */
#define		DOUBLE_SPACE_COMPRESSION_RATIO	0
#define		STACKER_COMPRESSION_RATIO			1
#define		SUPERSTOR_COMPRESSION_RATIO		2


/***************************************************************************/
/* Memory allocation function prototypes. If debugging is enabled the		*/
/* normal free() function is replaced by a function which does a heap walk	*/
/* before calling free(). A fatal error will be generated if the heap is	*/
/* corrupt.																						*/
/***************************************************************************/

void			*GetMemory( unsigned int Bytes );

#ifdef		MEM_BUG

	void  	FreeMemory( void *Addr );
	unsigned GetMemoryMax( void );
	int   	NumberAllocated( void );
	void  	InitializeMemory( void );

#else
		#define	FreeMemory( x )		free( x )

#endif

/***************************************************************************/
