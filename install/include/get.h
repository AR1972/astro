/***************************************************************************/
/*                                                                         */
/*	GET.H																							*/
/*                                                                         */
/*		Copyright (c) 1991 - Microsoft Corp.											*/
/*		All rights reserved.																	*/
/*		Microsoft Confidential																*/
/*                                                                         */
/* Definitions and function prototypes for field entry functions.          */
/*                                                                         */
/* Created 880305 - johnhe                                                 */
/***************************************************************************/

#define     MAX_INT_VALUE     32000
#define     MAX_DECIMALS      6
#define     INT_LENGTH        6
#define     DATE_LENGTH       8
#define     TIME_LENGTH       5

/***************************************************************************/
/* Enumerated definitions for data types												*/
/***************************************************************************/

enum TYPE_TAG { NO_TYPE, TYPE_CHAR, TYPE_STRING, TYPE_INT, TYPE_TIME,
                TYPE_DATE, TYPE_FLOAT, TYPE_FIELD };

#define			MAX_TYPE			TYPE_FLOAT

/***************************************************************************/
/* Bit aligned structure to hold the date. May be read as an unsigned int. */
/* UnsignedValue = (unsigned *)(*(&Date));                                 */
/***************************************************************************/

struct	Date
{
	unsigned int		Day	:	5;
	unsigned int		Month :	4;
	unsigned int		Year	:	7;
};

/***************************************************************************/
/* Structure to hold the value being passed to and returned from the field */
/* editing functions.                                                      */
/***************************************************************************/

union	TYPES
{
	char						Char;                /* Single character */
	char						*String;             /* Pointer to a string */
	int						Int;                 /* Single integer value */
	struct Date          Date;                /* Date structure */
	unsigned					Time; 					/* Hours * 100 + Minutes */
   double               Float;               /* Single double value */
};	

/***************************************************************************/
/* Entry field definition function which must be passed to the field       */
/* editing functions.                                                      */
/***************************************************************************/

struct	field
{
	int				Type;             /* Data type from enumerated TYPE_TAG */
	int				Row;              /* Row on the display */
	int				Col;              /* Col for first character in the field */
   int            Length;           /* Length of field on screen */
	union TYPES		Data;             /* Data to initialize the field with */
   union TYPES    Min;              /* Minimum returnable value */
   union TYPES    Max;              /* Maximum returnable value */
};


/***************************************************************************/

   /*  GETFIELD.C */
int   GetField(struct field *field_info);
int	ProcessCtr(void );
int	ProcessExtended(void );
void  ScrUpdate(void );
void  SaveChar(void );
void  BackSpace(void );
void  ClrField(void );
void  InsChar(char *s,int c);
void  DeleteChar(void );
void  CursToEol(void );
void  InitString(void );
int	StringToString( char *szString, char *szReturnStr, int Min, int Max );
int	InitializeString(char *Str, union TYPES Data, int Length);
