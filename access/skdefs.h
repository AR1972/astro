/*******************/
/*   skdefs.h     */
/*******************/


/*#define DEBUG			1*/


/* define this when linking with other code i.e. Front-End and Keyboard Enhancements */

#define ALL_LINK 1

/*	defines for operating system  */

#define parPort 0x3BC
#define OS_DOS			1
#define OS_WINDOWS	2
#define OS_TTAM		3

#define TARGET OS_DOS
#if !defined (FRONT)

/* GENERAL TYPEDEF's and DEFINES    */

typedef unsigned char BOOL;
typedef unsigned char BYTE;
typedef unsigned int  WORD;


#define	TRUE 	1
#define	FALSE	0

#define NEAR	near
#define LONG	long
#define VOID	void
#define true	TRUE
#define false	FALSE

#define notOKstatus 0
#define okStatus 1



/*	ASCII DEFINITIONS	*/

#define NULLCHAR		0
#define TAB				9
#define LINEFEED		10
#define VERTICALTAB	11
#define FORMFEED		12
#define RETURN			13
#define SPACE			32
#define COMMA			44
#define PERIOD			46
#define ESC				27
#define ESCAPE			27
#define XON				17
#define XOFF			19

/* INTERNAL VARIABLE SIZE SPECIFICATIONS  */

#define MAXALIASLEN		20

#define CODEBUFFERLEN	20

#define COMMBUFLEN		32
#define DEACTHANDSHAKE	24
#define ACTHANDSHAKE		6

#define OUTPUTBUFLEN		64

#define MAXPOINTERSTACK	20
#define MAXLISTLEN		5

#define TIMERTIMEOUT 2
#define INT9TIMEOUT	55
#define HANDSHAKETIMEOUT 95	/* a little more than 5 seconds */

/*	STRUCTURE DEFINITIONS  */

struct serialDataType {
	BYTE serChar, status;
	};

struct asciiTableType {
	BYTE code1, code2;
	};

struct aliasTableType {
	char *aliasName;
	BYTE gideiCode;
	};

struct listType {
	BYTE len;
	BYTE list[MAXLISTLEN];
	};

#define MOUSEID 3
#define KEYBDID 2

typedef struct {
	unsigned int status;
	int deltaX;
	int deltaY;
	} mouDataType;

typedef struct {
	BYTE id;
	union {
		BYTE scanCode;
		mouDataType mou;
		};
	} outBufferType;

struct kStateType {			/* 1=currently pressed, 0 = not pressed */
	unsigned rShift:	1;
	unsigned lShift:	1;
	unsigned rCtrl:	1;
	unsigned lCtrl:	1;
	unsigned rAlt:		1;
	unsigned lAlt:		1;
	unsigned numlck:	1;
	};

struct scanCodeType {
	BYTE ascii, scan;
	};

struct scanTblType {
	struct scanCodeType base;
	struct scanCodeType shift;
	struct scanCodeType ctrl;
	struct scanCodeType alt;
	};

typedef void (*functionPtrType)(); 	/*  functionPtr type is pointer to a*/
									/*  function that returns nothing */

struct stackType {
	struct aliasTableType *aliasTablePtr;	/*  pointer to alias table	*/
	functionPtrType commandRoutinePtr;	/*  pointer to command|vector| routine */
	};

#endif

/* SERIAL PORT DEFINITIONS    */

#if TARGET==OS_DOS 
	#define		BAUD300	 0x0180
	#define		BAUD600	 0x00C0
	#define		BAUD1200	 0x0060
	#define		BAUD2400	 0x0030
	#define		BAUD4800	 0x0018
	#define		BAUD9600	 0x000C
	#define		BAUD19200 0x0006

/*	#define		COM1		0x3F8
	#define		COM2		0x2F8
	#define		COM3		0x3E8
	#define		COM4 		0x2E8
*/
	#define		IRQ4		4
	#define		IRQ3		3

	#define		RBR 0x00		/* DLAB = 0 read	*/
	#define		THR 0x00		/* DLAB = 0 write	*/
	#define		DLL 0x00		/* DLAB = 1	*/

	#define		IER 0x01		/* DLAB = 0	*/
	#define		DLM 0x01		/* DLAB = 1	*/

	#define		FCR 0x02		/*  FIFO Control Register (some IBM machines)	*/
	#define		IIR 0x02		/*  Interrupt Identification Register */
	#define		LCR 0x03
	#define		MCR 0x04
	#define		LSR 0x05		/* Line Status Register */

	#define		DR_FLAG 0x1
	#define		OR_FLAG 0x2
	#define		PE_FLAG 0x4
	#define		FE_FLAG 0x8
	#define		BI_FLAG 0x10
	#define		THRE_FLAG 0x20
	#define		TSRE_FLAG 0x40

#define		INT_RCV 	0x1
#define		INT_THRE	0x2
#define		INT_RLS	0x4

#define		RTS_BIT 0x2
#define		DTR_BIT 0x1

#endif

#if !defined (FRONT)

/* Mouse Definitions */

#define LEFTBUTTONMASK  1
#define RIGHTBUTTONMASK	2
#define NOMOUSE		0
#define BUSMOUSE		1
#define SERIALMOUSE	2
#define INPORTMOUSE	3
#define PS2MOUSE		4
#define HPMOUSE		5

#define ignoreCode			0
#define INSERT_MODE_MASK	0x80
#define CAPS_MODE_MASK		0x40
#define NUM_MODE_MASK		0x20
#define SCROLL_MODE_MASK	0x10
#define ALT_DOWN_MASK		0x08
#define CTRL_DOWN_MASK		0x04
#define LSHIFT_DOWN_MASK	0x02
#define RSHIFT_DOWN_MASK	0x01

#define INSERT_DOWN_MASK	0x80
#define CAPS_DOWN_MASK		0x40
#define NUM_DOWN_MASK		0x20
#define SCROLL_DOWN_MASK	0x10
#define PAUSE_MODE_MASK		0x08
#define SYSREQ_DOWN_MASK	0x04
#define LALT_DOWN_MASK		0x02
#define LCTRL_DOWN_MASK		0x01

#define EXTEND_KBD_MASK		0x10
#define RALT_DOWN_MASK		0x08
#define RCTRL_DOWN_MASK		0x04
#define LAST_CODE_E0_MASK	0x02
#define LAST_CODE_E1_MASK	0x01

#endif
