#include "stdarg.h"
#define MAX_DDRIVER_REP      0x19
#define GET_UMB_LINK_STATE   0x5802
#define SET_UMB_LINK_STATE   0x5803
#define UNLINK_UMBS          0x0000
#define LINK_UMBS            0x0001
#define FALSE                (char)(1==0)
#define TRUE                 !(FALSE)
#define NUL                  (char) '\0'
#define	MAX_CLDATA_INDEX     100
#define GET_PSP              (unsigned char ) 0x62 /* get PSP function call */
#define MEMORY_DET           0x12                  /* BIOS interrupt used to get total memory size */
#define CASSETTE             0x15                  /* interupt 15 */
#define EMS                  0x67
#define GET_VECT             0x35
#define DOSEMSVER            0x40
#define GetExtended          0x8800
#define EMSGetVer            0x4600
#define EMSGetFreePgs        0x4200
#define EMSGetStat           0x4000
#define toK(x)               ((x)>>10)
#define p_too_many           1
#define p_op_missing         2
#define p_not_in_sw          3
#define p_not_in_key         4

static const char SumFormat[] = "%-16m%8ld%8c%8ld%8c%8ld%8c";
static const char MemFormat[] = "%-16m%6c%6c%6c";

struct files
{
    unsigned char psp_addr;
    long conv_ttl;
    long umb_ttl;
    char driveridx;
};

struct umbs
{
    long umb_free;
    long umb_ttl;
    unsigned long umb_addr;
    unsigned long umb_large;
};

struct mem_classif
{
    unsigned long conv_ttl;
    unsigned long conv_free;
    unsigned long umb_ttl;
    unsigned long umb_free;
    unsigned long umb_large;
    unsigned long xms_ttl;
    unsigned long xms_free;
    unsigned long ems_ttl;
    unsigned long ems_free;
    unsigned long int_15h;
    unsigned long conv_large;
    int hma;
    unsigned long rom_ttl;
    char noof_progs;
    char noof_umbs;
    char xmsMvers;
    char xmsmvers;
    char xmsMdrvr;
    char xmsmdrvr;
    char emsMvers;
    char emsmvers;
    struct files files[MAX_CLDATA_INDEX];
    struct umbs umbs[MAX_CLDATA_INDEX];
};

struct	DEVICEHEADER {
    struct DEVICEHEADER far *NextDeviceHeader;
    unsigned    Attributes;
    unsigned    Strategy;
    unsigned    Interrupt;
    char        Name[8];
	};
	
struct	SYSIVAR {
    char far *DpbChain;
    char far *SftChain;
    char far *Clock;
    char far *Con;
    unsigned  MaxSectorSize;
    char far *BufferChain;
    char far *CdsList;
    char far *FcbChain;
    unsigned  FcbKeepCount;
    unsigned char BlockDeviceCount;
    char      CdsCount;
    struct DEVICEHEADER far *DeviceDriverChain;
    unsigned  NullDeviceAttributes;
    unsigned  NullDeviceStrategyEntryPoint;
    unsigned  NullDeviceInterruptEntryPoint;
    char      NullDeviceName[8];
    char      SpliceIndicator;
    unsigned  DosParagraphs;
    char far *DosServiceRntryPoint;
    char far *IfsChain;
    unsigned  BufferValues;
    unsigned  LastDriveValue;
    char      BootDrive;
    char      MoveType;
    unsigned  ExtendedMemory;
	};

struct sublistx {
	 unsigned char size;	       /* sublist size                         */
	 unsigned char reserved;       /* reserved for future growth           */
	 unsigned far *value;	       /* pointer to replaceable parm          */
	 unsigned char id;             /* type of replaceable parm             */
	 unsigned char flags;	       /* how parm is to be displayed          */
	 unsigned char max_width;      /* max width of replaceable field       */
	 unsigned char min_width;      /* min width of replaceable field       */
	 unsigned char pad_char;       /* pad character for replaceable field  */
	};

struct	ARENA	 {
	char     Signature;
	unsigned Owner;
	unsigned Paragraphs;
	char     Dummy[3];
	char     OwnerName[8];
	};

///////////////////////////////////////////////////////////////////////////////

void init_data(void);
void Parse_Message (int, char*);
void GetEMS(void);
void GetXMS(void);
char EMSInstalled(void);
void Correct_Total_XMS(void);
void check_screen(void);
void DisplayFree(void);
char *OwnerOf(struct ARENA far *);
void GetFromArgvZero(unsigned, unsigned far *);
char *TypeOf(struct ARENA far *);
unsigned int AddMem_to_Table(unsigned int, unsigned long, unsigned long, unsigned long);
void DoMainLine(int, unsigned long int*, int, unsigned long int*, int, unsigned long int*);
void mainline(unsigned long int *,int,unsigned long int *,int,unsigned long int *);
void DoMainLine_a(int, unsigned long int*, char*, unsigned long int*, char*, unsigned long int*);
void mainline_a(unsigned long int *,char *,unsigned long int *,char *,unsigned long int *);
void estrip(char far *);
char *DriverData(void far *);
unsigned int GetDDriverPSP(void);
unsigned int IsDDriverAround(char*);
void mprintf();
unsigned long AddressOf(char far *);
void GetExtraMemory(void);
int fIsPooled(void);
long XMSVersion(int far *);
long XMSDriver(int far *);
long CheckVDisk(void);
int IsPre286(void);
char *GetDeviceDriver(void far *);
int parse_cmd(int, char**);
int get_lines(void);
unsigned int DisplayBaseDetail(void);
void GetSummary(void);
void DispMEMSummary(void);
void DisplayClassification(void);
void DisplaySummary(void);

extern void sysloadmsg(union REGS *, union REGS *);
extern void sysdispmsg(union REGS *, union REGS *);
extern void sysgetmsg(union REGS *, struct SREGS *, union REGS *);
extern void parse(union REGS *, union REGS *);

//////////////////////////////////////////////////////////////////////////////

extern char *SingleDrive;
extern char *MultipleDrives;
extern char *UnOwned;
extern char *Ibmbio;
extern char *Ibmdos;
extern int ddriveridx;
extern unsigned LastPSP;
extern char UseArgvZero;
extern char EMSInstalledFlag;
extern int NoCR;
extern struct sublistx sublist[10];
extern struct SYSIVAR far *SysVarsPtr;
extern char ddrivername[MAX_DDRIVER_REP][9];
extern unsigned far *ArenaHeadPtr;
extern char OwnerName[128];
extern char TypeText[128];
extern char cmd_line[128];
extern unsigned long UMB_Head;
extern union REGS InRegs;
extern union REGS OutRegs;
extern struct SREGS SegRegs;
extern int BlockDeviceNumber;
extern int DataLevel;
extern int PageBreak;
extern int num_lines;
extern char ModName[40];
extern struct mem_classif mem_table;

