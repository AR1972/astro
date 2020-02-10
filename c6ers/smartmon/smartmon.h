//
// Miscellaneous constants
//
#define BUFLEN		128
#define STATUSPOINTSIZE   8  // Point size of status bar font.
#define MAXFILENAMELEN	256
#define MAX_FREQUENCY	10000
#define MIN_FREQUENCY	 50
#define MAX_AUTOLOGTIME 480
#define MIN_AUTOLOGTIME   1
#define MAX_INTERVAL	100
#define MIN_INTERVAL	  3

#define DEF_INTERVAL	 30
#define DEF_FREQUENCY	500
#define DEF_AUTOSTOP   TRUE
#define DEF_LOGTIME	120
#define DEF_UPDATEDOS FALSE
#define DEF_TOPMOST    TRUE

//
// String table resource ID
//
#define IDS_TITLE	200
#define IDS_DRVBMP	201
#define IDS_HELPFILE	202
#define IDS_STARTLOG	203
#define IDS_STOPLOG	204
#define IDS_LOGGING	205
#define IDS_LOGSTOPPED	206
#define IDS_LOGSTARTED	207
#define IDS_CACHEACTIVE 208
#define IDS_CACHEIDLE	209
#define IDS_FLUSHED	210
#define IDS_RESET	211
#define IDS_LOGFILE	212
#define IDS_SAVEAUTO	213
#define IDS_STATUS	214

//
// Main dialog control ID
//
#define IDD_HELP	100
#define IDD_READONLY	101
#define IDD_READWRITE	102
#define IDD_NOCACHING	103
#define IDD_HITRATEBOX	104
#define IDD_DOSSIZE	105
#define IDD_WINSIZE	106
#define IDD_FLUSH	107
#define IDD_RESET	108
#define IDD_DRIVEID	109
#define IDD_HITS	110
#define IDD_MISSES	111
#define IDD_RATE	112
#define IDD_CHARTBOX	113
#define IDD_SAMPFREQ	114
#define IDD_OPTION	115
#define IDD_STARTLOG	116
#define IDD_STOPLOG	117
#define IDD_ABOUT	118
#define IDD_TOPMOST	119

//
// Options dialog control ID
//
#define IDD_MSEC	150
#define IDD_LOGFILE	151
#define IDD_AUTOSTOP	152
#define IDD_STOPTIME	153
#define IDD_SAVESET	154
#define IDD_INTERVAL	155
#define IDD_BATCHFILE	156

typedef struct tagDRVINFO {
    BYTE    type;	    // GetDriveType return value
    BYTE    status;	    // caching status
} DRVINFO;

//
// CACHE_A_DRIVE sub-function and return value
//
#define GET		0
#define ENABLE_READ	1
#define DISABLE_READ	2
#define ENABLE_WRITE	3
#define DISABLE_WRITE	4
#define SET		5   // not a real subfunction number

#define NO_READ 	0x0080
#define NO_WRITE	0x0040
#define NO_CACHING	0x00ff

//
// DOS Disk Transfer Area Structure
//
typedef struct tagDOSDTA {
    BYTE    reserved[21];
    BYTE    attrib;
    WORD    time;
    WORD    date;
    DWORD   size;
    char    name[13];
} DOSDTA;

//
// array index values for hbmpDirDrives array
// Note:  Two copies are kept, one for standard background,
// one for hilite.  Relative order is important.
//

#define BMPHIOFFSET 8

#define FLOPPYBMP     3
#define HARDDRVBMP    4
#define CDDRVBMP      5
#define NETDRVBMP     6
#define RAMDRVBMP     7

#define FLOPPYBMPHI   8
#define HARDDRVBMPHI  9
#define CDDRVBMPHI    10
#define NETDRVBMPHI   11
#define RAMDRVBMPHI   12

//
// Internal routine declarations
//
BOOL WambiInit( HANDLE );
void UpdateStat( HWND );
void InitRateBox( HWND );
void InitDriveBox( HWND );

//
// Bambi interface routines
//
extern long get_cache_hits( void );
extern long get_cache_misses( void );
extern int  get_cache_dirty_elements( void );
extern void commit_cache( void );
extern void reset_cache( void );
extern char cache_a_drive( int, int );
extern int  get_cache_info( int *, int * );

//
// Other assembler routines
//
extern void get_volume_label( int, LPSTR );
extern BOOL is_CDROM_drive( int );
extern BOOL is_RAM_drive( int );
extern BOOL is_valid_CD( void );
extern char get_current_drive( void );
extern void set_current_drive( int );
extern int  count_valid_drives( LPSTR );
extern int  is_Stacker_drive( int, LPSTR );
extern BYTE get_boot_drive( void );
extern WORD get_dos_version( void );
extern DWORD get_free_space( int );
