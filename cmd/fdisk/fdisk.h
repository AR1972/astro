//fdisk.h 
#define MAX_HDISK 8
#define BEGIN {
#define END }
#define NUL	 (char) '\0'
#define ESC 0x1B
#define ESC_FLAG 0x1B
#define FALSE	 (char)(1==0)
#define TRUE	 !(FALSE)
#define CR	 '\x0d'
#define LF	 '\x0a'
#define BACKSPACE 0x66
#define PERCENT 0x06
// partition types
#define DOS12 0x01
#define DOS16 0x04
#define DOSNEW 0x06
#define EXTENDED 0x05
#define BAD_BLOCK 0xFF
#define NOFORMAT 0x00 // ?
#define XENIX1 0x02
#define XENIX2 0x03
#define PCIX 0x75
#define HPFS 0x07
#define NOVELL 0x51
#define CPM 0x52

#define FAT12 "FAT12   "
#define FAT16 "FAT16   "
#define NOVOLUME "UNKNOWN " // ?

#define FAT16_SIZE 65,536 // FAT16 for sector sizes larger than 65536 ??
#define BYTES_PER_SECTOR 512

#define SEA 0x43 // "C"
#define INSTALLATION_CHECK 0 // ?
#define NETWORK 0 // ?
#define SERVER_CHECK 0 // ?
#define NETWORK_IOCTL 0 // ?
#define ERR_LEVEL_0 0 // ?
#define ERR_LEVEL_1 1 // ?
#define ERR_LEVEL_2 2 //?
#define c(x) (char)x
#define uc(x) (unsigned char)x
#define u(x) (unsigned)x
#define f(x) (float)x
#define ul(x) (unsigned long)x

#define HIWHITE_ON_BLACK 6
#define HIWHITE_ON_BLUE 3
#define GRAY_ON_BLACK 8 
#define WHITE_ON_BLUE 9

#define PRIMARY 1
#define MAX_PART_SIZE 32
#define NOT_FOUND 0
#define ACTIVE 0x80
#define INVALID 0x00
#define CURRENT_VIDEO_STATE 1
#define VIDEO 1
#define Color80_25 1
#define MONO80_25 1
#define MONO80_25A 1
#define BW80_25 1
#define SET_MODE 1
#define SET_ACTIVE_DISPLAY_PAGE 1
#define SET_PAGE 1
#define CURRENT_VIDEO_ATTRIBUTE 1

#define WRITE_DISK 1
#define SYSTEM_FILE_SECTORS 1
#define VERIFY_DISK 1
#define DISK_INFO 1
#define DISK 1
#define READ_DISK 1
#define CARRY_FLAG 1

#define NOERROR 0
#define SEMICOLON ':'

#define DOS_MAX 8
#define ONE_MEG 1024
#define CAPCHAR 1
#define INT21 24
#define CAP_YN 1
#define NO_GOOD 0
#define GENERIC_IOCTL 10
#define ZERO '0'
#define GET_MEDIA_ID 9
#define FILE_NAME 2
#define FIND_FIRST_MATCH 1
#define VOL_LABEL 0x20
#define PERIOD '.'
#define SPECIAL_FUNCTION 0x65

#define DELETED 1

typedef unsigned XFLOAT;
typedef unsigned char FLAG;

struct entry
{
    unsigned char    boot_ind;
	unsigned char    start_head;
	unsigned char    start_sector;
	unsigned         start_cyl;
	unsigned char    sys_id;
	unsigned char    end_head;
	unsigned char    end_sector;
	unsigned         end_cyl;
	unsigned long    rel_sec;
	unsigned long    num_sec;
	float            mbytes_used;
	unsigned         percent_used;
	unsigned char    changed;
	char             drive_letter;
	char             system[8];
	char             vol_label[8];
};

struct freespace
{
	unsigned    space;
    unsigned    start;
    unsigned    end;
    float       mbytes_unused;
    unsigned    percent_unused; 
};

struct sublistx {
	 unsigned char size;	       /* sublist size			       */
	 unsigned char reserved;       /* reserved for future growth	       */
	 unsigned far *value;	       /* pointer to replaceable parm	       */
	 unsigned char id;	       /* type of replaceable parm	       */
	 unsigned char flags;	       /* how parm is to be displayed	       */
	 unsigned char max_width;      /* max width of replaceable field       */
	 unsigned char min_width;      /* min width of replaceable field       */
	 unsigned char pad_char;       /* pad character for replaceable field  */
	};

struct dx_buffer_ioctl
{
	char file_system[8];
};

struct diskaccess
{
	unsigned char dac_access_flag;
};







