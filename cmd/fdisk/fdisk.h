//fdisk.h 
#define MAX_HDISK 8
#define FLAG int
#define BEGIN {
#define END }
#define NUL 0x20
#define ESC 0x1B
#define ESC_FLAG 0x1B
#define TRUE 1
#define FALSE 0
#define CR 0x13
#define LF 0x10
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
#define c(x) (char)x
#define uc(x) (unsigned char)x
#define u(x) (unsigned)x
#define f(x) (float)x
#define ul(x) (unsigned long)x

#define HIWHITE_ON_BLACK 6
#define HIWHITE_ON_BLUE 3
#define GRAY_ON_BLACK 8 
#define WHITE_ON_BLUE 9

typedef float XFLOAT;

typedef struct entry
{
    unsigned char boot_ind;
	unsigned char start_head;
	unsigned char start_sector;
	unsigned start_cyl;
	unsigned char sys_id;
	unsigned char end_head;
	unsigned char end_sector;
	unsigned end_cyl;
	unsigned long  rel_sec;
	unsigned long num_sec;
	float mbytes_used;
	unsigned percent_used;
	int changed;
	char drive_letter;
	char file_system[8];
	char system[8];
	char vol_label[8];
} entry;

typedef struct dx_buffer_ioctl
{
	char file_system[8];
} dx_buffer_ioctl;









