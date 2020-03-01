//fdisk.h 
#define MAX_HDISK 8
#define FLAG int
#define XFLOAT int
#define BEGIN {
#define END }
#define NUL NULL
#define ESC 0x1B
#define TRUE 1
#define FALSE 0
#define FAT12 0x01
#define FAT16 0x06
#define DOSNEW 0x0B
#define EXTENDED 0x05
#define BAD_BLOCK 0xFF
#define BYTES_PER_SECTOR 512

struct entry
{
    char boot_ind[1];
	char start_head[1];
	char start_sector[1];
	char start_cyl[1];
	char sys_id[1];
	char end_head[1];
	char end_sector[1];
	char end_cyl[1];
	char rel_sec[1];
	char num_sec[1];
}











