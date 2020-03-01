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
#define DOS12 0x01
#define DOS16 0x06
#define DOS16_SIZE 16 // ?
#define FAT12 0x01
#define FAT16 0x06
#define FAT16_SIZE 16 // ?
#define DOSNEW 0x0B // FAT32 ???
#define EXTENDED 0x05
#define BAD_BLOCK 0xFF
#define BYTES_PER_SECTOR 512
#define SEA 0x43 // "C"
#define INSTALLATION_CHECK 0 // ?
#define NETWORK 0 // ?
#define SERVER_CHECK 0 // ?
#define NOVOLUME 0 // ?
#define NOFORMAT 0 // ?
#define NETWORK_IOCTL 0 // ?
#define ERR_LEVEL_0 0 // ?
#define ERR_LEVEL_1 1 // ?

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











