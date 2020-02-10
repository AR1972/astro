/*
	COW : Character Oriented Windows

	handle.h : Handle information
*/

#define HE_SWAPPED      0x80
#define HE_DISCARDED    0x40
#define HE_SHAREALL     0x20
#define HE_SHARE        0x10
#define HE_DISCARDABLE  0x0F

#define HE_FREEHANDLE   0xFFFF

typedef struct
    {
    WORD    he_address;
    BYTE    he_flags;
    BYTE    he_count;
    } HANDLEENTRY;
typedef HANDLEENTRY *PHANDLEENTRY;
typedef HANDLEENTRY FAR *LPHANDLEENTRY;

typedef struct
    {
    WORD	ht_count;
    HANDLEENTRY ht_entry[ 1 ];
    } HANDLETABLE;
typedef HANDLETABLE *PHANDLETABLE;
typedef HANDLETABLE FAR *LPHANDLETABLE;

typedef struct
    {
    WORD    he_link;
    WORD    he_free;
    } FREEHANDLEENTRY;
typedef FREEHANDLEENTRY *PFREEHANDLEENTRY;
typedef FREEHANDLEENTRY FAR *LPFREEHANDLEENTRY;
