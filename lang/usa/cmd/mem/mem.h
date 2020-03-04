#define MAX_DDRIVER_REP 3
#define TRUE 1
#define FALSE 0
#define GET_UMB_LINK_STATE 1
#define SET_UMB_LINK_STATE 2
#define UNLINK_UMBS 3
#define LINK_UMBS 4
#define MAX_CLDATA_INDEX 5

struct files
{
    char psp_addr;
    long conv_ttl;
    long umb_ttl;
};

struct umbs
{
    long umb_free;
    long umb_ttl;
    long umb_addr;
    long umb_large;
};

struct mem_classif
{
    unsigned long conv_ttl;
    unsigned long conv_free;
    unsigned long umb_ttl;
    unsigned long umb_free;
    unsigned long xms_ttl;
    unsigned long xms_free;
    unsigned long ems_ttl;
	unsigned long ems_free;
    unsigned long int_15h;
	unsigned long conv_large;
    unsigned int hma;
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

struct mem_classif mem_table;

struct sublistx
{
    unsigned far * value;
    char size; 
    char reserved;      
    char id;            
    char flags;
    char max_width;
    char min_width;
    char pad_char; 
};

char p_not_in_key[] = "p_not_in_key";
char p_too_many[] = "p_too_many";
char p_op_missing[] = "p_op_missing";
char p_not_in_sw[] = "p_not_in_sw";

void init_data ();
void Parse_Message (int, char far *);

