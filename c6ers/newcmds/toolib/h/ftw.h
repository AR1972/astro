#define FTW_NS	0
#define FTW_DNR	1
#define FTW_D	2
#define FTW_F	3

int ftw(char *, int (*)(char *, struct stat *, int), int);
