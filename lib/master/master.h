
char  fGetCDS(int, struct CDSType *);
char  fPutCDS(int, struct CDSType *);
void  PutVars(struct sysVarsType *);
void  GetVars(struct sysVarsType *);
int   ffirst(char *, unsigned int, struct findType *);
int   fShared(int);
int   fNet(int);
int   fPhysical(int);
int   fPathErr(char *);
void  rootpath(char *, char *);
int   getdrv(void);
char  *strbscan(char *, char *);
long  GetDPB(int);