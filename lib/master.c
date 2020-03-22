
#include "cds.h"
#include "ctype.h"
#include "dos.h"
#include "stdio.h"
#include "sysvar.h"
#include "jointype.h"

char fGetCDS(int, struct CDSType *);
char fPutCDS(int, struct CDSType *);
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
long GetDPB(int);

char fGetCDS(int n, struct CDSType *T) {
	return (char) 0;
}

char fPutCDS(int n, struct CDSType *T) {
	return (char) 0;
}

void PutVars(struct sysVarsType *T) {
	return;
}

void GetVars(struct sysVarsType *T) {
	return;
}

int fShared(int i) {
	return 0;
}

int fNet(int i) {
	return 0;
}

int fPhysical(int i) {
	return 0;
}

int fPathErr(char *path) {
	return 0;
}

int ffirst(char *c, unsigned int un, struct findType *F) {
	return 0;
}

void rootpath(char *L, char *R) {
	return;
}

int getdrv() {
	return 0;
}

char *strbscan(char *L, char *R) {
	return L;
}

long GetDPB(int i) {
	return (long) i;
}
