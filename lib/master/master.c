
#include "cds.h"
#include "ctype.h"
#include "dos.h"
#include "stdio.h"
#include "sysvar.h"
#include "types.h"
#include "master.h"

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
