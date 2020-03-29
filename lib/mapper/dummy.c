#include "doscalls.h"
#include "subcalls.h"

////////////////////////// doscalls.h

void far pascal SET_INT24_VECTOR(unsigned vect){
    return;
}

//void far pascal DOSEXIT(unsigned a, unsigned b){
//    return;
//}

unsigned far pascal DOSSETFSINFO(unsigned a, unsigned b, char far *c, unsigned d ){
    return (unsigned)0;
}

//unsigned far pascal DOSCLOSE(unsigned a){
//    return (unsigned)0;
//}

unsigned far pascal DOSQCURDISK(unsigned far *a, unsigned long far *b){
    return (unsigned)0;
}

unsigned far pascal DOSERROR(unsigned a){
    return (unsigned)0;
}

//unsigned far pascal DOSWRITE(unsigned a, char far *b, unsigned c, unsigned far *d){
//    return (unsigned)0;
//}

unsigned far pascal DOSFINDCLOSE(unsigned a){
    return(unsigned)0;
}

unsigned far pascal DOSSETSIGHANDLER(void (far pascal * a)(),unsigned long far *b, unsigned far *c,unsigned d, unsigned e){
    return (unsigned)0;
}

//unsigned far pascal DOSFINDFIRST(char far *a, unsigned far *b, unsigned c, struct FileFindBuf far *d, unsigned e, unsigned far *f, unsigned long g){
//    return (unsigned)0;
//}

//unsigned far pascal DOSDELETE(char far *a, unsigned long b){
//    return (unsigned)0;
//}

unsigned far pascal DOSQFSINFO(unsigned a, unsigned b, char far *c, unsigned d){
    return (unsigned)0;
}

//unsigned far pascal DOSSETFILEMODE(char far *a, unsigned b, unsigned long c){
//    return (unsigned)0;
//}

//unsigned far pascal DOSOPEN(char far *a, unsigned far *b, unsigned far *c, unsigned long d,
//                            unsigned e, unsigned f, unsigned g, unsigned long h){
//    return (unsigned)0;
//}

unsigned far pascal DOSSELECTDISK(unsigned a){
    return (unsigned)0;
}

unsigned far pascal DOSGETCTRYINFO(unsigned a, struct countrycode far *b, char far *c, unsigned far *d){
    return (unsigned)0;
}

//unsigned far pascal DOSQCURDIR(unsigned a, char far *b, unsigned far *c){
//    return (unsigned)0;
//}

//unsigned far pascal DOSCHGFILEPTR(unsigned a, long b, unsigned c, unsigned long far *d){
//    return (unsigned)0;
//}

//unsigned far pascal DOSQFILEMODE(char far *a, unsigned far *b, unsigned long d){
//    return (unsigned)0;
//}

//unsigned far pascal DOSFINDNEXT(unsigned a, struct FileFindBuf far *b, unsigned c, unsigned far *d ){
//    return (unsigned)0;
//}

//unsigned far pascal DOSSETFILEINFO(unsigned a, unsigned b, char far *c, unsigned d){
//    return (unsigned)0;
//}

//unsigned far pascal DOSALLOCSEG(unsigned a, unsigned far *b, unsigned c){
//    return (unsigned)0;
//}

unsigned far pascal DOSQFILEINFO(unsigned a, unsigned b, char far *c, unsigned d){
    return (unsigned)0;
}

//unsigned far pascal DOSBEEP(unsigned a, unsigned b){
//    return (unsigned)0;
//}

/////////////////subcalls.h

//unsigned far pascal VIOSCROLLUP(unsigned a, unsigned b, unsigned c, unsigned d,
//                                unsigned e, char far *f, unsigned g){
//    return (unsigned)0;
//}

//unsigned far pascal VIOSETCURPOS(unsigned a, unsigned b, unsigned c){
//    return (unsigned)0;
//}

//unsigned far pascal VIOWRTCHARSTRATT(char far *a, unsigned b, unsigned c, unsigned d, char far *e, unsigned f){
//    return (unsigned)0;
//}

