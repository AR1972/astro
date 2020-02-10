#ifndef MAPSTR_H
#define MAPSTR_H (1)

// mapstr.h - definitions for text mapping

// basic lookup structure
typedef char *MapAbbr;
typedef int MapValu;
typedef char* (*MapTest) ( char*, char* );

typedef struct {
    MapAbbr szLabel;
    MapValu value;			// use your favorite enum here
    } MapItemRec, *MapItemPtr;

// public routines
extern MapItemPtr mapAbbrStr (
    MapAbbr szArg,
    MapItemPtr pTable,
    MapTest pTest );

#endif // MAPSTR_H
