#include <stdio.h>
#include "sulib.h"
#include "newexe.h"

#define NE_TYPE(ne) (((BYTE *)&(ne))[0x36])

main (int argc, char ** argv)
{
    printf("%-13s %-8s %-4s %-4s %-4s %s\n\n",
        "File",
        "Name",
        "Flag",
        "Type",
        "Ver",
        "Description" );

    if (argc>1)
        FindWin(argv[1]);
    else
        FindWin("/");
}

char    info[128];
char    modname[128];

FindWin(char *sz)
{
    char    path[128];
    WORD    fExe;
    struct  new_exe  newexe;
    FCB     fcb;
    BOOL    f;
    BOOL    fFAPI;

    if (!DosValidDir(sz))
        *FileName(sz) = 0;

    strcpy(path,sz);
    catpath(path,"*.exe");
    f = DosFindFirst(&fcb,path,ATTR_FILES);

    while (f)
    {
        strcpy(path,sz);
        catpath(path,fcb.szName);

        if (GetExeInfo(path,&newexe,sizeof(newexe),GEI_EXEHDR))
        {
            GetExeInfo(path,&fExe,sizeof(fExe),GEI_FLAGS);
            GetExeInfo(path,info,sizeof(info),GEI_DESCRIPTION);
            GetExeInfo(path,modname,sizeof(modname),GEI_MODNAME);

            if ( (NE_TYPE(newexe) == 0 || NE_TYPE(newexe) == 2) &&
                !(newexe.ne_flags & NENOTP) )
            {
                /*
                 * test for FAPI's that are unknown EXE types.
                 */
                if (NE_TYPE(newexe) == 0 && newexe.ne_expver == 0)
                    GetExeInfo(path,&fFAPI,sizeof(fFAPI),GEI_FAPI);
                else
                    fFAPI = FALSE;

                if (fFAPI)
                    printf("%-13s %-8s %04X %04X FAPI %s\n",
                        fcb.szName,
                        modname,
                        fExe,
                        NE_TYPE(newexe),
                        info );
                else
                    printf("%-13s %-8s %04X %04X %04X %s\n",
                        fcb.szName,
                        modname,
                        fExe,
                        NE_TYPE(newexe),
                        newexe.ne_expver,
                        info );
            }
        }

        f = DosFindNext(&fcb);
    }

    strcpy(path,sz);
    catpath(path,"*.*");

    f = DosFindFirst(&fcb,path,ATTR_DIR);
    while (f)
    {
        if ((fcb.Attr & ATTR_DIR) && fcb.szName[0] != '.')
        {
            strcpy(path,sz);
            catpath(path,fcb.szName);

            FindWin(path);
        }
        f = DosFindNext(&fcb);
    }
}

#ifdef DEBUG

/* FAR _Assert(char*,int);
 *
 * Called as a result of an assertion failure. Will print out an error
 * dialog containing the file and line number of where the assertion failure
 * occured.
 *
 * ENTRY: Only from an assertion failure.
 * EXIT : Fatal Error (exit to dos).
 *
 */
FAR _Assert(char *szFile, int line)
{
   fprintf(stderr,"ASSERT, FILE %s LINE: %d",szFile,line);
   exit(-1);
}
#endif
