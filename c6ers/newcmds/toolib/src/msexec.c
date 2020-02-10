/*
   EXEC.C: EXEC function with memory swap - Prepare parameters.

   Public domain software by

        Thomas Wagner
        Ferrari electronic GmbH
        Beusselstrasse 27
        D-1000 Berlin 21
        Germany
*/

//#include "compat.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <dos.h>
#include <malloc.h>
#include <direct.h>
#include <sys\types.h>
#include <sys\stat.h>


#define MK_FP(seg,ofs)	((void far *)(((unsigned long)(seg) << 16) | \
			    (unsigned short)(ofs)))

#include <ctype.h>
#include <tools.h>

#define SWAP_FILENAME "$$AAAAAA.AAA" 

/* internal flags for prep_swap */

#define CREAT_TEMP      0x0080
#define DONT_SWAP_ENV   0x4000

/* local variables */

static char l_drive [_MAX_DRIVE], dir [_MAX_DIR];
static char name [_MAX_FNAME], ext [_MAX_EXT];


#ifdef __cplusplus
extern "C" int
#else
extern int _cdecl
#endif
do_spawn (int swapping,     /* swap if non-0 */
          char *xeqfn,      /* file to execute */
          char *cmdtail,    /* command tail string */
          unsigned envlen,  /* environment length */
          char *envp);      /* environment pointer */

#ifdef __cplusplus
extern "C" int
#else
extern int _cdecl
#endif
prep_swap (int method,      /* swap method */
           char *swapfn);   /* swap file name and/or path */


/* Returns TRUE if a file with name 'fn' exists. */

static int exists (char *fn)
{
   struct find_t fb;

   return !_dos_findfirst (fn, _A_RDONLY|_A_HIDDEN|_A_SYSTEM|_A_ARCH, &fb);
}


/* Try '.COM' and '.EXE' on current filename, modify filename if found. */

static int tryext (char *fn)
{
   int i;

   i = strlen (fn);
   strcat (fn, ".COM");
   if (exists (fn))
      return 1;
   fn [i] = 0;
   strcat (fn, ".EXE");
   if (exists (fn))
      return 1;
   fn [i] = 0;
   return 0;
}


/* Try to find the file 'fn' in the current path. Modifies the filename
   accordingly. */

static int findfile (char *fn)
{
   char *path, *penv;
   char *prfx;
   int found;

   if (!fn [0])
      strcpy (fn, getenv ("COMSPEC"));

   _splitpath (fn, l_drive, dir, name, ext);

   if (ext [0])
      found = exists (fn);
   else
      found = tryext (fn);

   if (!found && !dir [0] && !l_drive [0])
      {
      penv = getenv ("PATH");
      if (!penv)
         return 0;
      path = (char *)malloc (strlen (penv) + 1);
      if (path == NULL)
         return 0;

      strcpy (path, penv);
      prfx = strtok (path, ";");

      while (!found && prfx != NULL)
         {
	 _makepath (fn, l_drive, prfx, name, ext);
         if (ext [0])
            found = exists (fn);
         else
            found = tryext (fn);

         prfx = strtok (NULL, ";");
         }
      free (path);
      }
   return found;
}


/*
   tempdir: Set temporary file path.
            Read "TMP/TEMP" environment. If empty or invalid, clear path.
	    If TEMP is l_drive or l_drive+backslash only, return TEMP.
            Otherwise check if given path is a valid directory.
            If so, add a backslash, else clear path.
*/

static void tempdir (char *outfn)
{
   int l;
   char *s;
   struct stat ff;
   union REGS regs;
   struct SREGS segregs;

   *outfn = 0;
   if ((s = getenv ("TMP")) == NULL)
      if ((s = getenv ("TEMP")) == NULL)
         return;

   strcpy (outfn, s);
   l = strlen (outfn);
   if (!l)
      return;

   if (outfn [l - 1] == '\\' || outfn [l - 1] == '/')
      outfn [--l] = 0;

   _splitpath (outfn, l_drive, dir, name, ext);

   if (l_drive [0])
      {
      regs.h.dl = (unsigned char)(toupper (l_drive [0]) - 'A' + 1);
      regs.h.ah = 0x1c;
      intdosx (&regs, &regs, &segregs);
      if (regs.h.al == 0xff)
         {
         *outfn = 0;
         return;
         }
      }

   if (!name [0])   /* No dir name */
      {
      if (dir [0])
         *outfn = 0;
      else
         {
         outfn [l++] = '\\';
         outfn [l] = 0;
         }
      return;
      }

   if (stat (outfn, &ff))
      *outfn = 0;
   /* the following won't work with TC/BC - directories never have the
      write attribute set (don't ask me why). So only check for dir.
   */
#if (0)
   else if (!(ff.st_mode & S_IFDIR) || !(ff.st_mode & S_IWRITE))
#else
   else if (!(ff.st_mode & S_IFDIR))
#endif
      *outfn = 0;
   else
      {
      outfn [l++] = '\\';
      outfn [l] = 0;
      }
}


int do_exec (char *exfn, char *epars, int spwn, unsigned needed, char **envp)
{
   static char swapfn [82];
   static char execfn [82];
   unsigned avail;
   union REGS regs;
   unsigned envlen;
   int rc;
   int idx;
   char **env;
   char *ep, *envptr, *envbuf;
   int swapping;

   strcpy (execfn, exfn);

   /* First, check if the file to execute exists. */

   if (!findfile (execfn))
      return RC_NOFILE;

   /* Now create a copy of the environment if the user wants it. */

   envlen = 0;
   envptr = NULL;

   if (envp != NULL)
      for (env = envp; *env != NULL; env++)
         envlen += strlen (*env) + 1;

   if (envlen)
      {
      /* round up to paragraph, and alloc another paragraph leeway */
      envlen = (envlen + 32) & 0xfff0;
      envbuf = (char *)malloc (envlen);
      if (envbuf == NULL)
         return RC_ENVERR;

      /* align to paragraph */
      envptr = envbuf;
      if (FP_OFF (envptr) & 0x0f)
         envptr += 16 - (FP_OFF (envptr) & 0x0f);
      ep = envptr;

      for (env = envp; *env != NULL; env++)
         {
	 ep = (strcpy (ep, *env), ep + strlen (*env)) + 1;
         }
      *ep = 0;
      }

   if (!spwn)
      swapping = -1;
   else
      {
      /* Determine amount of free memory */

      regs.x.ax = 0x4800;
      regs.x.bx = 0xffff;
      intdos (&regs, &regs);
      avail = regs.x.bx;

      /* No swapping if available memory > needed */

      if (needed < avail)
         swapping = 0;
      else
         {
         /* Swapping necessary, use 'TMP' or 'TEMP' environment variable
           to determine swap file path if defined. */

         swapping = spwn;
         if (spwn & USE_FILE)
            {
            tempdir (swapfn);

	    if (_osmajor >= 3)
               swapping |= CREAT_TEMP;
            else
               {
               strcat (swapfn, SWAP_FILENAME);
               idx = strlen (swapfn) - 1;
               while (exists (swapfn))
                  {
                  if (swapfn [idx] == 'Z')
                     idx--;
                  if (swapfn [idx] == '.')
                     idx--;
                  swapfn [idx]++;
                  }
               }
            }
         }
      }

   /* All set up, ready to go. */

   if (swapping > 0)
      {
      if (!envlen)
         swapping |= DONT_SWAP_ENV;

      rc = prep_swap (swapping, swapfn);
      if (rc < 0)
         return RC_PREPERR | -rc;
      }

   rc = do_spawn (swapping, execfn, epars, envlen, envptr);

   /* Free the environment buffer if it was allocated. */

   if (envlen)
      free (envbuf);

   return rc;
}
