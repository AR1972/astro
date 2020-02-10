/* TS = none */
/*
**  LZCOMP.C
*/

#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include <io.h>
#include <fcntl.h>
#include <stdio.h>

#include "..\sutkcomp.h"

#ifdef OS2SU
#include <doscalls.h>
#endif

#include "lz.h"



/*
**  void  LZInitTree(void)
**
**  For i = 0 to cbBufMax - 1, rson[i] and lson[i] will be the right and
**  left children of node i.  These nodes need not be initialized.
**  Also, dad[i] is the parent of node i.  These are initialized to
**  nilND (= N), which stands for 'not used.'
**  For i = 0 to 255, rson[cbBufMax + i + 1] is the root of the tree
**  for strings that begin with character i.  These are initialized
**  to nilND.  Note there are 256 trees.
*/
void LZInitTree(void)
{
    USHORT  us;

    for (us = 0; us < cbBufMax; us++) 
        {
        rgND[us].ibRingBuf = us;
        rgND[us].pNDpar = &nilND;
        }

    for (us = 0; us < 256; us++)
        rgRoot[us].pNDright = &nilND;
}


/*
**  void  LZDeleteNode(SHORT iND)
**
**  Deletes node p from tree.
*/
void LZDeleteNode(SHORT iND)
{
    register ND *pNDdel = &rgND[iND];
    register ND *pND;
        
    if (pNDdel->pNDpar == &nilND)
        return;                              /* not in tree */

        // if the node is a leaf, the insert ND is easy
    if (pNDdel->pNDright == &nilND)
        pND = pNDdel->pNDleft;
    else if (pNDdel->pNDleft == &nilND)
        pND = pNDdel->pNDright;
    else                      // node to be deleted is an interior node
        {
        pND = pNDdel->pNDleft;

        if (pND->pNDright != &nilND)
            {
            do  {
                pND = pND->pNDright;
                } while (pND->pNDright != &nilND);

            pND->pNDpar->pNDright = pND->pNDleft;
            pND->pNDleft->pNDpar = pND->pNDpar;

            pND->pNDleft = pNDdel->pNDleft;

            pNDdel->pNDleft->pNDpar = pND;
            }
        pND->pNDright = pNDdel->pNDright;
        pNDdel->pNDright->pNDpar = pND;
        }
    pND->pNDpar = pNDdel->pNDpar;

        // set the rigth/left pointer to parent node to new current
    if (pNDdel->pNDpar->pNDright == pNDdel)
        pNDdel->pNDpar->pNDright = pND;
    else
        pNDdel->pNDpar->pNDleft = pND;

    pNDdel->pNDpar = &nilND;
}


/*
** Inserts string of length cbStrMax, ringBuf[r..r+cbStrMax-1], into one of the
** trees (rgRoot[*iString]'th tree) and returns the longest-match position
** and length via the global variables iMatchCur and cbMatchCur.
** If cbMatchCur = cbStrMax, then removes the old node in favor of the new
** one, because the old one will be deleted sooner.
**
** There is a one to one relationship with the i'th position in the ringBuf
** and the i'th position in the rgND.
*/
void LZInsertNode(SHORT iString)
{
    ND *     pND;
    ND  *    pNDNew;
    SHORT    fComp;
    SHORT    cbMatchND;
    BYTE far *  pKey;

    pKey =   &ringBuf[iString];
    pND =    &rgRoot[*pKey];    // start with tree index by first char in string
    pNDNew = &rgND[iString];

    pNDNew->pNDleft = pNDNew->pNDright = &nilND;
    cbMatchCur = 0;
    goto first;

    do {
          // Follow the tree down to the leaves depending on the result
          // of the last string compare.  When you come the a leaf in the
          // tree, you are done and insert the node.
        if (fComp >= 0)
            {
first:
            if (pND->pNDright != &nilND)
                pND = pND->pNDright;
            else
                {
                pND->pNDright = pNDNew;
                pNDNew->pNDpar = pND;
                return;
                }
            }
        else
            {
            if (pND->pNDleft != &nilND)
                pND = pND->pNDleft;
            else
                {
                pND->pNDleft = pNDNew;
                pNDNew->pNDpar = pND;
                return;
                }
            }

          // compare the string at the current node with the string
          // that we are looking for.
        for (cbMatchND = 1; (USHORT)cbMatchND < cbStrMax; cbMatchND++)
            if ((fComp =pKey[cbMatchND]-ringBuf[pND->ibRingBuf+cbMatchND]) != 0)
                break;

          // if the length of the matched string is greater then the
          // current, make the iMatchCur point the pND
        if ((USHORT)cbMatchND > cbMatchCur)
            {
            /* iMatchCur = (ND far *)pND - (ND far *)rgND; */
            unsigned long ul = (unsigned long)((BYTE far *)pND);

            ul -= (unsigned long)((BYTE far *)rgND);
            ul = ul / sizeof(ND);
            if (ul > 0x0000FFFF)
                printf("Assert - Overflow in iMatchCur assignment!\n");
            iMatchCur = (USHORT)ul;

            cbMatchCur = (USHORT)cbMatchND;
            }

          // Search for strings while a less then maxium length string
          // is found
        } while (cbMatchCur < cbStrMax);

      // replace an older ND with the new node in the tree,
      // by replacing the current pND with the new node pNDNew
    pNDNew->pNDleft = pND->pNDleft;
    pND->pNDleft->pNDpar = pNDNew;
    pNDNew->pNDright = pND->pNDright;
    pND->pNDright->pNDpar = pNDNew;

      // insert into left/right side of parent
    pNDNew->pNDpar = pND->pNDpar;

    if (pND->pNDpar->pNDright == pND)
        pND->pNDpar->pNDright = pNDNew;
    else
        pND->pNDpar->pNDleft = pNDNew;

    pND->pNDpar = &nilND;        /* remove old node */
}
