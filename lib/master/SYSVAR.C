/* return the system variables in sysVars */

#include "types.h"
#include "sysvar.h"
#include <dos.h>

void GetVars(struct sysVarsType *pSVars);
void PutVars(struct sysVarsType *pSVars);

void GetVars(pSVars)
struct sysVarsType *pSVars ;
{
	struct sysVarsType far *vptr ;
	int i ;

	union REGS ir ;
	register union REGS *iregs = &ir ;	/* Used for DOS calls	   */
	struct SREGS syssegs ;

	iregs->h.ah = GETVARS ;			/* Function 0x52	   */
	intdosx(iregs, iregs, &syssegs) ;

	*(long *)(&vptr) = (((long)syssegs.es) << 16)+(iregs->x.bx & 0xffffL) ;

	for (i=0 ; i <= sizeof(*pSVars) ; i++)
		*((char *)pSVars+i) = *((char far *)vptr+i) ;

}




void PutVars(pSVars)
struct sysVarsType *pSVars ;
{
	struct sysVarsType far *vptr ;
	int i ;

	union REGS ir ;
	register union REGS *iregs = &ir ;	/* Used for DOS calls	   */
	struct SREGS syssegs ;

	iregs->h.ah = GETVARS ;			/* Function 0x52	   */
	intdosx(iregs, iregs, &syssegs) ;

	*(long *)(&vptr) = (((long)syssegs.es) << 16)+(iregs->x.bx & 0xffffL) ;

	for (i=0 ; i <= sizeof(*pSVars) ; i++)
		*((char far *)vptr+i) = *((char *)pSVars+i) ;

}
