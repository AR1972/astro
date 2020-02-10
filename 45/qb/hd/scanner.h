#undef SCANNER_H
#define SCANNER_H ON

/*======================================================================*/
/* Scanner state definitions															*/
#define SS_EXECUTE (ushort)0
#define SS_PARSE (ushort)1
#define SS_RUDE (ushort)2
#define SS_SUBRUDE (ushort)3

/*======================================================================*/
/* Scanner external interface.						 								*/ 

ushort	FAR SsScan(void);
ushort	FAR SsDescan(void);
void	NEAR SsRudeScan(ushort);

/*======================================================================*/
/* Non-RELEASE flag, allows for scanning from SS_RUDE to SS_PARSE			*/
