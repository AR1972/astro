/*
	COW : Character Oriented Windows

	shadow.h : shadow specifics
*/

#define	diShadowInit		(dmAttrOnly | isaShadow)

#ifdef WINDOW_OVERLAP
#define	daxShadowInit		2
#define	dayShadowInit		1

extern AX daxShadow;
extern AY dayShadow;
#else
#define	daxShadow		2
#define	dayShadow		1
#endif /*WINDOW_OVERLAP*/
