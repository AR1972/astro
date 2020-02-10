/*
	COW : Character Oriented Windows

	_scroll.h : scroll bar stuff
*/

/* ptElevator is the physical position of the elevator */

#define PtElevatorSb(pwnd) ((BYTE)((pwnd)->ptElevatorSb & 0xff))

#define SetPtElevatorSb(pwnd, pt) (pwnd)->ptElevatorSb = (pt);
	
#define	dxScrollMin	2
#define	dyScrollMin	2
