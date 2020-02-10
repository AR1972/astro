/*
	COW : Character Oriented Windows

	menu2.c : menu extras
*/

#define COW
#include <cow.h>

#include <umenu.h>
#include <uwindow.h>
#include <uevent.h>

#include "menu.h"
#include "_menu.h"

STATIC PMENU	FindMenu(WORD);


PUBLIC VOID FARPUBLIC
EnableMenu(id, fEnable)
/*
  -- Enable/Disable a particular menu
*/
WORD id;
BOOL fEnable;
	{
	StartPublic();
	REGISTER PMENU pmenu;
	
	if ((pmenu = FindMenu(id)) != NULL)
		pmenu->fEnabled = (fEnable != 0);
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
EnableMenuItem(id, fEnable)
/*
  -- Enable/Disable a particular menu item
*/
WORD id;
BOOL fEnable;
	{
	StartPublic();
	REGISTER PMENUITEM pmenuitem;
	
	if ((pmenuitem = FindMenuItem(id)) != NULL)
		pmenuitem->fEnabled = (fEnable != 0);
	StopPublic();
	}



PUBLIC VOID FARPUBLIC
CheckMenuItem(id, fChecked)
/*
  -- check specified menu item
*/
WORD id;
BOOL fChecked;
	{
	StartPublic();
	REGISTER PMENUITEM pmenuitem;
	
	if ((pmenuitem = FindMenuItem(id)) != NULL)
		pmenuitem->fChecked = (fChecked != 0);
	StopPublic();
	}



PUBLIC BOOL FARPUBLIC
FMenuItemChecked(id)
/*
  -- return state of menu item check flag
*/
WORD id;
	{
	StartPublic();
	REGISTER PMENUITEM pmenuitem;
	
	if ((pmenuitem = FindMenuItem(id)) != NULL)
		{
		ReturnPublic(pmenuitem->fChecked, BOOL);
		}
	else
		{
		ReturnPublic(FALSE, BOOL);
		}
	}

STATIC PMENU
FindMenu(id)
WORD id;
/* FindMenu - Find specified menu */
	{
	StartPublic();
	PMENU pmenu = pmenubarCur->rgmenu;
	WORD cmenu = pmenubarCur->cmenu;

	while (cmenu--)
		{
		if (pmenu->idMenu == id) return(pmenu);
		pmenu++;
		}
	return(NULL);
	}

