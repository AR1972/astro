#include "defines.h"
#include "messages.h"

char *SlowNotValidWithBounceAlertText =   "     You can not have the SlowKeys function turned on with an Acceptance Delay "
                                          "value and use the BounceKeys function at the same time.  Please note that the "
                                          "'SlowKeys On' option will be set to 'Off.'\n\n     If you want to set an Acceptance "
                                          "Delay for SlowKeys you must first select 'BounceKeys' from the Adjust menu and change "
                                          "the 'Bounce Time' option to 'Off.'";
char *BounceNotValidWithSlowAlertText =   "     You can not use the BounceKeys function when the SlowKeys function is "
                                          "turned on and Acceptance Delay has a value.  Please note that the BounceKeys function "
                                          "will be set to 'Off.'\n\n     If you want to use the BounceKeys function you must "
                                          "select 'SlowKeys' from the Adjust menu, set the 'SlowKeys On' option to 'On', "
                                          "and set 'Acceptance Delay' to 'Off.'";
char *NoSlowWithBounceAlertText        =  "     Sorry, you can not use SlowKeys and BounceKeys at the same time.";
char *NoNoteWithFlashAlertText			=  "     Sorry, you can not use the visual note and the screen flash at the same time.";
char *SerialKeysNotLoadedAlertText     =  "     You initially installed AccessDOS without enabling the SerialKeys feature.  It "
                                          "is not possible to enable SerialKeys when AccessDOS is initially loaded without it.  "
                                          "Please note that SerialKeys has been turned off.\n\n     If you want to use the "
                                          "SerialKeys function you must reboot the computer, re-run AccessDOS, and turn "
                                          "SerialKeys on before installing AccessDOS.";
char *CannotChangeCommPortAlertText    =  "     It is not possible to change the comm port once SerialKeys has been loaded.  "
                                          "Please note that the comm port has been changed back to it's original value.\n\n     "
                                          "If you want to change the comm port you must reboot the computer, re-run "
                                          "AccessDOS, select the desired comm port, and turn SerialKeys on before installing "
                                          "AccessDOS.";
char *NoMouseFoundAlertText            =  "    In order to use the MouseKeys function there must be a mouse attached to the "
                                          "computer.  AccessDOS did not find a supported mouse attached to the computer.  Please "
                                          "note that the MouseKeys function has been set to 'Off.'\n\n     If you want to use "
                                          "MouseKeys function, you must restart the computer with a mouse attached.";
char *SerKeysMouseConflictAlertText    =  "     You can not have a SerialKeys and your serial mouse attached to the same comm "
                                          "port.  Please note that the comm port for SerialKeys has been set to avoid conflicts "
                                          "with your serial mouse.\n\n  If you want to use both SerialKeys and MouseKeys, you "
                                          "must attach the mouse and the aid to different comm ports and tell AccessDOS which "
                                          "comm port the aid is attached to in the SerialKeys menu.  If you only have one comm "
                                          "port, you can not use both SerialKeys and a Serial mouse.";
char *NoSerialPortAvailableAlertText   =  "     There are no serial ports available for SerialKeys to use.  There are no serial "
                                          "ports in your computer and SerialKeys can not be used without one.  Please note that "
                                          "the SerialKeys function has been turned to 'Off.'";
char *OneSerialPortWithMouseAlertText  =  "     There are no serial ports available for SerialKeys to use.  AccessDOS has "
                                          "detected only one serial port in this computer and it is currently in use by a "
                                          "serial mouse.  Please note that the SerialKeys function has been set to 'Off.'\n\n"
                                          "     If you want to use the SerialKeys function, you will need to turn off the "
                                          "computer, disconnect the mouse, attach the communication aid to the serial port, "
                                          "and re-boot the computer.  Of course, you will not be able to use the MouseKeys "
                                          "feature in this case.";
char *NoCommTwoAlertText               =  "     You have elected to use comm port 2 for the SerialKeys function, but there is "
                                          "only one serial port in the computer.  Please note that the 'Serial Port' option has "
                                          "been set to 'COM1.'";
char *ComputerNotFoundAlertText 			=  "     AccessDOS was unable to determine what type of computer you are using.  Please "
														"select 'Miscellaneous' from the Adjust menu and choose the type of computer that most "
														"closely matches the one you are using.\n\n     If you don't choose a computer type, "
														"AccessDOS will assume you are using an IBM PC/XT with a 84 key keyboard.  This may "
														"limit the number of functions available for your use.";
char *LoadSuccessfulAlertText       	=  "\n\n AccessDOS settings were loaded successfully.";
char *LoadFailedAlertText           	=  "\nAn error has occured trying to load your saved settings.  AccessDOS "
														"requires the file ADOS.CFG to be present in the same directory that contains "
														"ADOS.COM.\n\nPlease note that AccessDOS will continue without your saved settings.";
char *SaveSuccessfulAlertText				=  "\n\n         AccessDOS settings were saved successfully.";
char *SaveFailedAlertText					=  "\n\n An error has occured trying to save AccessDOS settings.";



HELP_SCREEN_RECORD helpTextSaveParameters[] =
{
	"                                                              Page 1 of 1\n"
	"          ^SAVING AND AUTOMATICALLY STARTING UP WITH YOUR "
	"SETTINGS^\n\n     Whenever you initially run AccessDOS by just typing:  ^ADOS^\nAccessDOS will start up with the preset "
	"or default settings which were originally set at the Trace Center.  You are then brought into this program where you can "
	"change the settings to meet your particular needs.\n\n     Most users however would like to be able to save their "
	"settings so that they can use the same settings each time they start AccessDOS.  This can be done by saving your settings "
	"and then asking AccessDOS to use the saved settings when it starts up.  To do this you should:\n     1)  Use the ADJUST "
	"menu to set AccessDOS to meet your needs.\n     2)  Select SAVE SETTINGS... from the FILE menu. (saves them to disk)\n"
	"Thereafter if you ^initially^ start AccessDOS by typing:  ^ADOS /A^   or by putting   ^ADOS /A^   in your AUTOEXEC.BAT "
	"file, AccessDOS will start up with your saved settings."
	"\n                                 - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextInstallAccess[] =
{
	"                                                              Page 1 of 1\n                                ^INSTALL/RUN^"
	"\n\n     Choosing INSTALL/RUN from the FILE menu will cause AccessDOS to leave this setup program and install itself in "
	"your computer.  Once AccessDOS is installed, it will work with most of your other software programs.\n\n     If you are "
	"using SerialKeys, AccessDOS will use 25KB of your computer's memory.  If you are not using SerialKeys, it will use 10KB."
	"\n\n       You can use special key combinations on the keyboard to turn the various functions of AccessDOS on and off "
	"(see manual).  You can also turn functions on or off, and change their settings, by rerunning AccessDOS. (just type:  "
	"ADOS).\n\n    If you want to get AccessDOS completely out of your computer's memory, just reboot (restart) your computer."
	"\n                                 - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextCancelAccess[] =
{
	"                                                              Page 1 of 1\n\n                              ^CANCEL/QUIT^"
	"\n\n     Choosing CANCEL/QUIT from the FILE menu will cause AccessDOS to leave this setup program WITHOUT installing "
	"AccessDOS in your computer.\n\nUse CANCEL/QUIT if you:\n   - Want to discard any settings you just made while in the "
	"setup program\n   - Want to exit the Setup Program without installing AccessDOS.\n\nCANCEL/QUIT will NOT unload AccessDOS "
	"if you already have it installed.  If you want to unload AccessDOS you should reboot (restart) your computer.\n(NOTE: If "
	"you put ADOS in your AUTOEXEC.BAT file then see the manual for details on how to restart your computer without installing "
	"AccessDOS.)\n\n"
	"\n                                 - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextMenu[] =
{
	"                                                              Page 1 of 4\n    All of the adjustments for AccessDOS as "
	"well as the help screens can be reached by using the menus at the top of the screen.  You can move about in the menus by "
	"using the arrow keys on your keyboard.\n\n   -  Use the RIGHT and LEFT arrows to move between menus.\n   -  Use the UP "
	"and DOWN arrows to move up and down within a menu.\n   -  Use the ENTER key to select an item from the menu.\n\nSHORTCUT"
	"\n   You can also jump directly to an item in a menu (instead of using the UP and DOWN keys) by typing the hot key "
	"for that menu item.  The hot key for each menu item is the character that is a different color from the rest (on "
	"a black and white LCD display it would be Capitalized).\n     For example:    E would be the hot key for "
	"sErialkeys.\n                     M would be the hot key for Mousekeys.\n"
	"\n                                                                   MORE...",
	FALSE,
	"                                                              Page 2 of 4\n                  ^ Help Screens For "
	"Individual Functions ^\n\n    Help screens are available for each of the Functions in AccessDOS.  You can reach the help "
	"screens in two ways.\n\n     1) You can open the dialog box for the feature (under the\n        ADJUST menu) "
	"and then hit the F1 key.\n\nor   2) You can open up the help screens for any feature directly by going\n        to "
	"the appropriate item in the HELP menu.\n\n\n    All of the information in the Help screens (and more) is included in "
	"the manual that accompanies this program.  For those who find electronic documentation easier to use, there is also an "
	"electronic copy of the manual on the disk with this program.  "
	"\n                                                                   MORE...",
	FALSE,
	"             ^ IF YOU ARE HAVING TROUBLE USING THE MENUS  ^     Page 3 of 4\n\nPROBLEM 1:  I use the up and down "
	"arrows but it doesn't seem to have any\n            effect (except that the message at the bottom of the screen"
	"\n            changes).\n\nSOLUTION: AccessDOS tries to figure out what kind of screen you are using and then use a "
	"compatible color for highlighting.  Sometimes a computer will fool AccessDOS and it will try to highlight the characters "
	"in a way that is invisible on your computer.  (Which means you ARE moving up and down the menus but you won't be able to "
	"see it.)  To fix this problem you should tell AccessDOS what type of display you have by typing a special character after "
	"its name when you start AccessDOS.\n\n   If you are using a computer with an LCD display type:   ADOS /L\n   If you are "
	"using a MONOCHROME display type:             ADOS /M\n   If you are using a COLOR display type:                  ADOS /C"
	"\n                                                                   MORE...",
	FALSE,
	"PROBLEM 2:  I can move around on the menus just fine using the arrow keys\n            but it just seems to skip over "
	"MouseKeys (Or SerialKeys) on\n            the menus. (The hot key for this item won't work either.)\n\nSOLUTION: If "
	"AccessDOS skips over a menu item it means that that item is not available to you for some reason. (It will also be dimmer "
	"on most displays - or in parentheses on an LCD display.)\n\n   If it skips over SerialKeys: It means that you don't "
	"have a serial port available.  Maybe your computer doesn't have one installed or perhaps the only port available is used "
	"up by a serial mouse or built in modem.\n\n   If it skips over MouseKeys: It means that AccessDOS can't find a mouse "
	"driver.  Perhaps your mouse driver wasn't installed yet; you didn't have a mouse connected (and the mouse driver didn't "
	"load); or you are using an incompatible mouse (see manual or MouseKeys Help for more information)."
	"\n\n                                - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextGeneral[] =
{
	"\n\n\n\n                   ^TABLE OF CONTENTS  for  GENERAL HELP^\n\n\n        Page 1 - General Information\n             2 "
	"- Setting Up AccessDOS to Meet Your Needs\n             3 - Making AccessDOS load Automatically at StartUp\n             "
	"4 - Memory Considerations\n             5 - AccessDOS Command Line Switches\n",
	FALSE,
	"                                                               Page 1 of 5\n                            ^General Information^"
	"\n     AccessDOS is a set of DOS extensions which provide additional flexibility in the user-interface for DOS, and which "
	"are useful to individuals both with and without disabilities.  The extensions in this package allow the user to control "
	"the key repeat, to adjust the way the keys on the keyboard respond when pressed, to make all modifier keys into locking "
	"keys (something like the caps lock key), to control the mouse cursor with precision from the keyboard, and to obtain a "
	"visual indication of beeps or other sounds made by the computer.\n\n     AccessDOS consists of two files on your hard "
	"disk, labelled ADOS.COM and ADOS.OVL.  A third file labeled ADOS.CFG is created by AccessDOS to hold user settings.  "
	"A copy of the instruction manual is included on the disk as a text file labeled ADOS.TXT.  This file can be read or "
	"printed using most any word processor, or by using the PRINT command in DOS.  "
	"\n\n                                                              MORE...",
	FALSE,
	"                                                               Page 2 of 5\n                  ^Setting Up AccessDOS to "
	"Meet Your Needs^\n\n1)  Adjust AccessDOS\n     - Use the items you will find under the ADJUST menu to adjust\n       "
	"AccessDOS to meet your individual needs.\n\n2)  Save Your Settings.\n     - If you want to save these settings so "
	"that they can be used in\n       the future then select SAVE SETTINGS from the FILE menu.\n\n3)  Exit this Setup Program"
	"\n     - When you are done you can exit this program 2 ways.\n          1) select INSTALL/RUN from the FILE menu, or"
	"\n          2) select CANCEL/QUIT from the FILE menu.\n\n"
	"\n                                                              MORE...",
	FALSE,
	"                                                               Page 3 of 5\n\n              ^Making AccessDOS load "
	"Automatically at StartUp^\n\n     In many cases it will be desirable to have AccessDOS automatically load "
	"itself each time you restart or reboot the computer.  Individual users may need it in order to use their computers.  In "
	"shared-use computers, AccessDOS can be running in the background and be inactive until a user with a disability comes "
	"along.  Then a few taps on the proper keys will allow the disabled user to access the functions without having to reboot "
	"the computer.\n\n     To make AccessDOS load automatically each time the computer is restarted you should insert the "
	"program name (ADOS) along with the command line switch /A in your AUTOEXEC.BAT file.  (e.g.  ADOS /A)\n"
   "\n                                                               MORE...",
	FALSE,
	"                                                               Page 4 of 5\n\n                           ^Memory "
	"Considerations^\n\n     AccessDOS is a TSR (terminate and stay resident) program.  That means that a small piece of it "
	"remains in your computer's memory after you install it, so that it can work in conjunction with your other software "
	"programs.\n\n     When loading AccessDOS it takes 150 KB of memory to hold both AccessDOS (ADOS.COM) and its overlay "
	"(ADOS.OVL).  After AccessDOS is installed, it occupies:\n           ÷ 10 KB  if you DON'T use "
	"SerialKeys (most people don't)\n           ÷ 25 KB  if you DO use SerialKeys\n\n"
   "\n                                                               MORE...",
	FALSE,
	"                                                               Page 5 of 5\n                      ^AccessDOS Command Line "
	"Switches^\n\n     There are two major ways to run AccessDOS\n        1) you can type ADOS while you are at the DOS prompt\n"
	"        2) you can put ADOS into your AUTOEXEC.BAT file.\n\n     In either case there are loading options that you can "
	"enable by including any of the following command line switches:\n    /A = Automatic install with your settings or option "
	"to use the menu\n         without your saved settings.\n    /X = Automatic install with your settings and no option to use "
	"menu.\n    /C = Use Color display.\n    /M = Use Monochrome (with highlight) display.\n    /L = Use LCD display.\n    /? "
	"or /H = Show this list of command line switches.\n"
	"\n                                - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextSticky[] =
{
   "                                                              Page 1 of 3\n\n                                ^StickyKeys^"
   "\n\n   StickyKeys is a feature that allows people who must type with 1 finger, a mouthstick or a headstick to be able "
   "to use modifier keys (SHIFT, CTRL, and ALT keys).  With StickyKeys turned on, a "
   "person can press a modifier key and then another key in sequence rather than at the same time, to get shifted "
   "(modified) characters.\n\n   To turn StickyKeys on, tap either shift key five times (without disturbing the mouse).  "
   "You will hear an 'up-siren' tone. To turn off StickyKeys, tap a shift key five times (you will hear a 'down-"
   "siren').\n\n\n                                                               MORE...",
   FALSE,
   "                                                              Page 2 of 3\n                             ^Using "
   "StickyKeys^\n\n   StickyKeys operates in two modes: key latching mode and key locking mode.\n\n   Tapping ONCE on a "
	"modifier key causes it to go into LATCHED mode.  A short low beep - high beep will be heard.  As soon as the next "
	"non-modifier key is pressed the modifier key(s) will be released.\n\n    Tapping TWICE in "
   "succession on a modifier key will put it into LOCKED mode.  You will hear a short low - high beep after the first tap and a "
   "single high beep after the second tap.  Once a modifier key is LOCKED it will stay 'locked down' until that modifier "
   "key is hit a third time.\n\n    Any and all of the modifier keys (SHIFT, CTRL, and ALT) can be "
   "latched or locked in combination.\n                                                               MORE...",
   FALSE,
   "                                                              Page 3 of 3\n                             ^StickyKey "
   "Options^\n\n      For shared or public computers there is an optional\n'Two-Key Auto Shutoff' feature to keep non-"
   "disabled users from being confused if StickyKeys is accidentally left on.  Whenever an able-bodied typist uses "
   "a keyboard, they will hold the shift key down and strike another key simultaneously.  If the Two-Key shutoff feature "
   "is enabled, StickyKeys will detect that two keys are down simultaneously and automatically turn the StickyKeys "
   "feature off.\n\n     Some people do not like to have keyboard sounds while others find them useful.  The ability "
   "to turn the different feedback sounds on or off is therefore provided.\n\n     Finally, it is possible to "
   "disable the LOCKED mode of StickyKeys described above if it is not desired.\n                                - END -",
   TRUE
};

HELP_SCREEN_RECORD helpTextMouse[] =
{
	"\n\n\n\n                    ^TABLE OF CONTENTS - MOUSEKEYS HELP^\n\n\n        Page 1 - Introduction to MouseKeys"
	"\n             2 - MouseKeys Numeric Keypad Functions   (Summary Chart)\n             3 - How to Operate the Mouse "
	"Buttons\n             4 - Moving the Cursor Around the Screen\n             5 - Using MouseKeys On Keyboards without "
	"Numeric Keypads\n             6 - Using MouseKeys Along with a Standard Mouse\n             7 - Using MouseKeys Without "
	"a Mouse Connected",
	FALSE,
   "                                                              Page 1 of 7\n^                        Introduction to "
	"MouseKeys^\n\n     Some Users do not have the physical control to operate a mouse yet need to access software which "
	"requires a mouse.  MouseKeys provides a means to control the mouse cursor on the screen by using the numeric keypad on "
	"the keyboard.\n\n    You turn MouseKeys on or off by simultaneously pressing the\nleft ALT key, the left SHIFT key and "
	"the NUM LOCK key.  When MouseKeys turns on, you will hear an up-siren.    (If you are using only one finger, a "
	"mouthstick, or a headpointer, the easiest way to activate MouseKeys is to first activate StickyKeys by tapping the SHIFT "
	"key 5 times.  You can then hit the three keys in sequence rather than simultaneously.)\n\n    Once MouseKeys is turned "
	"on, the numeric keypad becomes a mouse control pad.  Each key on the keypad performs a different mouse function.  "
	"These functions are...",
   FALSE,
   "                                                              Page 2 of 7\n\n                    ^MouseKeys Numeric "
	"Keypad Functions^\n\n1 - Moves the mouse down and to the left\n2 - Moves the mouse down\n3 - Moves the mouse down and "
	"to the right\n4 - Moves the mouse to the left\n5 - Clicks the currently active mouse button\n6 - Moves the mouse to the "
	"right\n7 - Moves the mouse up and to the left\n8 - Moves the mouse up\n9 - Moves the mouse up and to the right\n0 - Locks "
	"the currently active mouse button down\n. - Releases the currently active mouse button\n+ - Double-clicks the currently "
	"active mouse button\n"
	"\n                                                               MORE...",
   FALSE,
   "                     ^How to Operate the Mouse Buttons^         Page 3 of 7\n\n     The 5, +, 0, and . keys operate "
	"the buttons on the mouse.  Use the '5' key when you want to click a button once.  To "
   "double-click a button use the '+' key.   When you want to hold a button down while you use the mouse (as for dragging), "
   "press the '0' key to lock the button down and the '.' key to release the button.\n\n"
	"^Which button is the currently active button?^\n   On 101-key keyboards:\n      / makes the left mouse button the "
	"currently active button\n      - makes the right mouse button the currently active button\n      * makes both mouse "
	"buttons the currently active button.\n   On 84-key keyboards:\n      * makes the left mouse button the currently "
	"active button\n      - makes the right mouse button the currently active button.\n      It is not possible to make both "
	"mouse buttons the currently active\n      button on an 84-key keyboard.",
   FALSE,
   "                                                              Page 4 of 7\n                    ^Moving the Cursor "
   "Around the Screen^\n\n   The keys 1, 2, 3, 4, 6, 7, 8, and 9 are used to move the mouse cursor around the screen in the "
   "directions listed above (and suggested by their location on the numeric keypad).\n\n   Pressing the key once will move "
   "the cursor one unit in that direction.  (The definition of a 'unit' varies with different programs and screens.)\n\n"
   "   Holding the key down will cause the mouse cursor to continue to move in the respective direction, slowly at first, "
   "and then increasing to the maximum speed.  The mouse pointer will then continue at maximum speed (units per second) "
   "until the key is released.\n\n   Both the maximum speed of the mouse cursor and the time it takes to get to "
   "maximum speed are adjustable in the MouseKeys ADJUST menu.\n"
   "                                                               MORE...",
   FALSE,
   "                                                              Page 5 of 7\n\n           ^Using MouseKeys on Keyboards "
   "Without Numeric Keypads^\n\n     While MouseKeys is on, you can use the NUM LOCK key to toggle the MouseKeys control "
	"pad back to a numeric keypad.\n\n     This feature is especially useful if you are using a laptop or notebook computer "
	"which doesn't have a separate numeric keypad.  On these keyboards the 'numeric keypad' is usually overlaid on top of "
	"part of the standard keyboard.  You can then use the NUM LOCK key to toggle between having those keys on the standard "
	"keyboard act as the MouseKeys control pad and having them act as the regular keyboard keys.\n"
	"\n\n                                                               MORE...",
   FALSE,
   "                                                              Page 6 of 7\n                  ^Using MouseKeys with "
   "the Standard Mouse^\n\n     MouseKeys will work in conjunction with the standard Microsoft or IBM PS/2 mouse.\n\n"
	"     You can use the standard mouse to move quickly about the screen and then use "
   "MouseKeys to move more precisely (unit by unit) to your final destination.\n\n    Some people cannot use the standard "
   "mouse while simultaneously holding down the mouse button.  You can use MouseKeys to lock down the currently active mouse "
	"button ('0'), move the mouse cursor using MouseKeys or the real mouse, then release the mouse button ('.').\n\n"
	"\n                                                               MORE...",
   FALSE,
	"                                                              Page 7 of 7\n\n                 ^Using MouseKeys without a "
	"Mouse Connected^\n\n     MouseKeys will not work unless there is a Microsoft or IBM PS/2 mouse driver installed.  Most "
	"mouse drivers will not install unless a mouse is connected to the computer.  As a result, you will need to have a mouse "
	"connected in order for MouseKeys to work.\n\n     On an IBM PS/2 computer you can run the program FAKEMOUS.COM in "
	"conjunction with MOUSE.COM to run MouseKeys without connecting a mouse.\n\n     For more information on using MouseKeys "
	"without a mouse connected, please refer to the AccessDOS manual.\n\n\n                                - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextToggle[] =
{
   "                                                              Page 1 of 1\n\n     People with visual impairments may not be "
   "able to see the lights on the keyboard that indicate CAPS LOCK, NUM LOCK and SCROLL LOCK status.  ^ToggleKeys^ provides "
	"a solution to this by emitting a tone whenever the keys are pressed.  When you press one of "
   "these keys and it turns on you will hear a high beep.  When you press a key and it turns off you will hear a low beep.\n\n"
	"     ToggleKeys is turned on by pressing the NUM-LOCK key and holding it down for a period of about 5 seconds.  When "
	"ToggleKeys turns on, you will hear an up-siren.  You turn ToggleKeys off the same way.  Just hold the NUM-LOCK key for 5 "
	"seconds and you will hear a down-siren.\n\n     ToggleKeys will also function with some keyboards which do not have "
	"indicator lights.  The audible low and high beeps can be very useful for all operators when using this style of keyboard.\n"
	"                                - END -",
   TRUE
};

HELP_SCREEN_RECORD helpTextSerial[] =
{
   "                                                              Page 1 of 2\n                                ^SerialKeys^"
   "\n\n     Some people cannot successfully use the standard keyboard even with all of the adaptations in AccessDOS.  Often, "
   "however, they have special communication or control interfaces which they can operate with proficiency.  If they are using "
   "a communication or other interface aid which has a serial port on it and has a programmable vocabulary, they can use "
   "the SerialKeys feature to connect their aid to this computer and use their communication aid instead of the computer's "
   "keyboard and mouse.\n\n     To use SerialKeys they would run a serial cable from their aid to the serial port on this "
   "computer.  They would then program their aid to send ASCII characters to the serial "
   "port which would be processed in a special way by SerialKeys so that they would look just like keystrokes from the standard "
   "keyboard or movements from the mouse.\n                                                               MORE...",
   FALSE,
	"                                                              Page 2 of 2\n                           ^Setting Up SerialKeys^"
	"\n\nThere are two parts to setting up SerialKeys...\n\n1) Making sure that the communication aid and the serial port on the "
	"computer can talk to each other.\n\n2) Programming your communication aid to send the right commands to SerialKeys.\n\n"
	"     Please refer to the owners manual for the communication aid for more information on connecting it to the computer and "
	"refer to the AccessDOS manual for more information on what needs to be programmed into the communication aid.\n"
   "\n\n                                  - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextKeyboard[] =
{
	"\n\n                TABLE OF CONTENTS - Keyboard Response Group\n\n\n        Page 1 - Introduction to RepeatKeys, "
	"SlowKeys & BounceKeys\n             2 - Enabling the Keyboard Response Group\n             3 - Adjusting RepeatKeys "
	"\n             4 - Adjusting SlowKeys\n             5 - Adjusting BounceKeys\n             6 - Enabling the Keyboard "
	"Response Group from this Program\n             7 - Enabling the Keyboard Response Group from the Keyboard\n             "
	"8 - Emergency Enabling of RepeatKeys and SlowKeys...\n             9 - Emergency Enabling... (continued)\n            "
	"10 - Emergency Enabling... (continued)",
	FALSE,
	"Some people would be able to use the standard keyboard if it functioned just a little differently than it usually does.  "
	"The Keyboard Response Group is a group of functions that modify how the individual keys on the keyboard behave or respond "
	"when you press them.\n\n     ^RepeatKeys^ - If the standard key-repeat feature of your keyboard\n                  is too "
	"fast, or you don't want it at all, ^RepeatKeys^\n                  allows you to slow it down or turn it off.\n\n     "
	"^SlowKeys^   - If you bump keys accidentally as you move around on\n                  the keyboard and would like to slow "
	"the keyboard down\n                  so that it would only accept keys after they have been\n                  held down "
	"for awhile, ^SlowKeys^ will do this for you.\n\n     ^BounceKeys^ - If you have a tremor and accidentally type keys twice"
	"\n                  as you press or release them, ^BounceKeys^ can be used\n                  to make the computer ignore "
	"the extra tap on the key.\n                                                               MORE...",
   FALSE,
   "                                                             Page 2 of 10\n\n                   ^Enabling the Keyboard "
   "Response Group^\n\n\n     The 3 functions in the Keyboard Response Group (RepeatKeys, SlowKeys and BounceKeys) are "
   "enabled and disabled together as a group.  This is done to simplify turning the group on and off from the keyboard.\n\n     "
   "Most people, however, are only interested in using one or two of the functions in the group.  This is not a problem.  "
   "You can individually adjust the functions within this group such that when enabled, unwanted functions will have no effect."
   "\n\n\n\n                                                                 MORE...",
   FALSE,
   "                           ^Adjusting RepeatKeys^              Page 3 of 10\n\n     There are two adjustments for the "
   "RepeatKeys function. The first setting, REPEAT RATE, allows you to adjust the speed at which the keys will "
   "repeat when you hold a key down.  The smaller numbers (like 0.25) will make it repeat faster.  ^If you do not want the "
   "keys to repeat at all^, then just set the REPEAT RATE to OFF.\n\n    The second setting, DELAY UNTIL REPEAT, "
   "allows you to set the length of time you must hold a key down before it will begin to repeat.  If you have trouble "
   "releasing a key in time, then set this to a higher setting.\n\nNOTE: There are some programs which have their "
   "own keyboard repeat key adjustment.  In order for RepeatKeys to work with these programs you must first turn "
   "off the repeat key adjustment in the program.\n(For example in Word Perfect, set keyboard speed to 'normal'; in "
   "Microsoft Word, set keyboard speed option to '0'.)"
	"\n                                                               MORE...",
   FALSE,
   "                            ^Adjusting SlowKeys^               Page 4 of 10\n\n   SlowKeys allows you to slow down the "
   "keyboard so that keys must be held down for a while before they will be accepted by the computer.  This is useful for "
   "individuals who could use the keyboard but accidentally bump extra keys when they try to type.  By slowing down the "
   "keyboard, these extra key bumps are ignored and only the keys that are typed and held down are accepted.\n\n"
   "   There are two adjustments for the SlowKeys function.  The first setting (called ACCEPTANCE DELAY) allows you to "
	"adjust the amount of time that you must hold a key down before it will be accepted by the computer. ^If you do not want "
	"SlowKeys^, then just set the ACCEPTANCE DELAY to  OFF.\n\n   The second setting is called KEY CLICK FEEDBACK.  If this "
	"is set to YES then you will hear a click when you press the key and another click when it is accepted.  (If you want "
	"your typing to be completely silent you must turn off this click setting and the click setting under StickyKeys.)  ...",
   FALSE,
   "                           ^Adjusting BounceKeys^              Page 5 of 10\n\n     Individuals who have tremor or "
   "impaired control of their hands may find that they type two or more of the same letter when they try to press a key.  They "
   "bounce on the key either when they are pressing it or when they release it causing the extra characters.  BounceKeys "
   "causes the keyboard to ignore these quick bounces so that only one key is accepted.\n\n    There is only one setting for "
   "BounceKeys, DEBOUNCE TIME. The larger the number the longer you will have to wait after you release a key "
   "before you can type the same key a second time (up to 2 seconds!).  ^If you do not want the BounceKeys^ function, then just "
   "set the DEBOUNCE TIME to OFF.\n\nNOTE 1: If you want to type the same key twice you just need to pause a bit between the "
   "two key presses.  BounceKeys does not prevent you from typing other keys quickly.  (You just can't type the same key "
	"quickly.)\nNOTE 2: You cannot use SlowKeys and BounceKeys at the same time (see\nmanual)."
	"                                                       MORE...",
   FALSE,
   "                                                             Page 6 of 10\n\n          ^Enabling the Keyboard Response "
   "Group from this Program^\n\n\n     Once you have set up the desired values under each of the Keyboard Response "
   "functions (RepeatKeys, SlowKeys and BounceKeys) you need to enable the Keyboard Response group.\n\nFROM THE ADJUST MENU:\n     "
   "Select 'KEYBOARD RESPONSE...' from the ADJUST menu.  Then select\n     YES for the GROUP ENABLE question at the top of "
	"the box.\n\n\n\n\n                                                               MORE...",
   FALSE,
   "                                                             Page 7 of 10\n          ^Enabling the Keyboard Response "
   "Group from the Keyboard^\n\n     Sometimes you may want to enable or disable these keyboard functions from the keyboard "
   "without having to run this program.  To do this just hold the right shift key down for 8 seconds.  After about 4 "
   "seconds you will hear 3 short warning beeps.  (These are provided just in case someone is accidentally resting their "
   "hand on the shift key and doesn't really want to enable these functions - it's sort of a 'get off the key' "
   "warning).  Since you DO want these functions enabled, just ignore these warning beeps and keep holding the key down.  "
   "After about 4 seconds more you will hear an up-siren.  You can release the key now and the "
   "functions will be enabled using the values you set for RepeatKeys, SlowKeys and BounceKeys.\n\nNOTE: You can "
   "disable this group of keyboard functions using this same procedure.  When you disable the group you will "
   "hear a down-siren.\n                                                               MORE...",
   FALSE,
   "                                                             Page 8 of 10\n\n        ^Emergency Enabling of the Keyboard "
   "Response Group - Part 1^\n\n     Some individuals are completely unable to operate the computer unless they can turn the "
   "keyboard repeat function off and/or turn SlowKeys on.  These individuals could find themselves in a Catch-22, "
	"unable to use the computer to turn the functions they need.  There is a solution to this problem built into AccessDOS.\n\n"
	"     An emergency enabling routine that allows you to turn functions on from the keyboard by simply holding the right "
	"shift key down for an extended period of time."
	"\n\n\n\n                                                               MORE...",
   FALSE,
   "                                                             Page 9 of 10\n        ^Emergency Enabling of the Keyboard "
	"Response Group - Part 2^\n\n     To enable the emergency settings for RepeatKeys and SlowKeys from the keyboard you just hold "
	"down the right shift key (as you normally do to enable the Keyboard Response Group) except that you keep holding it down "
	"even after you hear the up-siren.\n\n     If you hold the right shift key down long enough, this is the sequence of events "
	"that will happen:\n\nafter 4 seconds... 3 short beeps.    [they are the warning beeps]\n4 more seconds... an up-siren  "
	"[Kybd Resp Group turns on - your settings]\n4 more seconds... 2 up-sirens  [Repeat Rate & Acc. Dly off, Debnce 1 Sec]\n"
	"4 more seconds... 3 up-sirens  [Repeat Rate off, Acc. Dly 2 sec (Max)]\n"
	"\n                                                               MORE...",
   FALSE,
   "    In other words...  holding the right shift key down for 8 seconds will enable your normal settings for "
   "RepeatKeys, SlowKeys, and BounceKeys.\n\n    If you continue to hold it down until you hear 2 up-sirens, you "
   "will go into emergency setting #1 where the keyboard's repeat function will be turned off, Acceptance Delay "
   "will be off and Debounce Time will be 1 second.\n\n    If you continue to hold it down until you hear 3 up-sirens, "
	"you will go into emergency setting #2 where they keyboard's repeat function will be turned off and SlowKeys will be "
	"turned on to its Maximum (2 sec).\n\n    Setting #2 is a very slow keyboard response but should be usable by "
   "most anyone who can use the keyboard.  It would only be used long enough to run this program and change "
   "the settings to the values needed by the person.  Once the settings have been made and saved they shouldn't need to "
   "use this feature again unless someone changes their settings.\n                                - END -",
   TRUE
};

HELP_SCREEN_RECORD helpTextTimeOut[] =
{
   "\n    When AccessDOS is used on a shared computer it is sometimes "
   "useful to have an automatic timeout feature that would turn the AccessDOS functions off if the computer were left "
	"idle for awhile.  The TimeOut feature provides this capability.\n\n\nThere are two settings for the TimeOut feature:\n\n"
	"1) The IDLE TIME setting controls the length of time that the computer keyboard must be idle before AccessDOS will automatically "
   "turn itself off.  (AccessDOS is still loaded so it can be easily re-enabled from the keyboard without having to run "
   "this setup program.)\n\n2) You can control whether AccessDOS will make a sound (a down-siren) when it turns itself "
   "off.\n\n                                  - END -",
	TRUE
};

HELP_SCREEN_RECORD helpTextMisc[] =
{
   "\n\n                        ^Miscellaneous Adjustments^\n\n\n^ Space Saving Keyboard:^ If you are using a space "
   "saving keyboard such as\n                       the one that comes with the IBM PS/2 models 25 or\n                       "
   "you should set this option to YES so that\n                       AccessDOS will work properly with the keyboard."
   "\n\n\n\n\n\n\n\n\n                                  - END -",
   TRUE
};

HELP_SCREEN_RECORD helpTextShowSounds[] =
{
	"\n   For individuals with hearing impairments "
	"or for anyone working in a noisy environment, "
	"it is hard or impossible to hear beeps and other "
	"sounds made by the computer.  ShowSounds provides "
	"a visual indication on the screen each time a sound "
	"(other than a click) is made by AccessDOS.\n\n   "
	"ShowSounds has two options.  Whenever your computer "
	"makes a sound you can either have ShowSounds FLASH "
	"your screen or you can have it provide a small VISUAL "
	"CUE () in the upper left hand corner of your screen."
	"\n\n   For sounds created by AccessDOS, the small "
	"visual cue also gives you some additional information "
	"about the sound."
	"\n              Up-Siren                    High "
	"Beep \n              Down-Siren               	   Low Beep"
	"\n               Sounds produced by other programs\n"
	"NOTE: Not all programs permit the musical note to appear "
	"(see manual).\n\n                                  - END -",
	TRUE
};
