/*  VARS.C  */


#include "skdefs.h"
#include "gideidef.h"

/*	SERIAL PORT VARIABLES  */


#if !defined ALL_LINK
	unsigned int skCommPortAddr = COM1;
	unsinged int combase = COM2;
	BYTE fmouse_id = SERIALMOUSE;
	unsigned int skBaudRate = BAUD300;
	unsigned char skCommIRQ = IRQ4;
	BYTE vector = 0x15;
	BYTE comp_id = 8;				/* 6 for model 30 */
	BYTE finject_keys = TRUE;
	BYTE serialKeysOn = TRUE;
#endif

/*	LOOKUP TABLES	*/

struct aliasTableType nullTable[] = {
	{ "",		0	},
};


struct aliasTableType gideiAliasTable[] = {
	{ "BEGIN",	   BEGINCODE},
	{ "BLKTRNS",	BLKTRANSCODE},
	{ "CLEAR",		CLEARCODE},
	{ "END", 		ENDCODE},
	{ "",				NOCODE},
};


struct aliasTableType commandsAliasTable[] = {
	{ "baudrate",	BAUDRATECODE},
	{ "click",		MOUCLICKCODE},
	{ "combine",	KBDCOMBINECODE},
	{ "comm",		COMMCODE},
	{ "dblclick",	MOUDOUBLECLICKCODE},
	{ "gen",			GENCODE},
	{ "goto",		MOUGOTOCODE},
	{ "hold",		KBDHOLDCODE},
	{ "kbd",			KBDEXPANSIONCODE},
	{ "lock",		KBDLOCKCODE},
	{ "mou",			MOUEXPANSIONCODE},
	{ "moulock", 	MOULOCKCODE},
	{ "mourel",		MOURELCODE},
	{ "moureset",	MOURESETCODE},
	{ "move",		MOUMOVECODE},
	{ "press",		KBDPRESSCODE},
	{ "rel",			KBDRELCODE},
	{ "",				NOCODE},
};

struct aliasTableType kbdAliasTable[] = {
	{ "desc",		KBDDESCRIPTIONCODE},
	{ "ind",			KBDINDICATORCODE},
	{ "model",		KBDMODELCODE},
	{ "ver",			KBDVERSIONCODE},
	{ "",				NOCODE},
};


struct aliasTableType kbdModelAliasTable[] = {
	{ "ibmat",		IBMATCODE},
	{ "ibmenhc",	IBMENHANCEDCODE},					/* IBM 101 key keyboard */
	{ "ibmpc",		IBMPCCODE},							/* IBM original keyboard */
	{ "",				NOCODE},
};


struct aliasTableType kbdDescriptionAliasTable[] = {
	{ "",			NOCODE},
};

struct aliasTableType kbdVersionAliasTable[] = {
	{ "",			NOCODE},
};

struct aliasTableType kbdIndicatorAliasTable[] = {
	{ "",			NOCODE},
};

struct aliasTableType mouseAliasTable[] = {
	{ "",			NOCODE},
};

struct aliasTableType genAliasTable[] = {
	{ "compid",		COMPUTERID},
	{ "user1",		SINGLEUSER},
	{ "",			NOCODE},
};


struct aliasTableType commAliasTable[] = {
	{ "",			NOCODE},
};



struct aliasTableType baudrateAliasTable[] = {
	{ "1200",	BAUD1200CODE},
	{ "19200",	BAUD19200CODE},
	{ "2400",	BAUD2400CODE},
	{ "300",		BAUD300CODE},
	{ "4800",	BAUD4800CODE},
	{ "600",		BAUD600CODE},
	{ "9600",	BAUD9600CODE},
	{ "",			NOCODE},
};

struct aliasTableType mouButtonAliasTable[] = {
	{ "left",	LEFTBUTTONCODE},
	{ "right",	RIGHTBUTTONCODE},
	{ "",			NOCODE},
};



/****************************************************************************

	Table for converting the ASCII characters to the keys that need to be typed
	on the keyboard.

****************************************************************************/

struct asciiTableType asciiTable[] = {
	{ control_key,	two_key,},							/*   0 */
	{ control_key,	a_key,},								/*   1 */
	{ control_key,	b_key,},								/*   2 */
	{ control_key,	c_key,},								/*   3 */
	{ control_key,	d_key,},								/*   4 */
	{ control_key,	e_key,},								/*   5 */
	{ control_key,	f_key,},								/*   6 */
	{ control_key,	g_key,},								/*   7 */
	{ control_key,	h_key,},								/*   8 */
	{ control_key,	i_key,},								/*   9 */
	{ control_key,	j_key,},								/*  10 */
	{ control_key,	k_key,},								/*  11 */
	{ control_key,	l_key,},								/*  12 */
	{ control_key,	m_key,},								/*  13 */
	{ control_key,	n_key,},								/*  14 */
	{ control_key,	o_key,},								/*  15 */
	{ control_key,	p_key,},								/*  16 */
	{ control_key,	q_key,},								/*  17 */
	{ control_key,	r_key,},								/*  18 */
	{ control_key,	s_key,},								/*  19 */
	{ control_key,	t_key,},								/*  20 */
	{ control_key,	u_key,},								/*  21 */
	{ control_key,	v_key,},								/*  22 */
	{ control_key,	w_key,},								/*  23 */
	{ control_key,	x_key,},								/*  24 */
	{ control_key,	y_key,},								/*  25 */
	{ control_key,	z_key,},								/*  26 */
	{ control_key,	lbracket_key,},					/*  27 */
	{ control_key,	bslash_key,},						/*  28 */
	{ control_key,	rbracket_key,},					/*  29 */
	{ control_key,	six_key,},							/*  30 */
	{ control_key,	hyphen_key,},						/*  31 */
	{ space_key,	no_key,},							/*  32 */
	{ shift_key,	one_key,},							/*  33 */
	{ shift_key,	rquote_key,},						/*  34 */
	{ shift_key,	three_key,},						/*  35 */
	{ shift_key,	four_key,},							/*  36 */
	{ shift_key,	five_key,},							/*  37 */
	{ shift_key,	seven_key,},						/*  38 */
	{ rquote_key,	no_key,},							/*  39 */
	{ shift_key,	nine_key,},							/*  40 */
	{ shift_key,	zero_key,},							/*  41 */
	{ shift_key,	eight_key,},						/*  42 */
	{ shift_key,	equal_key,},						/*  43 */
	{ comma_key,	no_key,},							/*  44 */
	{ hyphen_key,	no_key,},							/*  45 */
	{ period_key,	no_key,},							/*  46 */
	{ fslash_key,	no_key,},							/*  47 */
	{ zero_key,		no_key,},							/*  48 */
	{ one_key,		no_key,},							/*  49 */
	{ two_key,		no_key,},							/*  50 */
	{ three_key,	no_key,},							/*  51 */
	{ four_key,		no_key,},							/*  52 */
	{ five_key,		no_key,},							/*  53 */
	{ six_key,		no_key,},							/*  54 */
	{ seven_key,	no_key,},							/*  55 */
	{ eight_key,	no_key,},							/*  56 */
	{ nine_key,		no_key,},							/*  57 */
	{ shift_key,	semicolon_key,},					/*  58 */
	{ semicolon_key,no_key,},							/*  59 */
	{ shift_key,	comma_key,},						/*  60 */
	{ equal_key,	no_key,},							/*  61 */
	{ shift_key,	period_key,},						/*  62 */
	{ shift_key,	fslash_key,},						/*  63 */
	{ shift_key,	two_key,},							/*  64 */
	{ shift_key,	a_key,},								/*  65 */
	{ shift_key,	b_key,},								/*  66 */
	{ shift_key,	c_key,},								/*  67 */
	{ shift_key,	d_key,},								/*  68 */
	{ shift_key,	e_key,},								/*  69 */
	{ shift_key,	f_key,},								/*  70 */
	{ shift_key,	g_key,},								/*  71 */
	{ shift_key,	h_key,},								/*  72 */
	{ shift_key,	i_key,},								/*  73 */
	{ shift_key,	j_key,},								/*  74 */
	{ shift_key,	k_key,},								/*  75 */
	{ shift_key,	l_key,},								/*  76 */
	{ shift_key,	m_key,},								/*  77 */
	{ shift_key,	n_key,},								/*  78 */
	{ shift_key,	o_key,},								/*  79 */
	{ shift_key,	p_key,},								/*  80 */
	{ shift_key,	q_key,},								/*  81 */
	{ shift_key,	r_key,},								/*  82 */
	{ shift_key,	s_key,},								/*  83 */
	{ shift_key,	t_key,},								/*  84 */
	{ shift_key,	u_key,},								/*  85 */
	{ shift_key,	v_key,},								/*  86 */
	{ shift_key,	w_key,},								/*  87 */
	{ shift_key,	x_key,},								/*  88 */
	{ shift_key,	y_key,},								/*  89 */
	{ shift_key,	z_key,},								/*  90 */
	{ lbracket_key,	no_key,},						/*  91 */
	{ bslash_key,	no_key,},							/*  92 */
	{ rbracket_key,	no_key,},						/*  93 */
	{ shift_key,	six_key,},							/*  94 */
	{ shift_key,	hyphen_key,},						/*  95 */
	{ lquote_key,	no_key,},							/*  96 */
	{ a_key,		no_key,},								/*  97 */
	{ b_key,		no_key,},								/*  98 */
	{ c_key,		no_key,},								/*  99 */
	{ d_key,		no_key,},								/* 100 */
	{ e_key,		no_key,},								/* 101 */
	{ f_key,		no_key,},								/* 102 */
	{ g_key,		no_key,},								/* 103 */
	{ h_key,		no_key,},								/* 104 */
	{ i_key,		no_key,},								/* 105 */
	{ j_key,		no_key,},								/* 106 */
	{ k_key,		no_key,},								/* 107 */
	{ l_key,		no_key,},								/* 108 */
	{ m_key,		no_key,},								/* 109 */
	{ n_key,		no_key,},								/* 110 */
	{ o_key,		no_key,},								/* 111 */
	{ p_key,		no_key,},								/* 112 */
	{ q_key,		no_key,},								/* 113 */
	{ r_key,		no_key,},								/* 114 */
	{ s_key,		no_key,},								/* 115 */
	{ t_key,		no_key,},								/* 116 */
	{ u_key,		no_key,},								/* 117 */
	{ v_key,		no_key,},								/* 118 */
	{ w_key,		no_key,},								/* 119 */
	{ x_key,		no_key,},								/* 120 */
	{ y_key,		no_key,},								/* 121 */
	{ z_key,		no_key,},								/* 122 */
	{ shift_key,	lbracket_key,},					/* 123 */
	{ shift_key,	bslash_key,},						/* 124 */
	{ shift_key,	rbracket_key,},					/* 125 */
	{ shift_key,	lquote_key,},						/* 126 */
	{ control_key,	kpperiod_key,},					/* 127 */
};


/****************************************************************************

	Table for converting the ASCII string of the key names into the key
	number.

****************************************************************************/

struct aliasTableType	keyAliasTable[] = {
	{ "alt",		alt_key,},
	{ "backspace",	backspace_key,},
  	{ "bksp",		backspace_key,},
  	{ "break",		pause_key,},
	{ "capslk",		caps_key,},
	{ "capslock",	caps_key,},
	{ "comma",		comma_key,},
	{ "control", 	control_key,},
	{ "ctrl",		control_key,},
	{ "del",		kpperiod_key,},
	{ "delete",		delete_key,},
	{ "divide",		kpfslash_key,},
	{ "down",		down_key,},
	{ "end",		end_key,},
	{ "enter",		return_key,},
	{ "esc",		escape_key,},
	{ "escape",		escape_key,},
	{ "f1",			f1_key,},
	{ "f10",		f10_key,},
	{ "f11",		f11_key,},
	{ "f12",		f12_key,},
	{ "f2",			f2_key,},
	{ "f3",			f3_key,},
	{ "f4",			f4_key,},
	{ "f5",			f5_key,},
	{ "f6",			f6_key,},
	{ "f7",			f7_key,},
	{ "f8",			f8_key,},
	{ "f9",			f9_key,},
	{ "home",		home_key,},
	{ "ins",		kp0_key,},
	{ "insert",		insert_key,},
	{ "kp*",		kpstar_key,},
	{ "kp+",		kpplus_key,},
	{ "kp-",		kpminus_key,},
	{ "kp/",		kpfslash_key,},
	{ "kp0",		kp0_key,},
	{ "kp1",		kp1_key,},
	{ "kp2",		kp2_key,},
	{ "kp3",		kp3_key,},
	{ "kp4",		kp4_key,},
	{ "kp5",		kp5_key,},
	{ "kp6",		kp6_key,},
	{ "kp7",		kp7_key,},
	{ "kp8",		kp8_key,},
	{ "kp9",		kp9_key,},
	{ "kpdel",		kpperiod_key,},
	{ "kpdelete",	kpperiod_key,},
	{ "kpdivide",	kpfslash_key,},
	{ "kpdn",		kp2_key,},
	{ "kpdown",		kp2_key,},
	{ "kpdp",		kpperiod_key,},
	{ "kpend",		kp1_key,},
	{ "kpenter",	kpenter_key,},
	{ "kphome",		kp7_key,},
	{ "kpins",		kp0_key,},
	{ "kpinsert",	kp0_key,},
	{ "kpleft",		kp4_key,},
	{ "kpmidl",		kp5_key,},
	{ "kpminus",	kpminus_key,},
	{ "kppagedown",	kp3_key,},
	{ "kppageup",	kp9_key,},
	{ "kppgdn",		kp3_key,},
	{ "kppgup",		kp9_key,},
	{ "kpplus",		kpplus_key,},
	{ "kpright",	kp6_key,},
	{ "kpslash",	kpfslash_key,},
	{ "kpstar",		kpstar_key,},
	{ "kptimes",	kpstar_key,},
	{ "kpup",		kp8_key,},
	{ "lalt",		lalt_key,},
	{ "lcontrol",	lcontrol_key,},
	{ "lctrl",		lcontrol_key,},
	{ "left",		left_key,},
	{ "leftalt",	lalt_key,},
	{ "leftcontrol", lcontrol_key,},
	{ "leftctrl",	lcontrol_key,},
	{ "leftshift",	lshift_key,},
	{ "lshift",		lshift_key,},
	{ "multiply",	kpstar_key,},
	{ "numlk",		numlock_key,},
	{ "numlock",	numlock_key,},
	{ "pagedown", 	pagedown_key,},
	{ "pageup",		pageup_key,},
	{ "pause",		pause_key,},
	{ "period",		period_key,},
	{ "pgdn",		pagedown_key,},
	{ "pgup",		pageup_key,},
	{ "print",		print_key,},
	{ "printscreen", print_key,},
	{ "prtsc",		print_key,},
	{ "ralt",		ralt_key,},
	{ "rcontrol",	rcontrol_key,},
	{ "rctrl",		rcontrol_key,},
	{ "ret",		return_key,},
	{ "return",		return_key,},
	{ "right",		right_key,},
	{ "rightalt",	ralt_key,},
	{ "rightcontrol", rcontrol_key,},
	{ "rightctrl",	rcontrol_key,},
	{ "rightshift",	rshift_key,},
	{ "rshift",		rshift_key,},
	{ "scroll",		scroll_key,},
	{ "scrolllock",	scroll_key,},
	{ "shift",		shift_key,},
	{ "space",		space_key,},
	{ "sysreq",		print_key,},
	{ "tab",		tab_key,},
	{ "tilde",		lquote_key,},
	{ "up",			up_key,},
	{ "",			no_key,},
};




/*************************************************************************

	Table of valid key codes

**************************************************************************/

BYTE okKeyTbl[] = {
	lquote_key,one_key,two_key,
	three_key,four_key,five_key,six_key,
	seven_key,eight_key,nine_key,zero_key,
	hyphen_key,equal_key,backspace_key,tab_key,
	q_key,w_key,e_key,r_key,
	t_key,y_key,u_key,i_key,
	o_key,p_key,lbracket_key,rbracket_key,
	bslash_key,caps_key,a_key,s_key,
	d_key,f_key,g_key,h_key,
	j_key,k_key,l_key,semicolon_key,
	rquote_key,return_key,lshift_key,z_key,
	x_key,c_key,v_key,b_key,
	n_key,m_key,comma_key,period_key,
	fslash_key,rshift_key,lcontrol_key,lcommand_key,
	lalt_key,space_key,ralt_key,rcommand_key,
	rcontrol_key,insert_key,delete_key,left_key,
	home_key,end_key,up_key,down_key,
	pageup_key,pagedown_key,right_key,numlock_key,
	kp7_key,kp4_key,kp1_key,kpfslash_key,
	kp8_key,kp5_key,kp2_key,kp0_key,
	kpstar_key,kp9_key,kp6_key,kp3_key,
	kpperiod_key,kpminus_key,kpplus_key,kpequal_key,
	kpenter_key,escape_key,f1_key,f2_key,
	f3_key,f4_key,f5_key,f6_key,
	f7_key,f8_key,f9_key,f10_key,
	f11_key,f12_key,print_key,scroll_key,
	pause_key,reset_key,
	no_key
	};






/****************************************************************************

	Lookup table to convert a key by its keynumber to the
	scan code for the key.

****************************************************************************/
	BYTE IBMextendedScanCodeSet1[] = {
		0x00,	/* 0 */		
		0x29,	/* 1 */		
		0x02,	/* 2 */		
		0x03,	/* 3 */		
		0x04,	/* 4 */		
		0x05,	/* 5 */		
		0x06,	/* 6 */		
		0x07,	/* 7 */		
		0x08,	/* 8 */		
		0x09,	/* 9 */		
		0x0A,	/* 10 */	
		0x0B,	/* 11 */	
		0x0C,	/* 12 */	
		0x0D,	/* 13 */	
		0x00,	/* 14 */	
		0x0E,	/* 15 */	
		0x0F,	/* 16 */	
		0x10,	/* 17 */	
		0x11,	/* 18 */	
		0x12,	/* 19 */	
		0x13,	/* 20 */	
		0x14,	/* 21 */	
		0x15,	/* 22 */	
		0x16,	/* 23 */	
		0x17,	/* 24 */	
		0x18,	/* 25 */	
		0x19,	/* 26 */	
		0x1A,	/* 27 */	
		0x1B,	/* 28 */	
		0x2B,	/* 29 */	
		0x3A,	/* 30 */	
		0x1E,	/* 31 */	
		0x1F,	/* 32 */	
		0x20,	/* 33 */	
		0x21,	/* 34 */	
		0x22,	/* 35 */	
		0x23,	/* 36 */	
		0x24,	/* 37 */	
		0x25,	/* 38 */	
		0x26,	/* 39 */	
		0x27,	/* 40 */	
		0x28,	/* 41 */	
		0x2B,	/* 42 */	
		0x1C,	/* 43 */	
		0x2A,	/* 44 */	
		0x56,	/* 45 */	
		0x2C,	/* 46 */	
		0x2D,	/* 47 */	
		0x2E,	/* 48 */	
		0x2F,	/* 49 */	
		0x30,	/* 50 */	
		0x31,	/* 51 */	
		0x32,	/* 52 */	
		0x33,	/* 53 */	
		0x34,	/* 54 */	
		0x35,	/* 55 */	
		0x00,	/* 56 */	
		0x36,	/* 57 */	
		0x1D,	/* 58 */	
		0x00,	/* 59 */	
		0x38,	/* 60 */	
		0x39,	/* 61 */	
		0x38,	/* 62 */	
		0x00,	/* 63 */	
		0x1D,	/* 64 */	
		0x00,	/* 65 */	
		0x00,	/* 66 */	
		0x00,	/* 67 */	
		0x00,	/* 68 */	
		0x00,	/* 69 */	
		0x00,	/* 70 */	
		0x00,	/* 71 */	
		0x00,	/* 72 */	
		0x00,	/* 73 */	
		0x00,	/* 74 */	
		0x52,	/* 75 */	
		0x53,	/* 76 */	
		0x00,	/* 77 */	
		0x00,	/* 78 */	
		0x4B,	/* 79 */	
		0x47,	/* 80 */	
		0x4F,	/* 81 */	
		0x00,	/* 82 */	
		0x48,	/* 83 */	
		0x50,	/* 84 */	
		0x49,	/* 85 */	
		0x51,	/* 86 */	
		0x00,	/* 87 */	
		0x00,	/* 88 */	
		0x4D,	/* 89 */	
		0x45,	/* 90 */	
		0x47,	/* 91 */	
		0x4B,	/* 92 */	
		0x4F,	/* 93 */	
		0x00,	/* 94 */	
		0x35,	/* 95 */	
		0x48,	/* 96 */	
		0x4C,	/* 97 */	
		0x50,	/* 98 */	
		0x52,	/* 99 */	
		0x37,	/* 100 */	
		0x49,	/* 101 */	
		0x4D,	/* 102 */	
		0x51,	/* 103 */	
		0x53,	/* 104 */	
		0x4A,	/* 105 */	
		0x4E,	/* 106 */	
		0x00,	/* 107 */	
		0x1C,	/* 108 */	
		0x00,	/* 109 */	
		0x01,	/* 110 */	
		0x00,	/* 111 */	
		0x3B,	/* 112 */	
		0x3C,	/* 113 */	
		0x3D,	/* 114 */	
		0x3E,	/* 115 */	
		0x3F,	/* 116 */	
		0x40,	/* 117 */	
		0x41,	/* 118 */	
		0x42,	/* 119 */	
		0x43,	/* 120 */	
		0x44,	/* 121 */	
		0x57,	/* 122 */	
		0x58,	/* 123 */	
		0x00,	/* 124 */	
		0x46,	/* 125 */	
		0x00,	/* 126 */	
		0x00	/* 127 */
		};


BYTE capsKeysTbl[] = {				/* keys affected by caps lock */
	q_key,w_key,e_key,r_key,
	t_key,y_key,u_key,i_key,
	o_key,p_key,
	a_key,s_key,
	d_key,f_key,g_key,h_key,
	j_key,k_key,l_key,
	z_key,
	x_key,c_key,v_key,b_key,
	n_key,m_key,
	no_key
	};

BYTE keyPadKeysTbl[] = {
	kp0_key,kp1_key,kp2_key,
	kp3_key,kp4_key,kp5_key,
	kp6_key,kp7_key,kp8_key,
	kp9_key,no_key
	};


/*  al,ah */
/*  ascii, scan */

struct scanTblType scanTbl[] = {
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 0   nokey 			*/
	{{0x60, 0x29,},  {0x7e, 0x29,},  {0x00, 0x00,},  {0xF0, 0x29,}},		/* 1   lquote_key 	*/
	{{0x31, 0x02,},  {0x21, 0x02,},  {0x00, 0x00,},  {0x00, 0x78,}},		/* 2   one_key 		*/
	{{0x32, 0x03,},  {0x40, 0x03,},  {0x00, 0x03,},  {0x00, 0x79,}},		/* 3   two_key 		*/
	{{0x33, 0x04,},  {0x23, 0x04,},  {0x00, 0x00,},  {0x00, 0x7a,}},		/* 4   three_key 		*/
	{{0x34, 0x05,},  {0x24, 0x05,},  {0x00, 0x00,},  {0x00, 0x7b,}},		/* 5   four_key 		*/
	{{0x35, 0x06,},  {0x25, 0x06,},  {0x00, 0x00,},  {0x00, 0x7c,}},		/* 6   five_key 		*/
	{{0x36, 0x07,},  {0x5e, 0x07,},  {0x1e, 0x07,},  {0x00, 0x7d,}},		/* 7   six_key 		*/
	{{0x37, 0x08,},  {0x26, 0x08,},  {0x00, 0x00,},  {0x00, 0x7e,}},		/* 8   seven_key 		*/
	{{0x38, 0x09,},  {0x2a, 0x09,},  {0x00, 0x00,},  {0x00, 0x7f,}},		/* 9   eight_key 		*/
	{{0x39, 0x0a,},  {0x28, 0x0a,},  {0x00, 0x00,},  {0x00, 0x80,}},		/* 10  nine_key 		*/
	{{0x30, 0x0b,},  {0x29, 0x0b,},  {0x00, 0x00,},  {0x00, 0x81,}},		/* 11  zero_key 		*/
	{{0x2d, 0x0c,},  {0x5f, 0x0c,},  {0x1f, 0x0c,},  {0x00, 0x82,}},		/* 12  hyphen_key 	*/
	{{0x3d, 0x0d,},  {0x2b, 0x0d,},  {0x00, 0x00,},  {0x00, 0x83,}},		/* 13  equal_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 14 					*/
	{{0x08, 0x0e,},  {0x08, 0x0e,},  {0x7f, 0x0e,},  {0xF0, 0x0e,}},		/* 15  backspace_key */
	{{0x09, 0x0f,},  {0x00, 0x0f,},  {0x00, 0x94,},  {0x00, 0xa5,}},		/* 16  tab_key 		*/
	{{0x71, 0x10,},  {0x51, 0x10,},  {0x11, 0x10,},  {0x00, 0x10,}},		/* 17  q_key 			*/
	{{0x77, 0x11,},  {0x57, 0x11,},  {0x17, 0x11,},  {0x00, 0x11,}},		/* 18  w_key 			*/
	{{0x65, 0x12,},  {0x45, 0x12,},  {0x05, 0x12,},  {0x00, 0x12,}},		/* 19  e_key 			*/
	{{0x72, 0x13,},  {0x52, 0x13,},  {0x12, 0x13,},  {0x00, 0x13,}},		/* 20  r_key 			*/
	{{0x74, 0x14,},  {0x54, 0x14,},  {0x14, 0x14,},  {0x00, 0x14,}},		/* 21  t_key 			*/
	{{0x79, 0x15,},  {0x59, 0x15,},  {0x19, 0x15,},  {0x00, 0x15,}},		/* 22  y_key 			*/
	{{0x75, 0x16,},  {0x55, 0x16,},  {0x15, 0x16,},  {0x00, 0x16,}},		/* 23  u_key 			*/
	{{0x69, 0x17,},  {0x49, 0x17,},  {0x09, 0x17,},  {0x00, 0x17,}},		/* 24  i_key 			*/
	{{0x6f, 0x18,},  {0x4f, 0x18,},  {0x0f, 0x18,},  {0x00, 0x18,}},		/* 25  o_key 			*/
	{{0x70, 0x19,},  {0x50, 0x19,},  {0x10, 0x19,},  {0x00, 0x19,}},		/* 26  p_key 			*/
	{{0x5b, 0x1a,},  {0x7b, 0x1a,},  {0x1b, 0x1a,},  {0xF0, 0x1a,}},		/* 27  lbracket_key 	*/
	{{0x5d, 0x1b,},  {0x7d, 0x1b,},  {0x1d, 0x1b,},  {0xF0, 0x1b,}},		/* 28  rbracket_key 	*/
	{{0x5c, 0x2b,},  {0x7c, 0x2b,},  {0x1c, 0x2b,},  {0xF0, 0x2b,}},		/* 29  bslash_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 30  caps_key 		*/
	{{0x61, 0x1e,},  {0x41, 0x1e,},  {0x01, 0x1e,},  {0x00, 0x1e,}},		/* 31  a_key 			*/
	{{0x73, 0x1f,},  {0x53, 0x1f,},  {0x13, 0x1f,},  {0x00, 0x1f,}},		/* 32  s_key 			*/
	{{0x64, 0x20,},  {0x44, 0x20,},  {0x04, 0x20,},  {0x00, 0x20,}},		/* 33  d_key 			*/
	{{0x66, 0x21,},  {0x46, 0x21,},  {0x06, 0x21,},  {0x00, 0x21,}},		/* 34  f_key 			*/
	{{0x67, 0x22,},  {0x47, 0x22,},  {0x07, 0x22,},  {0x00, 0x22,}},		/* 35  g_key 			*/
	{{0x68, 0x23,},  {0x48, 0x23,},  {0x08, 0x23,},  {0x00, 0x23,}},		/* 36  h_key 			*/
	{{0x6a, 0x24,},  {0x4a, 0x24,},  {0x0a, 0x24,},  {0x00, 0x24,}},		/* 37  j_key 			*/
	{{0x6b, 0x25,},  {0x4b, 0x25,},  {0x0b, 0x25,},  {0x00, 0x25,}},		/* 38  k_key 			*/
	{{0x6c, 0x26,},  {0x4c, 0x26,},  {0x0c, 0x26,},  {0x00, 0x26,}},		/* 39  l_key 			*/
	{{0x3b, 0x27,},  {0x3a, 0x27,},  {0x00, 0x00,},  {0xF0, 0x27,}},		/* 40  semicolon_key */
	{{0x27, 0x28,},  {0x22, 0x28,},  {0x00, 0x00,},  {0xF0, 0x28,}},		/* 41  rquote_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 42 					*/
	{{0x0d, 0x1c,},  {0x0d, 0x1c,},  {0x0a, 0x1c,},  {0xF0, 0x1c,}},		/* 43  return_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 44  lshift_key 	*/
	{{0x5c, 0x56,},  {0x7c, 0x56,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 45  45 key 			*/
	{{0x7a, 0x2c,},  {0x5a, 0x2c,},  {0x1a, 0x2c,},  {0x00, 0x2c,}},		/* 46  z_key 			*/
	{{0x78, 0x2d,},  {0x58, 0x2d,},  {0x18, 0x2d,},  {0x00, 0x2d,}},		/* 47  x_key 			*/
	{{0x63, 0x2e,},  {0x43, 0x2e,},  {0x03, 0x2e,},  {0x00, 0x2e,}},		/* 48  c_key 			*/
	{{0x76, 0x2f,},  {0x56, 0x2f,},  {0x16, 0x2f,},  {0x00, 0x2f,}},		/* 49  v_key 			*/
	{{0x62, 0x30,},  {0x42, 0x30,},  {0x02, 0x30,},  {0x00, 0x30,}},		/* 50  b_key 			*/
	{{0x6e, 0x31,},  {0x4e, 0x31,},  {0x0e, 0x31,},  {0x00, 0x31,}},		/* 51  n_key 			*/
	{{0x6d, 0x32,},  {0x4d, 0x32,},  {0x0d, 0x32,},  {0x00, 0x32,}},		/* 52  m_key 			*/
	{{0x2c, 0x33,},  {0x3c, 0x33,},  {0x00, 0x00,},  {0xF0, 0x33,}},		/* 53  comma_key 		*/
	{{0x2e, 0x34,},  {0x3e, 0x34,},  {0x00, 0x00,},  {0xF0, 0x34,}},		/* 54  period_key 	*/
	{{0x2f, 0x35,},  {0x3f, 0x35,},  {0x00, 0x00,},  {0xF0, 0x35,}},		/* 55  fslash_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 56 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 57  rshift_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 58  lcontrol_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 59 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 60  lalt_key 		*/
	{{0x20, 0x39,},  {0x20, 0x39,},  {0x20, 0x39,},  {0x20, 0x39,}},		/* 61  space_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 62  ralt_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 63 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 64  rcontrol_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 65 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 66 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 67 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 68 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 69 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 70 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 71 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 72 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 73 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 74 					*/
	{{0xe0, 0x52,},  {0xe0, 0x52,},  {0xe0, 0x92,},  {0x00, 0xa2,}},		/* 75  insert_key 	*/
	{{0xe0, 0x53,},  {0xe0, 0x53,},  {0xe0, 0x93,},  {0x00, 0xa3,}},		/* 76  delete_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 77 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 78 					*/
	{{0xe0, 0x4b,},  {0xe0, 0x4b,},  {0xe0, 0x73,},  {0x00, 0x9b,}},		/* 79  left_key 		*/
	{{0xe0, 0x47,},  {0xe0, 0x47,},  {0xe0, 0x77,},  {0x00, 0x97,}},		/* 80  home_key 		*/
	{{0xe0, 0x4f,},  {0xe0, 0x4f,},  {0xe0, 0x75,},  {0x00, 0x9f,}},		/* 81  end_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 82 					*/
	{{0xe0, 0x48,},  {0xe0, 0x48,},  {0xe0, 0x8d,},  {0x00, 0x98,}},		/* 83  up_key 			*/
	{{0xe0, 0x50,},  {0xe0, 0x50,},  {0xe0, 0x91,},  {0x00, 0xa0,}},		/* 84  down_key 		*/
	{{0xe0, 0x49,},  {0xe0, 0x49,},  {0xe0, 0x84,},  {0x00, 0x99,}},		/* 85  pageup_key 	*/
	{{0xe0, 0x51,},  {0xe0, 0x51,},  {0xe0, 0x76,},  {0x00, 0xa1,}},		/* 86  pagedown_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 87 					*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 88 					*/
	{{0xe0, 0x4d,},  {0xe0, 0x4d,},  {0xe0, 0x74,},  {0x00, 0x9d,}},		/* 89  right_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 90  numlock_key 	*/
	{{0x00, 0x47,},  {0x37, 0x47,},  {0x00, 0x77,},  {0x00, 0x00,}},		/* 91  kp7_key 		*/
	{{0x00, 0x4b,},  {0x34, 0x4b,},  {0x00, 0x73,},  {0x00, 0x00,}},		/* 92  kp4_key 		*/
	{{0x00, 0x4f,},  {0x31, 0x4f,},  {0x00, 0x75,},  {0x00, 0x00,}},		/* 93  kp1_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 94 					*/
	{{0x2f, 0xe0,},  {0x2f, 0xe0,},  {0x00, 0x95,},  {0x00, 0xa4,}},		/* 95  kpfslash_key 	*/
	{{0x00, 0x48,},  {0x38, 0x48,},  {0x00, 0x8d,},  {0x00, 0x00,}},		/* 96  kp8_key 		*/
	{{0xf0, 0x4c,},  {0x35, 0x4c,},  {0x00, 0x8f,},  {0x00, 0x00,}},		/* 97  kp5_key 		*/
	{{0x00, 0x50,},  {0x32, 0x50,},  {0x00, 0x91,},  {0x00, 0x00,}},		/* 98  kp2_key 		*/
	{{0x00, 0x52,},  {0x30, 0x52,},  {0x00, 0x92,},  {0x00, 0x00,}},		/* 99  kp0_key 		*/
	{{0x2a, 0x37,},  {0x2a, 0x37,},  {0x00, 0x96,},  {0xF0, 0x37,}},		/* 100 kpstar_key 	*/
	{{0x00, 0x49,},  {0x39, 0x49,},  {0x00, 0x84,},  {0x00, 0x00,}},		/* 101 kp9_key 		*/
	{{0x00, 0x4d,},  {0x36, 0x4d,},  {0x00, 0x74,},  {0x00, 0x00,}},		/* 102 kp6_key 		*/
	{{0x00, 0x51,},  {0x33, 0x51,},  {0x00, 0x76,},  {0x00, 0x00,}},		/* 103 kp3_key 		*/
	{{0x00, 0x53,},  {0x2e, 0x53,},  {0x00, 0x93,},  {0x00, 0x00,}},		/* 104 kpperiod_key 	*/
	{{0x2d, 0x4a,},  {0x2d, 0x4a,},  {0x00, 0x8e,},  {0xF0, 0x4a,}},		/* 105 kpminus_key 	*/
	{{0x2b, 0x4e,},  {0x2b, 0x4e,},  {0x00, 0x90,},  {0xF0, 0x4e,}},		/* 106 kpplus_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 107 					*/
	{{0x0d, 0xe0,},  {0x0d, 0xe0,},  {0x0a, 0xe0,},  {0x00, 0xa6,}},		/* 108 kpenter_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 109 					*/
	{{0x1b, 0x01,},  {0x1b, 0x01,},  {0x1b, 0x01,},  {0xF0, 0x01,}},		/* 110 escape_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 111 					*/
	{{0x00, 0x3b,},  {0x00, 0x54,},  {0x00, 0x5e,},  {0x00, 0x68,}},		/* 112 f1_key 			*/
	{{0x00, 0x3c,},  {0x00, 0x55,},  {0x00, 0x5f,},  {0x00, 0x69,}},		/* 113 f2_key 			*/
	{{0x00, 0x3d,},  {0x00, 0x56,},  {0x00, 0x60,},  {0x00, 0x6a,}},		/* 114 f3_key 			*/
	{{0x00, 0x3e,},  {0x00, 0x57,},  {0x00, 0x61,},  {0x00, 0x6b,}},		/* 115 f4_key 			*/
	{{0x00, 0x3f,},  {0x00, 0x58,},  {0x00, 0x62,},  {0x00, 0x6c,}},		/* 116 f5_key 			*/
	{{0x00, 0x40,},  {0x00, 0x59,},  {0x00, 0x63,},  {0x00, 0x6d,}},		/* 117 f6_key 			*/
	{{0x00, 0x41,},  {0x00, 0x5a,},  {0x00, 0x64,},  {0x00, 0x6e,}},		/* 118 f7_key 			*/
	{{0x00, 0x42,},  {0x00, 0x5b,},  {0x00, 0x65,},  {0x00, 0x6f,}},		/* 119 f8_key 			*/
	{{0x00, 0x43,},  {0x00, 0x5c,},  {0x00, 0x66,},  {0x00, 0x70,}},		/* 120 f9_key 			*/
	{{0x00, 0x44,},  {0x00, 0x5d,},  {0x00, 0x67,},  {0x00, 0x71,}},		/* 121 f10_key 		*/
	{{0x00, 0x85,},  {0x00, 0x87,},  {0x00, 0x89,},  {0x00, 0x8b,}},		/* 122 f11_key 		*/
	{{0x00, 0x86,},  {0x00, 0x88,},  {0x00, 0x8a,},  {0x00, 0x8c,}},		/* 123 f12_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x72,},  {0x00, 0x00,}},		/* 124 print_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 125 scroll_key 	*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 126 pause_key 		*/
	{{0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,},  {0x00, 0x00,}},		/* 127 					*/
	};





/****************************************************************************

	FUNCTION:	noOpRoutine

	PURPOSE:	"Do nothing" routine

	COMMENTS:

****************************************************************************/
void noOpRoutine(void)
{
	return;
}


