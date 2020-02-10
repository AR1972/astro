
	/* start prototypes here */

void GetInterruptNumber(void);
void GetAddressOfParameterBlock(void);
void GetAccessParameters(void);
void SetAccessParameters(unsigned char _far *paramBlockPtr, int forceDialogFlags);
void GetEquipmentParameters(void);
void SetEquipmentParameters(void);
int LoadParameters(void);
void SaveParameters(void);
void DisplayCurrentParameters(void);
void DisplayEquipmentParameters(void);

	/* end prototypes */

#define FILTER_USER_OPTION_OFF	   1
#define FILTER_USER_OPTION_1		   2
#define FILTER_USER_OPTION_2		   3

/*extern void *dataBlock;					/* address of the shared datablock */

