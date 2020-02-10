/*  INT9.H  */


BOOL keyAffectedByCaps (unsigned char key);
BOOL keyAffectedByNum (unsigned char key);
void kbdBufferInjectKeysRoutine (BYTE key);
void injectDownInKbdBuffer (unsigned char key);
void injectUpInKbdBuffer (unsigned char key);





















