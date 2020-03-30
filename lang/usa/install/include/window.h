// window.h dummy file

extern char far *ErrorBuffer;

typedef struct WindowStruct
{
    unsigned Bottom;
    unsigned IsShadow;
    unsigned Right;
    unsigned Left;
    unsigned Top;
    unsigned Color;
    unsigned BorderColor;
    unsigned Type;
}WINDOW;

#define WINDOW_TYPE 0
#define MAX_STRINGS 64
#define EXIT_HLP 10
#define CONT_HLP 20
#define LAST_VIDEO_COL 80
#define START_COL 0
#define KILL_COLOR 0xFF
#define ERR_READING 2
#define ERR_WRITING 3
#define SAVE 1
#define TITLE_ROW 10

void DisplayGage( void );















