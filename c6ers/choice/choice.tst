should here 11 beeps.. then choice Y
[Y,N]?Y
*** TEST "Default1" returned expected value*
should see no choices here...
N
*** TEST "Default2" returned expected value*
see choices A,B,C,D,E
[A,B,C,D,E]?E
*** TEST "Choices1" returned expected value*
should wait about 2 seconds...
[Y,N]?N
*** TEST "Timeout1" returned expected value*
should see message about invalid timeout syntax
CHOICE: Incorrect timeout syntax.  Expected form Tc,nn or T:c,nn
*** TEST "Timeout2" returned expected value*
should see message about timeout char not in choices
CHOICE: Timeout default not in specified (or default) choices.
*** TEST "Timeout3" returned expected value*
should see message about invalid choice syntax
CHOICE: invalid choice switch syntax. Expected form: /C[:]choices
*** TEST "BadChoice" returned expected value*
should see /? help
Waits for the user to choose one of a set of choices.

CHOICE [/C[:]choices] [/N] [/S] [/T[:]c,nn] [text]

/C[:]choices Specifies allowable keys. Default is YN
/N           Do not display choices and ? at end of prompt string.
/S           Treat choice keys as case sensitive.
/T[:]c,nn    Default choice to c after nn seconds
text         Prompt string to display

ERRORLEVEL is set to offset of key user presses in choices.

*** TEST "/? help" returned expected value*
should see invalid switch message
Invalid switch on command line. Expected form:
    CHOICE [/C[:]choices] [/N] [/S] [/T[:]c,nn] [text]

*** TEST " Invalid Switch" returned expected value*
should see prompt: "Testing this[Y,N]?"
Testing this[Y,N]?N
*** TEST " Prompt1" returned expected value*
should see prompt: "Testing this /C /Z /T"
Testing this /S /Z /TN
*** TEST " Prompt2" returned expected value*
should see message about only have one prompt
CHOICE: only one prompt string allowed. Expected Form:
    CHOICE [/C[:]choices] [/N] [/S] [/T[:]c,nn] [text]

*** TEST " Prompt3" returned expected value*
choice should be z.
[E,A,I,O,Y,U,z]?z
*** TEST " UcaseMap1" returned expected value*
choices should be upper case of „…†ƒ‚ˆ‰Š‹ŒŽ“”•–—˜™šz
[Ž,A,,š,A,E,E,E,E,I,I,I,Ž,,,O,™,O,U,U,Y,™,š,Z]?E
*** TEST " UcaseMap2" returned expected value*
 ALL TESTS returned expected value

C:\DATA\ASTRO\C6ERS\CHOICE>
