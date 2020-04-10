DEFINT A-Z

' Function call for QuickBASIC, Version 4,
'   and for future versions of Microsoft and IBM BASIC Compilers
' Comment out for older versions

DECLARE	FUNCTION Power2(A AS INTEGER, B	AS INTEGER)
PRINT "3 times 2 to the power of 5 is ";
PRINT Power2(3,5)

' Subprogram equivalent to function for QuickBASIC, Versions 1, 2, and 3;
'    for the Microsoft BASIC Compiler through Version 5.36
'    for the IBM BASIC Compiler through Version 2.02

' A = 3
' B = 5
' CALL Power2S(3,	5, C)
' PRINT "3 times 2 to the power of 5 is "; C

END


