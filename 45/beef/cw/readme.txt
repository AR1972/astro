	How to build COW libraries from here:

1.  Make sure you've got a lot of memory (around 570 K)

2.  Run BUILDLQB.BAT to build all libraries (cowlqb, cowtandy.lib)

3.  Propogate the new COW library to \45\qb5\qbas as COW.LIB.
    Propogate the new COWTANDY.LIB to \45\qb5\qbas also.


NOTES:	If you CTRL-BREAK out of the batch file, you should answer 'N' to
	the prompt to terminate the batch file.  It will still terminate,
	but it will restore the environment variables that it modified.

WHEN YOU COMPARE YOUR NEWLY BUILT COWLQB.LIB to 45\QB5\QBAS\COW.LIB:
        You will have a few bytes not match.  These bytes are a
        Date/Time Stamp in the Library. These bytes would be:
        00018B37
        00018B38  Day Of Week
        00018B39

        00018B3B
        00018B3C  Month Of Year
        00018B3D

        00018B3F  Day Of Month
        00018B40

        00018B42  Hour
        00018B43

        00018B45  Minute
        00018B46

        00018B48  Second
        00018B49

        00018B54  Checksum Value
