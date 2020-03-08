public table
_msg segment
table   db 4Ch,'Sorts input and writes results to the screen, a file, or another device.',13,10,13,10
        db 4Bh,'SORT [/R] [/+n] < [drive1:][path1]filename1 [> [drive2:][path2]filename2]',13,10
        db 3Dh,'[command |] SORT [/R] [/+n] [> [drive2:][path2]filename2]',13,10,13,10
        db 79h,'  /R                         Reverses the sort order; that is, sorts Z to A,',13,10
        db     '                             then 9 to 0.',13,10
        db 70h,'  /+n                        Sorts the file according to characters in',13,10
        db     '                             column n.',13,10
        db 3Dh,'  [drive1:][path1]filename1  Specifies a file to be sorted.',13,10
        db 76h,'  [drive2:][path2]filename2  Specifies a file where the sorted input is to be ',13,10 
        db     '                             stored.',13,10
        db 50h,'  command                    Specifies a command whose output is to be sorted.',13,10
_msg ends
end