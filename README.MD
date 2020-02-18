DOS 6.0.500 beta source code leak...

/*컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴*/
M001	SR	08/31/90	The stupid guys had sp = 0 even though they
				were allocating a stack segment. How did it
				work for so long?
/*컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴*/				

The pourpose of this project is to reconstruct the missing
bits of the source tree and get this tree to build. Most of
the important bit compile, but the strings will need to be 
copy/paste from MS-DOS 6.0 compiled binaries, some important
defines are missing and a few libaries are missing and will 
have to be rewritten from scratch.