;*	* CW : Cover.asm : Code Coverage kludge module

_data	segment byte public 'data'			 
dgroup	GROUP _data					 
assume	cs:dgroup					 
assume	ds:dgroup					 

	public	crefCow
crefCow	DW	0

_data	ends


core	segment byte public 'code'
assume	cs:core

	public	GetCodeHandle
GetCodeHandle:
	public	GetCodeInfo
GetCodeInfo:
	int	3

core	ends

;********************************

	end
