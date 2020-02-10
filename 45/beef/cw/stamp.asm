;*	* CW : Header for stamp
;*	* Contains version #

_data	segment byte public 'data'			 
dgroup	GROUP _data					 
assume	cs:dgroup					 
public	sdVerCow,szVerCow,szVerCowMax

sdVerCow:
szVerCow:

	DB	'CW Version 2.22'
	DB	13, 10

;********************************
