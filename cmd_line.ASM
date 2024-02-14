.model tiny
.code
.286

org 80h            			 	; SHIFT 80h FROM PSP BEGIN

Len           db     ?      	; COMMAND LINE LENGTH
Str           db     ?	     	; COMMAND LINE STRING

org 100h

start:	mov ax, 0900h
		mov dx, offset Str
		int 21h
		jmp Skip

Skip:	mov ax, 4c00h
		int 21h

end start
