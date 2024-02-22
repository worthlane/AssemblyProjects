.model tiny
.code
.286

org 100h

START_PLACE = 24d							; PLACE IN MEMORY TO PRINT REGISTERS
SHIFT       = 4d							; SHIFT BETWEEN REGISTERS IN MEMORY

start:	mov ax, 0FFFFh
		mov bx, 1111h
		mov cx, 2222h
		mov dx, 3333h
		mov si, 4444h
		mov di, 5555h

		in al, 60h
		cmp al, 1d
		jne start
		ret

end start
