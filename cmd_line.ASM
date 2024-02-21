.model tiny
.code
.286

org 100h

START_PLACE = 24d							; PLACE IN MEMORY TO PRINT REGISTERS
SHIFT       = 4d							; SHIFT BETWEEN REGISTERS IN MEMORY

start:	call SaveRegisters

		mov dx, offset FrameParams
		mov ah, 09h
		int 21h

		mov ax, 4c00h
		int 21h

; --------------------------------------------------
; Saves registers in memory
;
; Entry:
; Exit:
; Destr:
; --------------------------------------------------

SaveRegisters		proc
					push di

					mov di, offset FrameParams
					add di, START_PLACE

					call OutputRegister

					mov ax, bx
					call OutputRegister

					mov ax, cx
					call OutputRegister

					mov ax, dx
					call OutputRegister

					mov ax, si
					call OutputRegister

					mov ax, di
					call OutputRegister

					mov ax, bp
					call OutputRegister

					mov ax, sp
					call OutputRegister

					mov ax, ds
					call OutputRegister

					mov ax, es
					call OutputRegister

					mov ax, ss
					call OutputRegister

					mov ax, cs
					call OutputRegister

					pop di
					ret
					endp

; --------------------------------------------------
; Prints data from register in memory
;
; Entry: DI - place, AX - register
; Exit:
; Destr: DI
; --------------------------------------------------

OutputRegister		proc
					push ax ax ax ax

					shr ax, 12
					and ax, 000Fh
					call PrintDigit

					pop ax
					shr ax, 8
					and ax, 000Fh
					call PrintDigit

					pop ax
					shr ax, 4
					and ax, 000Fh
					call PrintDigit

					pop ax
					and ax, 000Fh
					call PrintDigit

					add di, SHIFT							; JUMPING ON THE NEXT REGISTER

					pop ax

					ret
					endp

; --------------------------------------------------
; Prints digit in memory from HEX number
;
; Entry: DI - memory adress, AX - number
; Exit:
; Destr: DI
; --------------------------------------------------

PrintDigit			proc
					push si es

					push cs
					pop es

					mov si, offset RegOutput
					add si, ax

					mov al, es:[si]

					stosb

					pop es si
					ret
					endp

RegOutput   db '0123456789ABCDEF'

FrameParams db '13 17 4e 1 registers:', \
                           'ax     \', \
                           'bx     \', \
                           'cx     \', \
                           'dx     \', \
                           'si     \', \
                           'di     \', \
                           'bp     \', \
                           'sp     \', \
                           'ds     \', \
                           'es     \', \
                           'ss     \', \
                           'cs     \', \
                           'ip     $', \

end start
