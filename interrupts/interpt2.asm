.286
.model tiny
.code
org 100h

VMEM = 0b800h

Start:      push 0
            pop es
            mov bx, 9 * 4

            cli
            mov es:[bx], offset New09       ; es:[bx] ->func start

            push cs                         ; pushing standart segment adress
            pop ax

            mov es:[bx + 2], ax             ; es:[bx+2] -> standart segment
            sti

            mov ax, 3100h
            mov dx, offset EOP
            shr dx, 4                       ; because of memory in paragraphs
            inc dx

            int 21h

New09       proc
            push ax bx es

            push VMEM
            pop es
            mov bx, (80 * 5 + 40) * 2
            mov ah, 4eh

            in al, 60h
            mov es:[bx], ax

            in al, 61h
            or al, 80h
            out 61h, al
            and al, not 80h
            out 61h, al

            mov al, 20h
            out 20h, al
            pop es bx ax
            iret
            endp

EOP:

end         Start
