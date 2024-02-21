.286
.model tiny
.code
org 100h

VMEM = 0b800h

Start:      push VMEM
            pop es
            mov bx, (80 * 5 + 40) * 2       ; (5, 40)
            mov ah, 4eh                     ; COLOR

Next:       in al, 60h                      ; GET INFO FROM PORT 60
            mov es:[bx], ax                 ; SHOW AX IN VMEM

            cmp al, 11d                     ; SCAN CODE '0'
            jne Next
            ret

end         Start
