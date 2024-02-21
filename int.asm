.286
.model tiny
.code
org 100h

VMEM     = 0b800h
LINE_LEN = 80d

FRAME_WIDTH = 20d
FRAME_LEN   = 40d
FRAME_COLOR = 4eh

Start:      mov ax, 3509h               ; GETS VECTOR FROM STD INTERRUPTION
            int 21h

            mov Old090fs, bx            ;   \
            mov bx, es                  ;   |   AUTOGENERATING FAR JUMP
            mov Old09Seg, bx            ;   /

            push 0
            pop es
            mov bx, 9 * 4               ;  GET 9th POSITION IN INTERRUIPTION TABLE

            cli                         ;  \
            mov es:[bx], offset Int09   ;   |
                                        ;   |
            push cs                     ;   |   ADD CUSTOM INTERRUPTION IN TABLE
            pop ax                      ;   |
                                        ;   |
            mov es:[bx + 2], ax         ;   |
            sti                         ;  /

            mov ax, 3100h               ; TERMINATE AND STAY RESIDENT

            mov dx, offset EOP          ;   SET MEMORY FOR INTERRUPTION
            shr dx, 4                   ;   GET PARAGRAPHS
            inc dx                      ;   +1 PARAGRAPH FOR MORE SAFETY

            int 21h

; =============================================
;
;   Start of custom interruption
;
; =============================================

Int09       proc
            push ax bx es

            in al, 60h                  ; GET INPUT FROM KEYBOARD PORT
            cmp al, 3bh                 ; F1 IS HOTKEY

            jne NotHotkey

IsHotkey:   call SpawnFrame

            in al, 61h                  ;   \
            or al, 80h                  ;    |
            out 61h, al                 ;    | BLINK TO ALLOW KEYBOARD INPUT
            and al, not 80h             ;    |
            out 61h, al                 ;   /

            mov al, 20h                 ; LEAVE INTERRUPTION
            out 20h, al

            pop es bx ax

            iret

NotHotkey:  pop es bx ax

db          0Eah
Old090fs    dw 0
Old09Seg    dw 0                    ; jmp on Old090Seg:Old090fs

            endp

include Hframe.asm

EOP:

end         Start
