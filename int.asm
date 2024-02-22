.286
.model tiny
.code
org 100h

FALSE = 0000h
TRUE  = not FALSE

VMEM     = 0b800h
LINE_LEN = 80d

FRAME_WIDTH = 20d
FRAME_LEN   = 40d
FRAME_COLOR = 4eh

Start:      call Init09
            call Init08

            mov ax, 3100h               ; TERMINATE AND STAY RESIDENT

            mov dx, offset EOP          ;   SET MEMORY FOR INTERRUPTION
            shr dx, 4                   ;   GET PARAGRAPHS
            inc dx                      ;   +1 PARAGRAPH FOR MORE SAFETY

            int 21h

; =============================================
;
;   Start of custom interruption 09
;
; =============================================

New09       proc
            push ax bx es

            in al, 60h                  ; GET INPUT FROM KEYBOARD PORT

            cmp al, 3bh                 ; F1 IS HOTKEY
            jne NotHotkey

IsHotkey:   mov cs:HotKeyFlag, TRUE

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

; =============================================
;
;   Start of custom interruption 08
;
; =============================================

New08       proc

            cmp cs:HotKeyFlag, FALSE
            je NoFrame

            push    cs ss es ds sp bp di si dx cx bx ax
            call    SaveRegisters
            add     sp, 24d

            call SpawnFrame

NoFrame:

db          0Eah
Old080fs    dw 0
Old08Seg    dw 0                    ; jmp on Old080Seg:Old080fs

            endp

; --------------------------------------------
;   Initialization 9th interruption
; --------------------------------------------

Init09      proc
            mov ax, 3509h               ; GETS VECTOR FROM STD INTERRUPTION
            int 21h

            mov Old090fs, bx            ;   \
            mov bx, es                  ;   |   AUTOGENERATING FAR JUMP
            mov Old09Seg, bx            ;   /

            push 0
            pop es
            mov bx, 9 * 4               ;  GET 9th POSITION IN INTERRUIPTION TABLE

            cli                         ;  \
            mov es:[bx], offset New09   ;   |
                                        ;   |
            push cs                     ;   |   ADD CUSTOM INTERRUPTION IN TABLE
            pop ax                      ;   |
                                        ;   |
            mov es:[bx + 2], ax         ;   |
            sti                         ;  /

            ret
            endp

; --------------------------------------------
;   Initialization of 9th interruption
; --------------------------------------------

Init08      proc
            mov ax, 3508h               ; GETS VECTOR FROM STD INTERRUPTION
            int 21h

            mov Old080fs, bx            ;   \
            mov bx, es                  ;   |   AUTOGENERATING FAR JUMP
            mov Old08Seg, bx            ;   /

            push 0
            pop es
            mov bx, 8 * 4               ;  GET 8th POSITION IN INTERRUIPTION TABLE

            cli                         ;  \
            mov es:[bx], offset New08   ;   |
                                        ;   |
            push cs                     ;   |   ADD CUSTOM INTERRUPTION IN TABLE
            pop ax                      ;   |
                                        ;   |
            mov es:[bx + 2], ax         ;   |
            sti                         ;  /

            ret
            endp

include Hframe.asm

HotKeyFlag  db FALSE

EOP:

end         Start
