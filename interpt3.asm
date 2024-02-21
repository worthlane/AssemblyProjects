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

IsHotkey:   call PlaceStdFrame

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

;------------------------------------------------
; Places standart frame
;
; Entry:
; Exit:
; Destr:
;------------------------------------------------

PlaceStdFrame   proc
                push es ds

                push VMEM
                pop es

                push cs
                pop ds

                cld

                mov dx, FRAME_WIDTH
                mov bx, FRAME_LEN
                mov si, offset Style
                mov ah, FRAME_COLOR
                call CenterFrame
                call DrawFrame

                pop ds es 
			    ret
			    endp

;------------------------------------------------
; Frame centering
;
; Entry: dx - width
;        bx - length
; Exit:  di - frame's left upper corner position
; Destr:
;------------------------------------------------

CenterFrame proc

            push    dx bx si ax

            shr     dx, 1                   ; DX = WIDTH / 2
            shr     bx, 1                   ; BX = LENGTH / 2

            mov     di, 40d
            mov     si, 12d

            sub     si, dx                  ; SI = MIDDLE LINE - WIDTH / 2
            sub     di, bx                  ; DI = MIDDLE COLUMN - LENGTH / 2

            imul    ax, si, LINE_LEN
            add     ax, di

            shl     ax, 1                   ; AX = (MIDDLE LINE - WIDTH / 2, MIDDLE COLUMN - LENGTH / 2)

            mov     di, ax

            pop     ax si bx dx

			ret
			endp

;------------------------------------------------
; Draw frame
;
; Entry: di - first byte position
;        si - char string adress
;        dx - width
;        bx - length
;        ah - color
; Exit: None
; Destr: CX, ES
;------------------------------------------------

DrawFrame   proc
            push    ax bx dx si di

            push    bx
            mov     bx, VMEM
		    mov     es, bx                  ; ES -> VIDEOMEM
            pop     bx

            sub     dx, 2d                  ; AMOUNT OF FRAME'S BODY LINES
            sub     bx, 2d                  ; AMOUNT OF LINE'S BODY SYMBOLS

            mov     cx, bx                  ; CX = BX
            call    DrawLine                ; PRINT LINE WITH (BX + 2) LENGTH

            add     di, 156d
            sub     di, bx
            sub     di, bx                  ; JUMP TO THE NEXT LINE

            mov     cx, dx                  ; PRINTING FRAME'S BODY WITH (DX - 2) WIDTH

body:       push    cx                      ; SAVING BODY WIDTH COUNTER

            mov     cx, bx
            call    DrawLine                ; PRINT LINE WITH (BX + 2) LENGTH

            pop     cx                      ; GETTING BODY WIDTH COUNTER
            sub     si, 3d                  ; NEXT LINE WILL HAVE SAME SYMBOLS, AS PREVIOUS

            add     di, 156d
            sub     di, bx
            sub     di, bx                  ; JUMP TO THE NEXT LINE
            loop    body

            add     si, 3d                  ; GETTING NEW SYMBOLS FOR NEXT LINE

            mov     cx, bx
            call    DrawLine                ; PRINT LINE WITH (BX + 2) LENGTH

            pop     di si dx bx ax

			ret
			endp


;------------------------------------------------
; Draw line
;
; Entry: di - first byte position
;        si - char string adress
;        cx - line length - 2
;        ah - color
; Exit: None
; Destr: AX, CX, DI
;------------------------------------------------

DrawLine    proc

            lodsb                   ; GET FIRST STRING SYMBOL
            stosw                   ; PLACE LINE'S START

            lodsb                   ; GET SECOND STRING SYMBOL
            rep stosw               ; PLACE LINE'S BODY

            lodsb                   ; GET LAST STRING SYMBOL
            stosw                   ; PLACE LINE'S ENDING

			ret
			endp


Style  db 201d, 205d, 187d, 186d, 0000d, 186d, 200d, 205d, 188d

EOP:

end         Start
