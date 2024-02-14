.model tiny
.code
.286

org 82h

argv    db      ?

org 100h

LINE_LEN = 80d

start:  mov di, offset argv

        call GetConsoleParams

        call CenterFrame

        call DrawFrame

		mov ax, 4c00h			        ; EXIT
		int 21h


;------------------------------------------------
; Getting frame parameters from console
;
; Entry: DI - command line
; Exit:  BX - LENGTH
;        DX - WIDTH
;        AH - COLOR
; Destr:
;------------------------------------------------

GetConsoleParams    proc

                    call GetDec             ; LENGTH
                    mov bx, ax

                    call SkipSpaces

                    call GetDec             ; WIDTH
                    mov dx, ax

                    call SkipSpaces

                    call GetHex             ; COLOR
                    mov ah, al

                    call SkipSpaces

                    call GetStyle           ; STYLE

			        ret
			        endp

;------------------------------------------------
; Getting hexadecimal number (FORMAT: ...h)
;
; Entry: DI - command line
; Exit:  AX - number
; Destr:
;------------------------------------------------

GetHex              proc
                    push bx                     ; SAVING BX

                    xor bx, bx                  ; BX = 0
                    xor ah, ah

                    mov al, [di]

HexDigit:           cmp ax, '0' - 1d
                    jle HexAlpha
                    cmp ax, '9'
                    jg  HexAlpha

                    sub al, '0'

                    jmp HexLoop

HexAlpha:           cmp ax, 'a' - 1d
                    jle ExitHexLoop
                    cmp ax, 'f'
                    jg  ExitHexLoop

                    sub al, 'a'
                    add al, 10d

HexLoop:            imul bx, bx, 16d            ; BX *= 16

                    xor ah, ah                  ; AX = 00 AL
                    add bx, ax                  ; BX += AL

                    inc di                      ; DI++
                    mov al, [di]

                    jmp HexDigit
ExitHexLoop:

                    mov ax, bx

                    pop bx
			        ret
			        endp

;------------------------------------------------
; Getting style from console
;
; Entry: DI - command line
; Exit:  SI - char string
; Destr:
;------------------------------------------------

GetStyle    proc
            push ax

            mov al, [di]

            cmp al, '*'
            jne DefaultStyle

            mov si, di
            add si, 1d

            add di, 10d

            pop ax
            ret

DefaultStyle:

            call GetDec

            sub ax, 1d

            imul ax, ax, 9d

            mov si, offset Style
            add si, ax

            pop ax
			ret
			endp

;------------------------------------------------
; Getting decimal number from command line
;
; Entry: DI - command line
; Exit:  AX - number
; Destr:
;------------------------------------------------

GetDec          proc
                push bx                     ; SAVING BX

                xor bx, bx                  ; BX = 0
                xor ah, ah

                jmp DecLoopCheck

DecLoop:        imul bx, bx, 10d            ; BX *= 10

                inc di                      ; DI++
                sub al, '0'                 ; AL = DIGIT

                xor ah, ah                  ; AX = 00 AL
                add bx, ax                  ; BX += AL

DecLoopCheck:   mov al, [di]
                cmp ax, '0' - 1
                jle DecLoopExit
                cmp ax, '9'
                jle DecLoop
DecLoopExit:

                mov ax, bx                  ; AX = BX

                pop bx                      ; RETURNING BX

			    ret
			    endp

;------------------------------------------------
; Skipping spaces from command line
;
; Entry: DI - command line
; Exit:
; Destr:
;------------------------------------------------

SkipSpaces  proc

            push ax                 ; SAVING REGISTERS
            push cx

            xor cx, cx              ; CX = 0
            dec cx                  ; CX = FFFF

            mov al, ' '

            repe scasb              ; SKIP WHILE SPACE

            mov al, [di]

            dec di                  ; FIRST SYMBOL AFTER SPACE

            pop cx                  ; RETURNING REGISTERS
            pop ax

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

            push dx                     ; SAVING WIDTH
            push bx                     ; SAVING LENGTH
            push si
            push ax

            shr dx, 1                   ; DX = WIDTH / 2
            shr bx, 1                   ; BX = LENGTH / 2

            mov di, 40d
            mov si, 12d

            sub si, dx                  ; SI = MIDDLE LINE - WIDTH / 2
            sub di, bx                  ; DI = MIDDLE COLUMN - LENGTH / 2

            imul ax, si, LINE_LEN
            add ax, di

            shl ax, 1                   ; AX = (MIDDLE LINE - WIDTH / 2, MIDDLE COLUMN - LENGTH / 2)

            mov di, ax

            pop ax
            pop si
            pop bx                      ; RETURNING WIDTH
            pop dx                      ; RETURNING LENGTH

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
; Destr: AX, CX, DI, BX, DX, SI, ES
;------------------------------------------------

DrawFrame   proc

            push bx
            mov bx, 0b800h
		    mov es, bx
            pop bx

            sub dx, 2d                  ; AMOUNT OF FRAME'S BODY LINES
            sub bx, 2d                  ; AMOUNT OF LINE'S BODY SYMBOLS

            mov cx, bx                  ; CX = BX
            call DrawLine               ; PRINT LINE WITH (BX + 2) LENGTH

            add di, 156d
            sub di, bx
            sub di, bx                  ; JUMP TO THE NEXT LINE

            mov cx, dx                  ; PRINTING FRAME'S BODY WITH (DX - 2) WIDTH

body:       push cx                     ; SAVING BODY WIDTH COUNTER

            mov cx, bx
            call DrawLine               ; PRINT LINE WITH (BX + 2) LENGTH

            pop cx                      ; GETTING BODY WIDTH COUNTER
            sub si, 3d                  ; NEXT LINE WILL HAVE SAME SYMBOLS, AS PREVIOUS

            add di, 156d
            sub di, bx
            sub di, bx                  ; JUMP TO THE NEXT LINE
            loop body

            add si, 3d                  ; GETTING NEW SYMBOLS FOR NEXT LINE

            mov cx, bx
            call DrawLine               ; PRINT LINE WITH (BX + 2) LENGTH

			ret
			endp


;------------------------------------------------
; Draw line
;
; Entry: di - first byte position
;        si - char string adress
;        cx - line length - 2
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


Style  db 201d, 205d, 187d, 186d, 0000d, 186d, 200d, 205d, 188d, \
          '/',   '-',  '\',  'I',   ' ',  'I',  '\',  '-',  '/', \
          'L',   'O',  'L',  'O',   ' ',  'O',  'L',  'O',  'L'

end start
