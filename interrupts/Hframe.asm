START_PLACE  = 24d							; PLACE IN MEMORY TO PRINT REGISTERS
SHIFT        = 4d							; SHIFT BETWEEN REGISTERS IN MEMORY
CENTER_BYTE  = 2000d

;------------------------------------------------
; Spawns frame
;
; Entry:
; Exit:
; Destr:
;------------------------------------------------

SpawnFrame      proc
                push es ds di

                mov di, offset FrameParams

                push cs
                push cs
                pop es
                pop ds

                call    GetConsoleParams

                push    di                      ; SAVING CONSOLE POINTER

                call    CenterFrame
                call    DrawFrame

                pop     si                      ; SI - NEW CONSOLE POINTER

                add     di, bx                  ; DI += 2 * (LEN / 2) [CENTER OF THE TOP LINE]
                shr     di, 1
                shl     di, 1                   ; DI MUST BE EVEN

                call    FillFrameText

                pop di ds es
                ret
			    endp

;------------------------------------------------
; Fills frame with text
;
; Entry: SI - text pointer
;        DI - center of the top line
;        AH - COLOR
; Exit:
; Destr:
;------------------------------------------------

FillFrameText       proc

                    mov     al, ':'
                    call    PrintFrameLine      ; PRINTING HEADER

                    mov     di, CENTER_BYTE

                    call    CountTextStrings

                    mov     cx, bx              ; AMOUNT OF STRINGS TO PRINT
                    sub     cx, 1d              ; EXCEPT THE LAST

                    shr     bx, 1
                    imul    bx, bx, 160d
                    sub     di, bx              ; CENTER OF THE FIRST STRING'S LINE


PrintStrLoop:       mov     al, '\'
                    call    PrintFrameLine
                    add     di, 160d
                    loop    PrintStrLoop

                    mov     al, '$'
                    call    PrintFrameLine     ; PRINTING LAST STRING

			        ret
			        endp

;------------------------------------------------
; Counts amount of strings in text
;
; Entry: SI - text pointer
; Exit:  BX - amount of strings
; Destr:
;------------------------------------------------

CountTextStrings    proc
                    push    si
                    push    ax
                    push    cx

                    xor     bx, bx              ; BX = 0
                    xor     cx, cx              ; CX = 0
                    dec     cx                  ; CX = FFFF

StrCounterLoop:     mov     al, [si]
                    cmp     al, '\'

                    jne     NotNewStr           ; JUMP IF NOT NEW STRING
                    inc     bx                  ; AMOUNT OF STRINGS++

NotNewStr:          cmp     al, '$'
                    je      StrLoopExit         ; JUMP IF END
                    inc     si                  ; NEW SYMBOL
                    loop    StrCounterLoop

StrLoopExit:        inc     bx                      ; LAST STRING

                    pop     cx
                    pop     ax
                    pop     si
			        ret
			        endp

;------------------------------------------------
; Prints frame's line
;
; Entry: SI - header pointer
;        DI - center of the line
;        AH - COLOR
;        AL - BREAK SYMBOL
; Exit:
; Destr: SI
;------------------------------------------------

PrintFrameLine      proc
                    push    di
                    push    cx

                    call    MyStrlen

                    push    cx                  ; SAVING STRING LENGTH

                    shr     cx, 1
                    shl     cx, 1
                    sub     di, cx              ; DI -= 2 * (HEADER_LEN / 2)

                    pop     cx                  ; CX = STRING LEN

                    call    PrintText

                    pop     cx
                    pop     di
			        ret
			        endp

;------------------------------------------------
; Prints text in videomemory
;
; Entry: SI - text pointer
;        CX - text len
;        AH - COLOR
;        AL - Break symbol
; Exit:
; Destr: DI, SI, CX
;------------------------------------------------

PrintText       proc

Text:           lodsb
                stosw
                loop    Text

                add     si, 1d      ; SKIPPING BREAK SYMBOL

			    ret
			    endp

;------------------------------------------------
; Gets string length
;
; Entry: SI - string adress
;        AL - end symbol
; Exit:  CX - string len
; Destr: AL
;------------------------------------------------

MyStrlen    proc
            push    si
            push    bx
            push    di

            mov     di, si

            mov     bx, ds
            mov     es, bx              ; ES -> TEXT MEM

            xor     cx, cx
            dec     cx                  ; CX = FFFF

            repne   scasb               ; CX = -1 - LENGTH

            not     cx                  ; CX = 1 + LENGTH
            sub     cx, 1d              ; CX = LENGTH

            mov     bx, 0b800h
		    mov     es, bx              ; ES -> VIDEOMEM

            pop     di
            pop     bx
            pop     si
            ret
            endp


;------------------------------------------------
; Getting frame parameters from console
;
; Entry: DI - command line
; Exit:  BX - LENGTH
;        DX - WIDTH
;        AH - COLOR
;        DI - TEXT POINTER
; Destr:
;------------------------------------------------

GetConsoleParams    proc

                    call    GetDec             ; LENGTH
                    mov     bx, ax

                    call    SkipSpaces

                    call    GetDec             ; WIDTH
                    mov     dx, ax

                    call    SkipSpaces

                    call    GetHex             ; COLOR
                    mov     ah, al

                    call    SkipSpaces

                    call    GetStyle           ; STYLE

                    call    SkipSpaces

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
                    push    bx                      ; SAVING BX

                    xor     bx, bx                  ; BX = 0
                    xor     ah, ah

                    mov     al, [di]                ; FIRST SYMBOL

HexDigit:           cmp     ax, '0'
                    jl      HexAlpha
                    cmp     ax, '9'
                    jg      HexAlpha

                    sub     al, '0'                 ; AL = [0-9]

                    jmp     HexLoop

HexAlpha:           cmp     ax, 'a'
                    jl      ExitHexLoop
                    cmp     ax, 'f'
                    jg      ExitHexLoop

                    sub     al, 'a'
                    add     al, 10d                 ; AL = [A-F]

HexLoop:            imul    bx, bx, 16d            ; BX *= 16

                    xor     ah, ah                  ; AX = 00 AL
                    add     bx, ax                  ; BX += AL

                    inc     di                      ; DI++
                    mov     al, [di]

                    jmp     HexDigit
ExitHexLoop:

                    mov     ax, bx

                    pop     bx
			        ret
			        endp

;------------------------------------------------
; Getting style from console
;
; Entry: DI - command line
; Exit:  SI - char string
;        DI - string after style
; Destr:
;------------------------------------------------

GetStyle    proc
            push    ax

            mov     al, [di]

            cmp     al, '*'
            jne     DefaultStyle        ; JUMP IF STYLE IS NOT CUSTOM

            mov     si, di              ; CREATING CUSTOM STYLE STRING IN SI REGISTER
            add     si, 1d              ; SKIPPING *

            add     di, 10d             ; SKIPPING CUSTOM STYLE STRING

            pop     ax
            ret

DefaultStyle:

            call    GetDec              ; GETTING NUMBER OF DEFAULT STYLE

            sub     ax, 1d              ; DEFAULT STRING STARTS WITH ZERO SHIFT

            imul    ax, ax, 9d          ; GETTING MEMORY SHIFT

            mov     si, offset Style
            add     si, ax              ; USING MEMORY SHIFT

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
                push    bx                      ; SAVING BX

                xor     bx, bx                  ; BX = 0
                xor     ah, ah

                jmp     DecLoopCheck

DecLoop:        imul    bx, bx, 10d             ; BX *= 10

                inc     di                      ; DI++
                sub     al, '0'                 ; AL = DIGIT

                xor     ah, ah                  ; AX = 00 AL
                add     bx, ax                  ; BX += AL

DecLoopCheck:   mov     al, [di]
                cmp     ax, '0'
                jl      DecLoopExit
                cmp     ax, '9'
                jle     DecLoop
DecLoopExit:

                mov     ax, bx                  ; AX = BX

                pop     bx                      ; RETURNING BX

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

            push    ax                      ; SAVING REGISTERS
            push    cx

            xor     cx, cx              ; CX = 0
            dec     cx                  ; CX = FFFF

            mov     al, ' '

            repe    scasb               ; SKIP WHILE SPACE

            mov     al, [di]

            dec     di                  ; FIRST SYMBOL AFTER SPACE

            pop     cx                  ; RETURNING REGISTERS
            pop     ax

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

            push    dx                      ; SAVING WIDTH
            push    bx                      ; SAVING LENGTH
            push    si
            push    ax

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

            pop     ax
            pop     si
            pop     bx                      ; RETURNING WIDTH
            pop     dx                      ; RETURNING LENGTH

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
            push    ax
            push    bx
            push    dx
            push    si
            push    di

            push    bx
            mov     bx, 0b800h
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

            pop     di
            pop     si
            pop     dx
            pop     bx
            pop     ax
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

; --------------------------------------------------
; Saves registers in memory
;
; Entry:
; Exit:
; Destr:
; --------------------------------------------------

SaveRegisters		proc
                    push bp
                    mov bp, sp

					push di cx bx

                    mov cx, 13d                     ; REGISTERS AMOUNT
                    mov bx, bp                      ; FIRST PARAMETER INDEX
                    add bx, 4d

					mov di, offset FrameParams
					add di, START_PLACE

RegisterLoop:       mov ax, ss:[bx]
                    call OutputRegister
                    add bx, 2d
                    loop RegisterLoop

					pop bx cx di

                    pop bp
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

Style  db 201d, 205d, 187d, 186d, 0000d, 186d, 200d, 205d, 188d, \
          '/',   '-',  '\',  'I',   ' ',  'I',  '\',  '-',  '/', \
          'L',   'O',  'L',  'O',   ' ',  'O',  'L',  'O',  'L', \
           3d,    3d,   3d,   3d, 0000d,   3d,   3d,   3d,   3d

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
                           'ip     $' \


