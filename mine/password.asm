.286
.model tiny
.code
org 100h

FALSE = 0000h
TRUE  = not FALSE

NEW_LINE_ASCII = 13d
PASSWORD_HASH  = 12123d

CIPHER_SHIFT   = 78d

Start:      mov ah, 09h
            mov dx, offset INTRO_MESSAGE            ; INTRO MSG OUTPUT
            int 21h

            mov di, offset BUFFER
            call GetPassword                        ; GETS PASSWORD AND PLACES IT IN BUFFER

            mov si, offset CORRECT_PASS
            mov di, offset BUFFER
            call CheckPassword                      ; COMPARES BUFFER WITH CORRECT PASSWORD

            cmp bx, TRUE
            je Success                              ; SUCCESS PROMPT IF BUFFER IS CORRECT

            mov di, offset FAILURE_FRAME            ; FAILURE PROMPT IF BIFFER IS NOT CORRECT
            jmp OpenFrame

Success:    mov di, offset SUCCESS_FRAME

OpenFrame:  call SpawnFrame

            mov     ax, 4c00h			    ; EXIT
		    int     21h

;------------------------------------------------
; Gets symbols from keyboard input
;
; Entry:  ds:di - starting address of the input buffer
; Exit:
; Destr: DI
;------------------------------------------------

GetPassword     proc

                mov ah, 06h

InputLoop:      mov dl, 0ffh                            ; CONSOLE INPUT MODE
                int 21h

                jz InputLoop

                mov dl, al                              ; PRINT SYMBOL IN CONSOLE (TO SEE INPUT IN CONSOLE)
                int 21h

                add al, CIPHER_SHIFT                    ; SYMBOL IS ENCRYPTED

                mov ds:[di], al                         ; MOVE SYMBOL IN BUFFER
                inc di

                cmp al, NEW_LINE_ASCII + CIPHER_SHIFT   ; STOP INPUT WHEN [ENTER]
                jne InputLoop

                ret
                endp

;------------------------------------------------
; Check if passport is correct
;
; Entry:  ds:si - address of the correct password
;         ds:di - address of the input buffer
; Exit:   bx - TRUE if password is the same, FALSE if not
; Destr: DI, SI
;------------------------------------------------

CheckPassword   proc

                mov bx, TRUE

CheckLoop:      mov al, ds:[si]                     ; AL - EXPECTED SYMBOL
                mov cl, ds:[di]                     ; CL - CURRENT SYMBOL (NO CIPHER)

                cmp cl, al
                jne NotSame                         ; IF SYMBOLS ARE NOT SAME - SWITCH BX TO FALSE

                cmp al, NEW_LINE_ASCII + CIPHER_SHIFT
                je CheckExit                        ; EXIT IF NO ELSE SYMBOLS EXPECTED

                inc si
                inc di

                jmp CheckLoop

NotSame:        mov bx, FALSE

CheckExit:      ret
                endp


include frameH.asm

INTRO_MESSAGE   db      'Please, enter the password: $'

SUCCESS_FRAME   db      '30 6 28 1 SUCCESS:ACCESS GRANTED\GRATZ$'
FAILURE_FRAME   db      '30 6 4e 1 FAILURE:ACCESS DENIED\NOOB LMAO EZZZZ$'

CORRECT_PASS    db      'm' + CIPHER_SHIFT, 'o' + CIPHER_SHIFT, 'd' + CIPHER_SHIFT, 'e' + CIPHER_SHIFT, ':' + CIPHER_SHIFT, \
                        'd' + CIPHER_SHIFT, 'e' + CIPHER_SHIFT, 'k' + CIPHER_SHIFT, 'o' + CIPHER_SHIFT, NEW_LINE_ASCII + CIPHER_SHIFT

BUFFER          db      '00000000000000000000$'

end         Start
