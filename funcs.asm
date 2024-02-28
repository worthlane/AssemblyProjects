;------------------------------------------------
; Gets string length
;
; Entry: DI - string adress
; Exit:  AX - string len
; Destr:
;------------------------------------------------

MyStrlen    proc
            push di


            push cx

            mov al, '$'             ; COUNT TILL MEET

            xor cx, cx
            dec cx                  ; CX = FFFF

            repne scasb

            not cx
            sub cx, 1d

            mov ax, cx              ; AX = CX

            pop cx
            pop di
            ret
            endp

;------------------------------------------------
; Finds the first occurrence of char in the initial count bytes
;
; Entry: DI - pointer to the object to be examined
;        SI - bytes to search for
;        DX - max number of bytes to examine
; Exit:  AX - pointer to the location of the byte, or a null pointer
;             if no such byte is found
; Destr: CX
;------------------------------------------------

MyMemchr        proc
                push dx
                push si
                push di

                mov cx, dx
                mov ax, si

                repne scasb         ; WHILE (CX-- && ES:[DI++] != AL)

                mov ax, 0000d
                cmp cx, 0000d
                je  MemchrExit      ; RETURN NULL POINTER IF DID NOT FIND BYTE

                dec di
                mov ax, di

MemchrExit:     pop di
                pop si
                pop dx
                ret
                endp

;------------------------------------------------
; Copies the value into each of the first [count] characters
; of the object pointed to by [dest]
;
; Entry: DI - pointer to the object to fill
;        SI - fill byte
;        DX - number of bytes to fill
; Exit:  AX - pointer to the filled object
; Destr: CX
;------------------------------------------------

MyMemset    proc
            push dx
            push si
            push di

            mov cx, dx      ; AMOUNT OF BYTES
            mov ax, si

            rep stosw


            pop di          ; EXIT

            mov ax, di      ; RETURNED VALUE

            pop si
            pop dx
            ret
            endp

;------------------------------------------------
; Reinterprets the objects pointed to by [lhs] and [rhs] as arrays of
; unsigned char and compares the first count bytes of these arrays.
;
; Entry: DI - pointer to the memory buffers to compare [lhs]
;        SI - pointer to the memory buffers to compare [rhs]
;        DX - number of bytes to examine
; Exit:  AL - 0​ if all count bytes of lhs and rhs are equal.
;               Positive value if the first differing byte in lhs is
;               greater than the corresponding byte in rhs.
;               Negative value if the first differing byte in lhs is
;               less than the corresponding byte in rhs.
; Destr: CX
;------------------------------------------------

MyMemcmp    proc
            push dx
            push si
            push di

            mov cx, dx      ; AMOUNT OF BYTES

            repe cmpsb

            mov al, [di]
            mov ah, [si]

            pop di          ; EXIT
            pop si
            pop dx
            ret
            endp

;------------------------------------------------
; Copies count bytes from the object pointed to by [src] to the object pointed
; to by [dest]. Both objects are reinterpreted as arrays of unsigned char.
;
; Entry: DI - pointer to the memory location to copy to [dest]
;        SI - pointer to the memory location to copy from [src]
;        DX - number of bytes to copy
; Exit:  AX - [dest]
; Destr: CX
;------------------------------------------------

MyMemcpy    proc
            push dx
            push si
            push di

            mov cx, dx      ; AMOUNT OF BYTES

            rep movsb

            pop di          ; EXIT

            mov ax, di      ; RETURNED VALUE

            pop si
            pop dx
            ret
            endp

;------------------------------------------------
; Copies count bytes from the object pointed to by [src] to the object pointed
; to by [dest]. Both objects are reinterpreted as arrays of unsigned char.
;
; Entry: DI - pointer to the memory location to copy to [dest]
;        SI - pointer to the memory location to copy from [src]
;        DX - number of bytes to copy
; Exit:  AX - [dest]
; Destr: CX
;------------------------------------------------

MyMovmem    proc
            push dx
            push si
            push di

            cld             ; DF = 0
            cmp di, si
            jle SrcFirst

            std             ; DF = 1
            add di, dx
            add si, dx

SrcFirst:   mov cx, dx      ; AMOUNT OF BYTES

            rep movsb

            pop di          ; EXIT

            mov ax, di      ; RETURNED VALUE

            pop si
            pop dx
            ret
            endp
