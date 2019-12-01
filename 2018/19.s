global _start

        section .text

;;; esi is register 0, eax is register 1, ebx is 3, ecx is 4 and edx is 5
_start:
        mov eax, 0
        mov ebx, 0
        mov ecx, 0
        mov edx, 0
        mov esi, 0
_program:
        ;; #ip 2 => register 2 is the program counter
        jmp _skip               ; addi 2 16 2
_program1:
        mov eax, 1              ; seti 1 1
_program2:
        mov ebx, 3              ; seti 1 3
        imul edx, eax, ebx      ; mulr 1 3 5
        ; eqrr 5 4 5
        ; addr 5 2 2
        jmp _skipone
        add esi, eax            ; addr 1 0 0
_skipone:
        add ebx, 1              ; addi 3 1 3
        ; gtrr 3 4 5
        ; addr 2 5 2
        jmp _program2           ; seti 2 2
        add eax, 1              ; addi 1 1 1
        ; gtrr 1 4 5
        ; addr 5 2 2
        jmp _program1
        jmp _end
_skip:
        addi 4 2 4
        mulr 4 4 4
        mulr 2 4 4
        muli 4 11 4
        addi 5 6 5
        mulr 5 2 5
        addi 5 19 5
        addr 4 5 4
        addr 2 0 2
        jmp _program            ; seti 0 2
        setr 2 5
        mulr 5 2 5
        addr 2 5 5
        mulr 2 5 5
        muli 5 14 5
        mulr 5 2 5
        addr 4 5 4
        seti 0 0
        seti 0 2
_end:
