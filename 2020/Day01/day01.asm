; Aoc 2020, Day 1: Report Repair
; Author: Chi-Kit Pao
;
; Commands:
; nasm -f elf64 day01.asm -o day01.o
; ld day01.o -o day01
; ./day01 input.txt
;
; Output:
; Advent of code 2020, Day 1
; Question 1: Find the two entries that sum to 2020; what do you get if you multiply them together?
; Answer: 864864
; Question 2: What is the product of the three entries that sum to 2020?
; Answer: 281473080
;
; Time usage shown via command "time".
; real	0m0,001s
; user	0m0,001s
; sys	0m0,000s


global _start

section .data
    debug db "Debug: "
    debug_len equ $ - debug
    newline db 10
    error_msg db "Could not open file", 10
    error_len equ $ - error_msg
    hello_msg db "Advent of code 2020, Day 1", 10
    hello_len equ $ - hello_msg
    answer_msg db "Answer: "
    answer_len equ $ - answer_msg
    question1_msg db "Question 1: Find the two entries that sum to 2020; what do you get if you multiply them together?",10
    question1_len equ $ - question1_msg
    answer1 dq 0
    question2_msg db "Question 2: What is the product of the three entries that sum to 2020?",10
    question2_len equ $ - question2_msg
    answer2 dq 0
    number_count dq 0   ; count of "numbers"

section .bss
    buffer resb 4096
    number_text resb 32
    numbers resq 256


section .text

print_number:
    ; ------------------------------------------------------------
    ; Print non negative number from rax
    ; ------------------------------------------------------------
    cmp rax, 0
    mov rdi, number_text + 31  ; write backward
    jg print_positive_number

    ; Handle non-positive numbers
    mov byte [rdi], '0'
    jmp print_number_impl

print_positive_number:
    mov rdx, rax
convert_loop:
    xor rdx, rdx    ; clear flags
    mov rbx, 10
    div rbx
    add dl, '0'
    mov byte [rdi], dl
    test rax, rax
    je print_number_impl
    dec rdi
    jmp convert_loop

print_number_impl:
    ; Really print the number
    mov rax, 1  ; sys_write
    mov rsi, rdi ; buffer
    mov rdx, number_text + 32 ; length
    sub rdx, rdi
    mov rdi, 1  ; stdout
    syscall
    ret

print_endline:
    mov rax, 1  ; sys_write
    mov rsi, newline ; buffer
    mov rdx, 1  ; length
    mov rdi, 1  ; stdout
    syscall
    ret

print_debug:
    push rsi
    push rdx
    push rdi
    push rbx
    push rcx

    push rax
    mov rax, 1  ; sys_write
    mov rsi, debug ; buffer
    mov rdx, debug_len  ; length
    mov rdi, 1  ; stdout
    syscall
    pop rax

    call print_number

    call print_endline

    pop rcx
    pop rbx
    pop rdi
    pop rdx
    pop rsi

    ret

_start:
    ; ------------------------------------------------------------
    ; Open file
    ; int open(const char *filename, int flags, ...)
    ; syscall: open = 2
    ; ------------------------------------------------------------

    ; Get filename from command line
    mov rax, [rsp]          ; argc
    cmp rax, 2
    jne exit

    mov rdi, [rsp + 16]    ; argv[1]
    xor rsi, rsi           ; O_RDONLY = 0
    mov rax, 2             ; sys_open
    syscall

    ; Check for error
    cmp rax, 0
    jl open_error

    mov r12, rax           ; save file descriptor

    ; ------------------------------------------------------------
    ; Read file
    ; ------------------------------------------------------------

read_file:
    mov rax, 0             ; sys_read
    mov rdi, r12            ; file descriptor
    mov rsi, buffer
    mov rdx, 4096
    syscall

    cmp rax, 0
    jle close_file         ; EOF or error

    ; rax = number of bytes read
    mov r13, rax

    mov rax, 1  ; sys_write
    mov rsi, hello_msg  ; buffer
    mov rdx, hello_len  ; length
    mov rdi, 1  ; stdout
    syscall

    ; ------------------------------------------------------------
    ; Program logic
    ; ------------------------------------------------------------
    xor rcx, rcx    ; buffer position
    xor rdx, rdx    ; temporary number
parse_loop:
    ; Remark: only non-negative numbers followed by newline
    cmp rcx, r13
    jge find_answers

    mov bl, [buffer + rcx]
    cmp bl, 10
    jne check_digit
    push rcx
    mov rax, 8
    mov rcx, [number_count]
    imul rcx, rax
    mov rdi, numbers
    add rdi, rcx
    mov [rdi], rdx
    inc qword [number_count]
    pop rcx
    inc rcx

    xor rdx, rdx
    jmp parse_loop

check_digit:
    cmp bl, '0'
    jl close_file
    cmp bl, '9'
    jg close_file
    sub bl, '0'
    xor rax, rax
    mov eax, 10
    imul edx, eax
    add rdx, rbx
    inc rcx
    jmp parse_loop

find_answers:
    mov r9, [number_count] ; number count * 8 per qword
    imul r9, 8

; find_answer1:
    xor rcx, rcx
iterate_rcx1:
    cmp rcx, r9
    jge find_answer2
    mov r10, [numbers + rcx]    ; outer loop value

    mov rdx, rcx
    add rdx, 8
iterate_rdx1:
    cmp rdx, r9
    jge increment_rcx1

    ; Was using r11, but not working, since syscall overwrites r11 with the contents of rflags.
    mov r8, [numbers + rdx]    ; inner loop value
    mov rax, r8
    add rax, r10
    cmp rax, 2020
    jne increment_rdx1
    imul r10, r8
    mov [answer1], r10
    jmp find_answer2
increment_rdx1:
    add rdx, 8
    jmp iterate_rdx1
increment_rcx1:
    add rcx, 8
    jmp iterate_rcx1

find_answer2:
    xor rcx, rcx
iterate_rcx2:
    cmp rcx, r9
    jge find_answer2
    mov r10, [numbers + rcx]    ; outer loop value

    mov rdx, rcx
    add rdx, 8
iterate_rdx2:
    cmp rdx, r9
    jge increment_rcx2

    ; Was using r11, but not working, since syscall overwrites r11 with the contents of rflags.
    mov r8, [numbers + rdx]    ; middle loop value
    mov rax, r8
    add rax, r10
    cmp rax, 2020
    jg increment_rdx2

    mov rbx, rdx
    add rbx, 8
iterate_rbx2:
    cmp rbx, r9
    jge increment_rdx2

    mov r14, [numbers + rbx] ; inner loop value
    mov rax, r14
    add rax, r8
    add rax, r10
    cmp rax, 2020
    jne increment_rbx2
    imul r10, r8
    imul r10, r14
    mov [answer2], r10
    jmp print_answer1
increment_rbx2:
    add rbx, 8
    jmp iterate_rbx2
increment_rdx2:
    add rdx, 8
    jmp iterate_rdx2
increment_rcx2:
    add rcx, 8
    jmp iterate_rcx2


print_answer1:
    mov rax, 1  ; sys_write
    mov rsi, question1_msg  ; buffer
    mov rdx, question1_len  ; length
    mov rdi, 1  ; stdout
    syscall
    mov rax, 1  ; sys_write
    mov rsi, answer_msg  ; buffer
    mov rdx, answer_len  ; length
    mov rdi, 1  ; stdout
    syscall
    mov rax, [answer1]
    call print_number
    call print_endline

print_answer2:
    mov rax, 1  ; sys_write
    mov rsi, question2_msg  ; buffer
    mov rdx, question2_len  ; length
    mov rdi, 1  ; stdout
    syscall
    mov rax, 1  ; sys_write
    mov rsi, answer_msg  ; buffer
    mov rdx, answer_len  ; length
    mov rdi, 1  ; stdout
    syscall
    mov rax, [answer2]
    call print_number
    call print_endline


    ; ------------------------------------------------------------
    ; Close file
    ; ------------------------------------------------------------

close_file:
    mov rax, 3              ; sys_close
    mov rdi, r12
    syscall

exit:
    mov rax, 60             ; sys_exit
    xor rdi, rdi
    syscall

open_error:
    mov rax, 1              ; sys_write
    mov rdi, 2              ; stderr
    mov rsi, error_msg
    mov rdx, error_len
    syscall

    mov rax, 60
    mov rdi, 1
    syscall
