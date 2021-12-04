; Architecture: x86_64
; System: Linux
; Notes: 
;   <input> file must have LF line endings!
;   Important keep one blank line at the end of the file!
; Useful info:
;   https://blog.rchapman.org/posts/Linux_System_Call_Table_for_x86_64/
;   set disassembly-flavor intel

section .text
global _start

_start:
  ; if argc != 2 print usage and exit
  pop   rax ; argc
  cmp   rax, 2
  jne   .print_usage

  ; open file argv[1]
  .open_file:
    pop  rdi ; argv[0]
    pop  rdi ; argv[1]

    mov    rax, SYS_OPEN
    mov    rsi, O_RDONLY ; flags (O_RDONLY)
    mov    rdx, 0666     ; mode 
    syscall

    cmp rax, 0
    jl .print_open_file_error
  ; rax = fd

  mov  rdi, rax ; fd
  call prepare_matrix

  .find_oxygen_generator_rating:

    ; for (i = 0; i < matrix_width; i++)
    xor   r10, r10 ; i = 0
    mov   r9, [rsp+32] ; matrix_height
    .loop_0:

      .count_zeros_call_0:
        mov   rdi, [rsp]  ; vector_address
        mov   rsi, r9     ; matrix_height
        mov   rdx, r10    ; column_index
        call  count_zeros
      ; rax = zeros
      mov rbx, r9
      sub rbx, rax
      ; rbx = ones

      .keep_if_call_0:
        mov   rdi, [rsp] ; vector_address
        mov   rsi, r9    ; matrix_height
        mov   rdx, r10   ; column_index

        cmp rbx, rax ; zeros <= ones
        setge cl
        add cl, '0' ; keep_value = '0' + (zeros <= ones)

        call keep_if
        mov r9, rax

      .next_0:
        inc r10
        cmp r10, [rsp+24] ; if (i < matrix_width) goto .loop
        jl .loop_0

      .done_0:
        mov   rdi, [rsp] ; vector_address
        mov rdi, [rdi]
        call read_binary_int

        mov r11, rax
  ; r11 = oxygen_generator_rating
  
  .find_CO2_scrubber_rating:
  
    .fill_vector_call:
      mov   rax, [rsp] ; vector_addr
      mov   rbx, [rsp+24] ; matrix_width
      mov   rcx, [rsp+16] ; file_addr
      mov   rdx, [rsp+8] ; file_size
      call  fill_vector

    ; for (i = 0; i < matrix_width; i++)
    xor   r10, r10 ; r10 = i = 0
    mov   r9, [rsp+32] ; matrix_height
    .loop_1:

      .count_zeros_call_1:
        mov   rdi, [rsp]  ; vector_address
        mov   rsi, r9     ; matrix_height
        mov   rdx, r10    ; column_index
        call  count_zeros
      ; rax = zeros
      mov rbx, r9
      sub rbx, rax
      ; rbx = ones

      .keep_if_call_1:
        mov   rdi, [rsp] ; vector_address
        mov   rsi, r9    ; matrix_height
        mov   rdx, r10   ; column_index

        cmp rbx, rax ; zeros <= ones
        setge al
        mov cl, '1'
        sub cl, al ; keep_value = '1' - (zeros <= ones)

        call keep_if
        mov r9, rax

      .next_1:
        inc r10
        cmp r10, [rsp+24] ; if (i < matrix_width) goto .loop
        jl .loop_1

      .done_1:
        mov   rdi, [rsp] ; vector_address
        mov rdi, [rdi]
        call read_binary_int
  ; rax = CO2_scrubber_rating

  imul rax, r11
  mov rdi, rax 
  call print_int

  ; exit(0)
  mov   rdi, 0  ; exit code
  mov   rax, SYS_EXIT ; exit
  syscall

  ; "Usage: ${argv[0]} <input>"
  .print_usage:

    ; better to buffer this in memory (too lazy for this)
    mov     rax, SYS_WRITE
    mov     rdi, STDOUT
    mov     rsi, usage0
    mov     rdx, usage0_len
    syscall

    pop     rdi     ; argv[0]
    call    str_len ; get length of argv[0]

    mov     rsi, rdi ; prepate pointer for syscall
    mov     rdx, rax ; prepare length for syscall
    mov     rax, SYS_WRITE
    mov     rdi, STDOUT
    syscall

    mov     rsi, usage1
    mov     rdx, usage1_len
    mov     rax, SYS_WRITE
    syscall

    jmp .exit_error
  
  .print_open_file_error:
    push rdi ; argv[1]

    mov     rax, SYS_WRITE
    mov     rdi, STDOUT
    mov     rsi, open_file_error0
    mov     rdx, open_file_error0_len
    syscall

    pop     rdi ; argv[1]
    call    str_len

    mov     rsi, rdi ; prepate pointer for syscall
    mov     rdx, rax ; prepare length for syscall
    mov     rax, SYS_WRITE
    mov     rdi, STDOUT
    syscall

    mov     rax, SYS_WRITE
    mov     rsi, open_file_error1
    mov     rdx, open_file_error1_len
    syscall
    
  ; exit(1)
  .exit_error:
    mov     rdi, 1        ; exit code
    mov     rax, SYS_EXIT ; exit
    syscall
; end of _start

; Function: str_len
; Input: (rdi value: byte[*:0])
; Output: (rax value: int)
; Modifies: (rax)
str_len:
    xor     rax, rax ; len = 0
    cmp     BYTE [rdi], 0
    je      .return
  .loop:
    inc     rax
    cmp     BYTE [rdi+rax], 0
    jne     .loop
  .return:
    ret
; end of str_len

; Function: print_int
; Description: prints integer to stdout followed by newline
; Input: (rdi value: i64)
; Output: (none)
; Modifies: (rax, rbx, rcx, rsi)
print_int:
  mov   rbx, -1  ; buffer offset
  mov   rcx, 10  ; divisor
  mov   rax, rdi ; dividend

  ; add new line at the end
  mov   BYTE [rsp+rbx], 10
  dec   rbx

  cmp   rdi, 0
  jge    .loop
    neg   rax

  .loop:
    mov   rdx, 0   ; clear dividend
    div   rcx      ; rax = rax / 10, rdx = rax % 10
    add   rdx, '0'
    mov   BYTE [rsp+rbx], dl
    dec   rbx

    cmp   rax, 0
    jne   .loop


  .handle_sign:
    cmp   rdi, 0
    jge    .sys_write
      mov   BYTE [rsp+rbx], '-'
      dec   rbx

  .sys_write:
    lea   rsi, [rsp+rbx+1] ; rsi = buffer
    mov   rdx, -1
    sub   rdx, rbx ; length of buffer
    mov   rax, SYS_WRITE
    mov   rdi, STDOUT
    syscall

  ret
; end of print_int

; Function: read_binary_int
; Description: read base 2 integer from input
; Input: (rdi ptr: byte[*])
; Output: (rax value: i64)
read_binary_int:
  xor   rax, rax ; value = 0
  xor   rbx, rbx ; tmp = 0

  .loop:
    ; load char
    mov   bl, BYTE [rdi] ; rbx = *ptr
    cmp   BYTE [is_whitespace+rbx], 1
    je    .return
    inc   rdi ; ptr++
    sub   bl, '0'
    shl   rax, 1
    or    rax, rbx
    jmp .loop

  .return:
    ret
; end of read_binary_int


; Function: file_size
; Input: (rdi fd)
; Output: (rax len: i64)
; Modifies: (rax, rbx, rsi, rdx)
file_size:
  .seek_end:
    mov   rax, SYS_LSEEK
    xor   rsi, rsi      ; offset = 0
    mov   rdx, SEEK_END ; whence = SEEK_END
    syscall
    push  rax

  .seek_set:
    mov   rax, SYS_LSEEK
    mov   rdx, SEEK_SET ; whence = SEEK_END
    syscall
    pop   rax

  ret
; end of file_size

; Function: prepare_matrix
; Input: 
;   rdi fd
; Ouput on stack:
;   [rsp]    : vector_addr
;   [rsp+8]  : file_size
;   [rsp+16] : file_addr
;   [rsp+24] : matrix_width
;   [rsp+32] : matrix_height
prepare_matrix:
  pop   rax ; rax = return_addr
  sub   rsp, 6*8 ; reserve space for variables
  ; [rsp]    : return_addr
  ; [rsp+8]  : fd
  ; [rsp+16] : file_size
  ; [rsp+24] : file_addr
  ; [rsp+32] : matrix_width
  ; [rsp+40] : matrix_height

  mov   [rsp], rax ; save return_addr
  mov [rsp+8], rdi ; store fd

  call  file_size
  mov   [rsp+16], rax ; store file_size

  ; Step #1: Read file to memory
  .mmap_0:
    mov   rsi, rax ; size
    mov   rax, SYS_MMAP  
    mov   rdx, PROT_READ | PROT_WRITE ; prot
    mov   r10, MAP_PRIVATE | MAP_ANONYMOUS ; flags
    mov   r9,  0 ; offset
    mov   r8,  0 ; fd
    mov   rdi, 0 ; addr
    syscall
    mov   [rsp+24], rax ; store file_addr

  .read_file:
    mov   rsi, rax      ; file_addr
    mov   rax, SYS_READ
    mov   rdi, [rsp+8]  ; fd
    mov   rdx, [rsp+16] ; file_size
    syscall

  .close_file:
    mov   rax, SYS_CLOSE
    syscall

  ; Step #2: Get matrix dimensions
  .find_width:
    mov   rax, [rsp+24] ; file_addr
    .loop_0:
      inc   rax
      cmp   BYTE [rax], 0xa
      jne   .loop_0
    
    sub   rax, [rsp+24] ; rax = matrix width
    mov   [rsp+32], rax ; store matrix_width

  .find_height:
    ; rbx = matrix height = file size / (matrix width + 1)
    lea   rbx, [rax+1]  ; rbx = (matrix width + 1)
    mov   rax, [rsp+16] ; rax = file_size
    mov   rdx, 0        ; clear dividend
    div   rbx           ; rax = rax / rbx, rdx = rax % rbx
    mov   [rsp+40], rax ; store matrix_height

  ; rax = matrix height
  ; rbx = (matrix width + 1)

  ; Step #3: Allocate vector for each row, and fill it with offsets
  ; [rsp+8] : vector_addr
  .mmap_1:
    mov rsi, rax  
    imul  rsi, 8  ; size = height * 8
    mov   rax, SYS_MMAP
    mov   rdx, PROT_READ | PROT_WRITE ; prot
    mov   r10, MAP_PRIVATE | MAP_ANONYMOUS ; flags
    mov   r9,  0 ; offset
    mov   r8,  0 ; fd
    mov   rdi, 0 ; addr
    syscall
    mov [rsp+8], rax ; store vector_addr
  
  .fill_vector_call:
    mov   rbx, [rsp+32] ; rbx = matrix_width
    mov   rcx, [rsp+24] ; rcx = file_addr
    mov   rdx, [rsp+16] ; rdx = file_size
    call  fill_vector



  .end:
    ret
; end of prepare_matrix

; Function: fill_vector
; Decription: Fill vector with pointers to rows
; Input: 
;   rax vector_addr
;   rbx matrix_width
;   rcx file_addr
;   rdx file_size
; Output: 
; Modifies: (rax, rbx, rdi, rsi)
fill_vector:
  ; rdi = offset
  ; rsi = tmp
  inc   rbx ; rbx = (matrix_width + 1)
  xor   rdi, rdi ; i = 0

  ; while (offset < file_size)
  .loop:
    lea   rsi, [rcx+rdi] ; tmp = file_addr + offset
    mov   [rax], rsi     ; vector[i] = tmp
    add   rdi, rbx       ; offset += (matrix width + 1)
    add   rax, 8         ; vector_addr += 1 * sizeof(void*)

    cmp   rdi, rdx       ; if (offset < file_size) goto .loop
    jl    .loop 

  ret
; end of fill_vector

; Function: count_zeros
; Description: Count number of zeros in column
; Input:
;   rdi vector_addr
;   rsi matrix_height
;   rdx column_index
; Output:
;   rax number_of_zeros
count_zeros:
  xor   rax, rax ; count = 0
  lea   rsi, [rdi+8*rsi] ; rsi = last_vector_addr = vector_addr + matrix_height * 8
  ; rbx = tmp

  ; while (vertor_addr < last_vector_addr)
  .loop:
    mov   rbx, [rdi]
    cmp   BYTE [rbx+rdx], '0' ; vector[i][column_index] == '0'
    jne   .one
    inc   rax
    .one:

    add rdi, 8 ; vector_addr += sizeof(void*)
    cmp rdi, rsi ; if (vector_addr < last_vector_addr) goto .loop
    jl .loop

  ret
; end of count_zeros

; Function: keep_if
; Description: Keep rows if value at collumn_index is equal to specified keep_value
; Input:
;   rdi vector_addr
;   rsi matrix_height
;   rdx column_index
;   cl keep_value
; Output:
;   number_of_kept_rows
keep_if:
  push  rdi      ; save vector_addr
  mov   rax, rdi ; keep_addr = vector_addr
  lea   rsi, [rdi+8*rsi] ; rsi = last_vector_addr = vector_addr + matrix_height * 8
  ; rbx = tmp

  ; while (vertor_addr < last_vector_addr)
  .loop:
    mov   rbx, [rdi]
    cmp   BYTE [rbx+rdx], cl ; vector[i][column_index] == keep_value
    jne   .skip
      mov   [rax], rbx ; *keep_addr = vector_addr
      add   rax, 8     ; keep_addr += sizeof(void*)
    .skip:

    add rdi, 8   ; vector_addr += sizeof(void*)
    cmp rdi, rsi ; if (vector_addr < last_vector_addr) goto .loop
    jl .loop

    pop rbx
    sub rax, rbx ; rax = number_of_kept_rows = keep_addr - vector_addr
    mov rbx, 8
    mov rdx, 0 ; clear dividend
    div rbx
  ret
; end of keep_if

section .data
  usage0 db "Usage: "
  usage0_len equ $ - usage0
  usage1 db " <input>", 0xa
  usage1_len equ $ - usage1
  open_file_error0 db 'Could not open file: "'
  open_file_error0_len equ $ - open_file_error0
  open_file_error1 db '"', 0xa
  open_file_error1_len equ $ - open_file_error1
  is_whitespace db 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

; syscalls
SYS_EXIT  equ 60
SYS_READ  equ 0
SYS_WRITE equ 1
SYS_OPEN  equ 2
SYS_CLOSE equ 3
SYS_LSEEK equ 8
SYS_MMAP  equ 9

STDIN     equ 0
STDOUT    equ 1

SEEK_SET  equ 0x00
SEEK_END  equ 0x02

O_RDONLY  equ 0x00
PROT_READ     equ 0x01
PROT_WRITE    equ 0x02
MAP_PRIVATE   equ 0x02
MAP_ANONYMOUS equ 0x20