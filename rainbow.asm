;*********************************************
;	rainbow.asm
;		- A Simple Bootloader (x64 Port)
;
;	Ported to x64 Long Mode
;*********************************************

%define VGA_MEM 0xB8000
%define W 80
%define H 25

%define ORG 0x00007C00

org ORG
bits 16

boot:
    cli                         ; Disable interrupts
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00              ; Stack grows down from bootloader

    ; Enable A20 Line (Fast Method)
    in al, 0x92
    or al, 2
    out 0x92, al

    ; Clear Page Tables (0x1000 - 0x3FFF)
    mov di, 0x1000
    mov cx, 0x1800              ; 3 tables * 4096 bytes / 2 (words)
    xor ax, ax
    rep stosw

    ; Setup Page Tables
    ; PML4 at 0x1000, PDP at 0x2000, PD at 0x3000

    ; PML4[0] -> PDP
    mov eax, 0x2003             ; 0x2000 | Present | Write
    mov [0x1000], eax

    ; PDP[0] -> PD
    mov eax, 0x3003             ; 0x3000 | Present | Write
    mov [0x2000], eax

    ; PD[0] -> 2MB Page (Identity map 0-2MB)
    mov eax, 0x0083             ; 0x0 | Present | Write | Huge (2MB)
    mov [0x3000], eax

    ; Load GDT
    lgdt [gdt_descriptor]

    ; Enable PAE
    mov eax, cr4
    or eax, 1 << 5
    mov cr4, eax

    ; Enable Long Mode in EFER MSR
    mov ecx, 0xC0000080
    rdmsr
    or eax, 1 << 8
    wrmsr

    ; Enable Paging and Protected Mode
    mov eax, cr0
    or eax, (1 << 31) | (1 << 0)
    mov cr0, eax

    ; Jump to 64-bit code
    jmp 0x08:LongMode

bits 64
LongMode:
    ; Set data segments to 0
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov fs, ax
    mov gs, ax
    mov ss, ax

    ; Initialize drawing variables
    mov rbx, VGA_MEM
    xor r10, r10                ; r10 = rainbowStartColor index

draw_loop:
    xor r8, r8                  ; r8 = current row (y)

.row_loop:
    cmp r8, H
    jge .row_done

    ; Calculate color index for this row: (rainbowStartColor + y) % colorsLen
    mov rax, r10
    add rax, r8
    xor rdx, rdx
    mov rcx, colorsLen
    div rcx                     ; rdx = remainder (index)

    ; Get color
    lea rsi, [colorsArray]
    mov al, [rsi + rdx]         ; al = color attribute
    mov ah, [letter]            ; ah = character (Wait, in VGA memory: Byte 0=Char, Byte 1=Attr)
                                ; So word is (Attr << 8) | Char
                                ; Let's assemble the word in ax.
                                ; al was color. ah was char.
                                ; We want low byte char, high byte color.
    mov dl, al                  ; dl = color
    mov al, [letter]            ; al = char
    mov ah, dl                  ; ah = color
                                ; Now ax = (color << 8) | char

    ; Draw row (80 characters)
    mov r9, 0                   ; r9 = col (x)
.col_loop:
    cmp r9, W
    jge .col_done

    ; Calculate memory address: VGA_MEM + (row * W + col) * 2
    mov rdi, r8
    imul rdi, W
    add rdi, r9
    shl rdi, 1
    add rdi, VGA_MEM

    mov [rdi], ax

    inc r9
    jmp .col_loop

.col_done:
    inc r8
    jmp .row_loop

.row_done:
    ; Delay loop
    mov rcx, 0x4000000           ; Adjust for speed
.delay:
    dec rcx
    jnz .delay

    ; Update rainbowStartColor
    inc r10
    cmp r10, colorsLen
    jl .next_frame
    xor r10, r10
.next_frame:
    jmp draw_loop

; GDT Data
align 4
gdt_start:
    dq 0x0000000000000000       ; Null Descriptor
    dq 0x0020980000000000       ; Code Descriptor (64-bit, Present, Ring 0, Executable, Readable)
    dq 0x0000920000000000       ; Data Descriptor (Present, Ring 0, Writable)
gdt_end:

gdt_descriptor:
    dw gdt_end - gdt_start - 1
    dd gdt_start

; Data
colorsArray db 0x44,0x4c,0x6c,0xce,0xee,0xea,0xaa,0xbb,0x9b,0x99,0x19,0x59,0x55
colorsLen equ $ - colorsArray

letter db 0xb1

; Padding to 512 bytes
times 510-($-$$) db 0
db 0x55, 0xAA
