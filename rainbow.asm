; rainbow.asm - 3D Spinning XOR Fractal on Rainbow Background
;
; Uses a 2-stage bootloader mechanism to break the 512-byte limit.

bits 16
org 0x7c00

; =============================================================================
; Stage 1: Bootloader
; =============================================================================
start:
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7c00

    ; Reset disk system
    xor ah, ah
    int 0x13

    ; Load Stage 2 (Payload) from disk
    ; The file is padded to 4096 bytes (8 sectors).
    ; Sector 1 is bootloader. Sectors 2-8 are payload (7 sectors).
    mov ah, 0x02    ; Read sectors
    mov al, 7       ; Count
    mov ch, 0       ; Cylinder
    mov cl, 2       ; Sector (1-based, 1 is boot sector)
    mov dh, 0       ; Head
    ; dl is already drive number
    mov bx, 0x7e00  ; Buffer address
    int 0x13

    jc disk_error

    jmp 0x0000:0x7e00

disk_error:
    mov ah, 0x0e
    mov al, 'E'
    int 0x10
    cli
    hlt

    times 510-($-$$) db 0
    dw 0xaa55

; =============================================================================
; Stage 2: Main Payload
; =============================================================================
payload:
    ; Set Video Mode 13h (320x200, 256 colors)
    mov ax, 0x13
    int 0x10

    ; Setup Segment for Video Memory
    push 0xa000
    pop es

    ; Generate Palette
    call set_palette

    ; Initialize FPU
    fninit

mainloop:
    ; 1. Draw Background (Rainbow)
    call draw_background

    ; 2. Update Angles
    call update_angles

    ; 3. Draw 3D XOR Object
    call draw_object

    ; 4. VSync
    mov dx, 0x3da
wait_vsync:
    in al, dx
    test al, 8
    jz wait_vsync

    ; Loop
    jmp mainloop


; -----------------------------------------------------------------------------
; Palette Generation
; -----------------------------------------------------------------------------
set_palette:
    mov dx, 0x3c8
    xor al, al
    out dx, al
    inc dx

    ; We want a smooth rainbow for indices 0-199 (matching screen height)
    mov cx, 0      ; Color index
.palette_loop:
    ; Segment 0: Red -> Yellow
    cmp cx, 32
    jb .seg0
    cmp cx, 64
    jb .seg1
    cmp cx, 96
    jb .seg2
    cmp cx, 128
    jb .seg3
    cmp cx, 160
    jb .seg4
    cmp cx, 192
    jb .seg5
    jmp .seg_gray

.seg0: ; R->Y
    mov al, 63
    mov bx, cx
    shl bx, 1
    xchg ax, bx    ; G is ramping up, R=63
    push bx        ; Save R
    xor bx, bx     ; B is 0
    jmp .write_color

.seg1: ; Y->G
    mov bx, cx
    sub bx, 32
    shl bx, 1
    not bl
    and bl, 63     ; R ramping down
    mov al, 63     ; G constant
    push bx        ; Save R
    xor bx, bx     ; B is 0
    jmp .write_color

.seg2: ; G->C
    xor bx, bx     ; R is 0
    push bx
    mov al, 63     ; G constant
    mov bx, cx
    sub bx, 64
    shl bx, 1      ; B ramping up
    jmp .write_color

.seg3: ; C->B
    xor bx, bx     ; R is 0
    push bx
    mov bx, cx
    sub bx, 96
    shl bx, 1
    not bl
    and bl, 63     ; G ramping down
    xchg ax, bx    ; Swap G and ramping
    mov bx, 63     ; B constant
    jmp .write_color

.seg4: ; B->M
    mov bx, cx
    sub bx, 128
    shl bx, 1      ; R ramping up
    push bx        ; Save R
    xor ax, ax     ; G is 0
    mov bx, 63     ; B constant
    jmp .write_color

.seg5: ; M->R
    mov bx, 63     ; R constant
    push bx
    xor ax, ax     ; G is 0
    mov bx, cx
    sub bx, 160
    shl bx, 1
    not bl
    and bl, 63     ; B ramping down
    jmp .write_color

.seg_gray:
    ; Colors 192-255: Bright Cyan/White for the fractal
    mov bx, cx
    sub bx, 192
    push bx        ; R
    mov al, 63     ; G (High)
    mov bx, 63     ; B (High)
    jmp .write_color

.write_color:
    mov ah, al     ; Save G in AH
    pop ax         ; Pop R into AL
    out dx, al     ; Write R

    mov al, ah     ; Restore G
    out dx, al     ; Write G

    mov al, bl     ; B was in BX
    out dx, al     ; Write B

    inc cx
    cmp cx, 256
    jne .palette_loop
    ret
.palette_loop: ; Wait, I have ret above, this label is needed for loop.
    ; Re-check structure.
    ; jne .palette_loop jumps to start of loop.
    ; Ah, the labels inside loop jump to .write_color which does inc cx, cmp, jne .palette_loop.
    ; So I need the label at top.
    ; NASM local labels: .palette_loop is defined at top.
    ; Wait, `jne .palette_loop` refers to the label at the top. Correct.
    ; But I had a `ret` inside `.write_color` block? No, I put `ret` after `jne`.
    ; My code logic:
    ; top: ...
    ; jmp .seg
    ; .seg: ... jmp .write
    ; .write: out... inc... cmp... jne top
    ; ret

    ; This is correct.

; -----------------------------------------------------------------------------
; Background Drawing
; -----------------------------------------------------------------------------
draw_background:
    inc word [frame_counter]

    xor di, di      ; ES:DI = A000:0000
    mov dx, 0       ; Line counter (0-199)

.line_loop:
    ; Calculate color index = (line + frame) % 192
    mov ax, dx
    add ax, [frame_counter]
    shr ax, 1

    mov bl, 192
    div bl
    mov al, ah      ; al = remainder

    mov cx, 320
    mov ah, al
    rep stosb

    inc dx
    cmp dx, 200
    jne .line_loop
    ret

; -----------------------------------------------------------------------------
; Angles Update
; -----------------------------------------------------------------------------
update_angles:
    fld dword [ang_x]
    fadd dword [spd_x]
    fstp dword [ang_x]

    fld dword [ang_y]
    fadd dword [spd_y]
    fstp dword [ang_y]

    fld dword [ang_z]
    fadd dword [spd_z]
    fstp dword [ang_z]
    ret

; -----------------------------------------------------------------------------
; 3D Logic & Drawing
; -----------------------------------------------------------------------------
draw_object:
    ; Rotate Basis U (1,0,0)
    fld1
    fldz
    fldz
    call rotate_point
    fstp dword [vec_u_z]
    fstp dword [vec_u_y]
    fstp dword [vec_u_x]

    ; Rotate Basis V (0,1,0)
    fldz
    fld1
    fldz
    call rotate_point
    fstp dword [vec_v_z]
    fstp dword [vec_v_y]
    fstp dword [vec_v_x]

    ; Now iterate over u, v to draw the plane
    mov cx, -50     ; u
.loop_u:
    mov bx, -50     ; v
.loop_v:
    ; Load u
    mov word [temp_int], cx
    fild word [temp_int] ; st0 = u

    ; Load v
    mov word [temp_int], bx
    fild word [temp_int] ; st0 = v, st1 = u

    ; Calculate Pz
    fld st1             ; u
    fmul dword [vec_u_z]; u*Uz
    fld st1             ; v
    fmul dword [vec_v_z]; v*Vz
    faddp               ; u*Uz + v*Vz
    fadd dword [center_z] ; + CenterZ -> Pz

    ; Check if Pz is behind camera
    fcom dword [near_plane]
    fstsw ax
    sahf
    jb .skip_point

    ; Calculate Px
    fld st2             ; u
    fmul dword [vec_u_x]
    fld st2             ; v
    fmul dword [vec_v_x]
    faddp               ; Px

    ; Calculate Py
    fld st3             ; u
    fmul dword [vec_u_y]
    fld st3             ; v
    fmul dword [vec_v_y]
    faddp               ; Py

    ; Stack: Py, Px, Pz, v, u

    ; Perspective Projection
    ; Py
    fmul dword [focal]
    fdiv st0, st2       ; Py*F / Pz
    fadd dword [center_sy] ; + 100
    frndint
    fistp word [out_y]  ; Store sy

    ; Px
    fmul dword [focal]
    fdiv st0, st1       ; Px*F / Pz
    fadd dword [center_sx] ; + 160
    frndint
    fistp word [out_x]  ; Store sx

    ; Clean up stack (Pz, v, u remaining)
    fstp st0 ; pop Pz
    fstp st0 ; pop v
    fstp st0 ; pop u

    ; Bounds check
    cmp word [out_x], 0
    jl .next_v
    cmp word [out_x], 319
    jg .next_v
    cmp word [out_y], 0
    jl .next_v
    cmp word [out_y], 199
    jg .next_v

    ; Calculate Color = (u ^ v)
    mov ax, cx
    xor ax, bx
    and ax, 0x1F
    add al, 192

    ; Plot pixel
    push bx
    push cx

    mov cx, ax      ; Save color

    mov ax, 320
    mul word [out_y]
    add ax, word [out_x]
    mov di, ax

    mov al, cl      ; Restore color
    stosb

    pop cx
    pop bx

    jmp .next_v

.skip_point:
    ; Clean stack: Pz, v, u
    fstp st0
    fstp st0
    fstp st0

.next_v:
    add bx, 2
    cmp bx, 50
    jl .loop_v

    add cx, 2
    cmp cx, 50
    jl .loop_u

    ret


; Routine: Rotate Point (x,y,z) on stack
rotate_point:
    ; Input: st0=X, st1=Y, st2=Z

    ; 1. Rotate around X
    fld dword [ang_x]
    fsincos

    fstp dword [rc]
    fstp dword [rs]

    fstp dword [rx]
    fstp dword [ry]
    fstp dword [rz]

    ; Y'
    fld dword [ry]
    fmul dword [rc]
    fld dword [rz]
    fmul dword [rs]
    fsubp
    fstp dword [ry_new]

    ; Z'
    fld dword [ry]
    fmul dword [rs]
    fld dword [rz]
    fmul dword [rc]
    faddp
    fstp dword [rz_new]

    mov eax, [ry_new]
    mov [ry], eax
    mov eax, [rz_new]
    mov [rz], eax

    ; 2. Rotate around Y
    fld dword [ang_y]
    fsincos
    fstp dword [rc]
    fstp dword [rs]

    ; X'
    fld dword [rx]
    fmul dword [rc]
    fld dword [rz]
    fmul dword [rs]
    faddp
    fstp dword [rx_new]

    ; Z'
    fld dword [rz]
    fmul dword [rc]
    fld dword [rx]
    fmul dword [rs]
    fsubp
    fstp dword [rz_new]

    mov eax, [rx_new]
    mov [rx], eax
    mov eax, [rz_new]
    mov [rz], eax

    ; 3. Rotate around Z
    fld dword [ang_z]
    fsincos
    fstp dword [rc]
    fstp dword [rs]

    ; X'
    fld dword [rx]
    fmul dword [rc]
    fld dword [ry]
    fmul dword [rs]
    fsubp
    fstp dword [rx_new]

    ; Y'
    fld dword [rx]
    fmul dword [rs]
    fld dword [ry]
    fmul dword [rc]
    faddp
    fstp dword [ry_new]

    mov eax, [rx_new]
    mov [rx], eax
    mov eax, [ry_new]
    mov [ry], eax

    ; Push back to stack: Z, Y, X
    fld dword [rz]
    fld dword [ry]
    fld dword [rx]
    ret

; =============================================================================
; Data Section
; =============================================================================

frame_counter dw 0

; Angles
ang_x dd 0.0
ang_y dd 0.0
ang_z dd 0.0

; Speeds
spd_x dd 0.02
spd_y dd 0.03
spd_z dd 0.01

; Constants
focal dd 200.0
center_z dd 250.0
center_sx dd 160.0
center_sy dd 100.0
near_plane dd 10.0

; Temps
rx dd 0.0
ry dd 0.0
rz dd 0.0
rx_new dd 0.0
ry_new dd 0.0
rz_new dd 0.0
rc dd 0.0
rs dd 0.0

vec_u_x dd 0.0
vec_u_y dd 0.0
vec_u_z dd 0.0
vec_v_x dd 0.0
vec_v_y dd 0.0
vec_v_z dd 0.0

out_x dw 0
out_y dw 0
temp_int dw 0

; Padding to make sure the file is large enough for the loader
times 4096-($-$$) db 0
