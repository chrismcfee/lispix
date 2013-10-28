[BITS 16]       ; We need 16-bit intructions for Real mode

[ORG 0x1000]    ; The BIOS loads the boot sector into memory location 0x7C00

; A "1.44 MB" floppy disk has 80 cylinders (numbered 0 to 79),
; 2 heads (numbered 0 to 1) and 18 sectors (numbered 1 to 18). 
CYLINDERS equ 80
HEADS equ 2
SECTORS equ 18
		
entry:
	; relocate bootsector to 0x1000
.retry
		
	cli ; disable interrupts, while setting stack
	xor ax, ax
	mov ds, ax
	mov ss, ax
	mov sp, 0x1000 			; lets pray it will be enough for bios routines
	sti

	mov ah, 0               ; RESET-command
	int 13h                 ; Call interrupt 13h
	or ah, ah               ; Check for error code
	jnz .retry              ; Try again if ah != 0

	; Put destination address in ES:BX
	mov ax, 0
	mov es, ax
	mov bx, 0x1000

	mov ah, 02h             ; READ SECTOR-command
	mov al, 1               ; Number of sectors to read = 1
	mov ch, 0               ; Cylinder = 0
	mov dh, 0               ; Head = 0
	mov cl, 1               ; Sector = 2
	int 13h                 ; Call interrupt 13h
	or ah, ah               ; Check for error code
	jnz .retry              ; Try again if ah != 0

	jmp 0:bootsect_copy     ; Switch to copy, because 0x7C00
                            ; will be overwriten soon

	;; bootsector only loads rest of code into memory
	;; and jumps to it
bootsect_copy:
	;;  copy rest of bootloader
	mov ax, 0
	mov es, ax  ; destination segment
	push dx
	push 0x1200 ; destination offset
	push 1		; starting sector
	push 7+8	; number of sectors to read
	call read_floppy

	;; load kernel to 0x20000
	mov ax, 0x2000
	mov es, ax
	push dx
	push 0
	push 16
	push 128
	call read_floppy

	call stop_drive_motor

	jmp start
 
read_floppy:
	mov bp, sp
	mov dx, [bp+8] ; disk to read from
	mov ax, [bp+6] ; where to put readed
	mov bx, [bp+4] ; starting block
	mov cx, [bp+2] ; number of blocks to read
.loop
	or cx,cx
	jz .return

	push dx
	push ax
	push bx
	call load_block

	add ax,512
	add bx,1
	sub cx,1
	jmp .loop
.return
	ret 8

load_block:
	mov bp, sp
	sub sp, 6
	push ax
	push bx
	push cx
	push dx

	mov ax, [bp+2]
	mov cx, SECTORS*HEADS
	div cl
	mov [bp-2],ax ; cylinder

	mov ax, [bp+2]
	mov cx, SECTORS
	div cl
	mov ah, 0
	mov cx, HEADS
	div cl
	mov dl, ah
	mov [bp-4], dx ; head

	mov ax, [bp+2]
	mov cx, SECTORS
	div cl
	mov dl, ah
	add dx, 1
	mov [bp-6], dx ; sector

	mov dx, [bp+6]

retry:	
	mov ah, 0               ; RESET-command
	int 13h                 ; Call interrupt 13h
	or ah, ah               ; Check for error code
	jnz retry               ; Try again if ah != 0

	mov bx, [bp+4]          ; Destination address

	mov ah, 02h             ; READ SECTOR-command
	mov al, 1               ; Number of sectors to read = 1
	mov ch, [bp-2]          ; Cylinder = 0
	mov dh, [bp-4]          ; Head = 0
	mov cl, [bp-6]          ; Sector = 2
	int 13h                 ; Call interrupt 13h
	or ah, ah               ; Check for error code
	jnz retry               ; Try again if ah != 0

	pop dx
	pop cx
	pop bx
	pop ax
	mov sp, bp
	ret 6

stop_drive_motor:
	push dx
	push ax
	mov dx,0x3f2
	xor al,al
	out dx,al
	pop ax
	pop dx
	ret

times 510-($-$$) db 0           ; Fill up the file with zeros

      dw 0AA55h		; Boot sector identifyer


print_tetra:
	push bp
	mov bp, sp
	push ax
	push bx
	mov ax, [bp+4]
	and ax, 0xf
	cmp ax, 10
	jb .add_zero
	sub ax, 10
	add ax, 'A'
	jmp .out
.add_zero
	add ax, '0'
.out
	mov ah,0x0E    ; TTY Mode put char call
	mov bh,0x00    ; Page number
	mov bl,0x07    ; Normal attribute
	int 0x10

	;mov ax,0xb000
	;mov es,ax
	;mov [es:0x8000], al
	;mov al,0x07
	;mov [es:0x8000], al

	pop bx
	pop ax
	mov sp, bp
	pop bp
	ret 2

print_hex:
	push bp
	mov bp, sp
	push ax

	mov ax,[bp+4]
	shr ax,12
	push ax
	call print_tetra

	mov bx,[bp+4]
	shr bx,8
	push bx
	call print_tetra

	mov ax,[bp+4]
	shr ax,4
	push ax
	call print_tetra

	mov ax,[bp+4]
	push ax
	call print_tetra

	pop ax
	mov sp, bp
	pop bp
	ret 2


	;; now we can comfortably enter protected mode...
start:
	call enable_a20_line

	cli                     ; Disable interrupts, we want to be alone

	xor ax, ax
	mov ds, ax              ; Set DS-register to 0 - used by lgdt

	lgdt [gdt_desc]         ; Load the GDT descriptor

	; Enter protected mode
	mov eax, cr0
	or eax, 1
	mov cr0, eax

	jmp 08h:clear_pipe ; Jump to code segment, offset clear_pipe

enable_a20_line:
	in al, 0x92
	or al, 2
	out 0x92, al

	;mov ax,2401
	;int 15
	ret


[BITS 32]                       ; We now need 32-bit instructions
clear_pipe:
	mov ax, 10h             ; Save data segment identifyer
    mov ds, ax              ; Move a valid data segment into the data segment register
    mov ss, ax              ; Move a valid data segment into the stack segment register
	;mov esp, 090000h        ; Move the stack pointer to 090000h
    jmp 08h:dword 20000h          ; Jump to section 08h (code), offset 01000h


gdt:                    ; Address for the GDT

gdt_null:               ; Null Segment
        dd 0
        dd 0

gdt_cs32: ; Code segment, read/execute, nonconforming
        dw 0FFFFh
        dw 0
        db 0
        db 10011010b
        db 11001111b
        db 0

gdt_ds32: ; Data segment, read/write, expand down
        dw 0FFFFh
        dw 0
        db 0
        db 10010010b
        db 11001111b
        db 0

gdt_cs16:
	dw 0ffffh
    gdt_cs16_bsl:
    dw 0
    gdt_cs16_bsm:
    db 0
    db 10011010b
    db 10000000b
    db 0

gdt_ds16:
    dw 0ffffh
    gdt_ds16_bsl:
    dw 0
    gdt_ds16_bsm:
    db 0
    db 10010010b
    db 10000000b
    db 0


gdt_end:                ; Used to calculate the size of the GDT


gdt_desc:                       ; The GDT descriptor
        dw gdt_end - gdt - 1    ; Limit (size)
        dd gdt                  ; Address of the GDT


times 4096-($-$$) db 0			; total=0x1000
