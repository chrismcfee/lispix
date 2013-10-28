[ORG 0x2000] ; just after bootloader
[BITS 32]

int_bios_asm:
	pushad
	mov eax, [esp+32+8]
	mov byte [int_num], al

	mov eax, [esp+32+4]
	mov [stack16], eax

	mov [saved_esp], esp

	sgdt [gdt32]

	;mov eax,[stack16]
	;push eax
	;push hello_str
	;call printf
	;add esp,8

	jmp pm_leave

;; hello_str:
;; 	db "0x%x", 0xa, 0

pm_leave:
    ; reset data segs...
    mov ax,0x20
    mov ds,ax
    mov es,ax
    mov fs,ax
    mov gs,ax
    mov ss,ax

    ; reset CS (jmp 18:enter_rm)
    db 0xEA
    dw enter_rm,0,0x18

enter_rm:
    ; reset IDTR to nil (or perhaps we should load dummy IDT?)
    ;lidt [idt_nil]

    ; finally, leave PM
    mov eax,cr0
    and al,0xFE
    mov cr0,eax

    ; reset CS again, this time for realmode
    db 0xEA
    dw do_bios_int, 0

[BITS 16]
do_bios_int:

	xor ax,ax
	mov ds,ax
    mov ss,ax
	lidt [idt_real]


	mov sp,[stack16]

	pop ds
	pop es
	pop fs
	pop gs
	popa ; pop passed registers

	;push ax
	;mov ax,0xb800
	;mov es,ax
	;mov al,"H"
	;mov [es:0], al
	;mov al,0x07
	;mov [es:1], al
	;pop ax

	sti

	; int 0x??
	db 0xcd
int_num:
	db 0x0

	cli

	pusha
	push gs
	push fs
	push es
	push ds

	xor ax,ax
	mov ds,ax

	lgdt [gdt32] ; restore GDT

	;; return to protected mode
	mov eax, cr0
	or eax, 1
	mov cr0, eax

	;jmp $
	jmp 08h:pm_return

[BITS 32]

pm_return:
	mov ax, 10h
	mov ds, ax
	mov ss, ax
	mov esp,[saved_esp]
	popad
	ret


idt_nil:
	dw 0 						; limit
	dd 0						; address

idt_real:
	dw 0x03ff 					; limit
	dd 0						; address

stack16:
	dd 0

gdt32:
	dw 0
	dd 0

saved_esp:
	dd 0

times 4096-($-$$) db 0
