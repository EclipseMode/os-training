[ORG 0x00]
[BITS 16]

SECTION.text

jmp 0x07c0:START

START:
	mov ax, 0x07C0	; Convert bootloader's start address(0x7C00) to segment register
	mov ds, ax			; Set ds segment register
	mov ax, 0xB800	; Convert video memory's start address(0xb800) to segment register
	mov es, ax			; Set es segment register

	mov si, 0				; Initialize SI register

.SCREENCLEARLOOP: ; Clear screen
	mov byte [ es: si], 0 ; copy 0 to delete address that located by video memory character
	mov byte [ es: si + 1], 0x0A; copy 0x0A(green) to address that located by video memory option

	add si, 2				; go to next location

	cmp si, 80 * 25 * 2; compare whole screen with SI register

	jl .SCREENCLEARLOOP; if si < 80 * 25 * 2 then continue

	mov si, 0				; init si
	mov di, 0				; init di

.MESSAGELOOP:
	mov cl, byte [si + MESSAGE1]	;copy the character is added SI and message1's address to CL register(1 byte)
	cmp cl, 0				; copy with null byte
	je .MESSAGEEND	; if cl is null byte, this means the string is end, so jump to MESSAGEEND function

	mov byte [ es: di], cl; if is not null byte, print the character to video memory address 0xB800:di

	add si, 1				; add 1 to string index
	add di, 2				; add 2 to video memory index (video memory is configured with [char, option] so we need to add 2)

	jmp .MESSAGELOOP

.MESSAGEEND:
	jmp $						; make infinite loop

MESSAGE1: db 'MINT64 OS Boot Loader GAZUAAAAA!', 0; set message

times 510 - ($ - $$)	db 0x00; $ : this line's addr, $$ : .text's start addr, $-$$ : offset. => fill 0x00 until here to address 510

db 0x55						; address 511 - Boot Sector
db 0xAA						; address 512 - Boot Sector
