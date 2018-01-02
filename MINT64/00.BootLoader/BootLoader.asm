[ORG 0x00]
[BITS 16]

SECTION .text

jmp 0x07c0:START

TOTALSECTORCOUNT:	dw	1024; Max image size, 1152 sector(0x90000byte)

START:
	mov ax, 0x07C0	; Convert bootloader's start address(0x7C00) to segment register
	mov ds, ax			; Set ds segment register
	mov ax, 0xB800	; Convert video memory's start address(0xb800) to segment register
	mov es, ax			; Set es segment register

	mov ax, 0x0000	; Convert stack segment's start address to segment register
	mov ss,	ax	; Set SS segment register
	mov sp, 0xFFFE	; Set SP register's address to 0xFFFE
	mov bp, 0xFFFE	; Set BP register's address to 0xFFFE

	mov si, 0	; Initialize SI register

.SCREENCLEARLOOP: ; Clear screen
	mov byte [ es: si ], 0 ; copy 0 to delete address that located by video memory character
	mov byte [ es: si + 1 ], 0x0A; copy 0x0A(green) to address that located by video memory option

	add si, 2	; go to next location

	cmp si, 80 * 25 * 2; compare whole screen with SI register

	jl .SCREENCLEARLOOP; if si < 80 * 25 * 2 then continue

	push MESSAGE1	; argument 3 : message
	push 0		; argument 2 : Y value
	push 0		; argument 1 : X value
	call PRINTMESSAGE;
	add sp,	6	;
	
	push IMAGELOADINGMESSAGE	; argument 3 : message
	push 1				; argument 2 : Y value
	push 0				; argument 1 : X value
	call PRINTMESSAGE;
	add sp,	6	;
	
RESETDISK:
	;Call BIOS Reset Function. Service Number 0 & Drive Number 0 (Floopy)
	mov ax, 0	;
	mov dl, 0	;
	int 0x13	; Interrupt
	jc HANDLEDISKERROR

	;Read Sector From Disk. Set the Address that will copy the content of disk to 0x10000
	mov si, 0x1000	;
	mov es, si	;
	mov bx, 0x0000	;
	mov di, word [ TOTALSECTORCOUNT ];

READDATA:
	;check all sectors were read
	cmp di, 0	; Compare the number of images with 0
	je READEND	; If remaining sector is 0 then go to READEND
	sub di, 0x1	; else sector number --

	;Call BIOS read function
	mov ah, 0x02	; BIOS service number 2 (read sector)
	mov al, 0x1	; Read 1 sector
	mov ch, byte [ TRACKNUMBER ]; Set track number to read
	mov cl, byte [ SECTORNUMBER ]; Set sector number to read
	mov dh, byte [ HEADNUMBER ]; set head number to read
	mov dl, 0x00	; Set drive number to read(Floppy)
	int 0x13	; interrupt 
	jc HANDLEDISKERROR

	; calculate the address of track, head, sector address to copy address
	add si ,0x0020	; conver 0x200 to segment register value
	mov es, si	; add es and si to increase the address
	
	;Increase the sector number and determine if it has read to the last sector(18)
	mov al, byte [ SECTORNUMBER ]; Set sector number to AL register
	add al, 0x01	; Increase sector number 1
	mov byte [ SECTORNUMBER ], al; Reset SECTORNUMBER to an increased sector number
	cmp al, 19	; compare sector number with 19
	jl READDATA	; if sector number is lower than 19, go to READDATA

	;if the sector number is 19, toggle head(xor) and set sector number 1
	xor byte [ HEADNUMBER ], 0x01	; XOR HEAD NUMBER
	mov byte [ SECTORNUMBER ], 0x01	; Set SECTORNUMBER 1

	;If head is changed to 1, then we already read both of head, so increase track number 1 and go to readdata 
	cmp byte [ HEADNUMBER ], 0x00	; compare head number with 0x00
	jne READDATA 			; if head number is not 0, go to readdata
	
	add byte [ TRACKNUMBER ], 0x01	; increase track number 1
	jmp READDATA			; go to READDATA
	
READEND:
	;print the os image has already done
	push LOADINGCOMPLETEMESSAGE	; argument 3 : message
	push 1				; argument 2 : Y value
	push 20				; argument 1 : X value
	call PRINTMESSAGE		; 
	add sp, 6

	;execute os image
	jmp 0x1000:0x0000

HANDLEDISKERROR:
	push DISKERRORMESSAGE	; argument 3 : error message
	push 1			; argument 2 : Y value
	push 20			; argument 1 : X value
	call PRINTMESSAGE	; 
	
	jmp $	; infinite loop

PRINTMESSAGE:
	push bp		;
	mov bp, sp	;	
	
	push es
	push si
	push di
	push ax
	push cx
	push dx
	
	mov ax, 0xB800	; convert video init address to segment register value
	mov es, ax	; set ES Segment register
	
	; calculate V-MEM's address with x,y location 	
	mov ax, word [ bp + 6 ] ; set AX (Y location) with parameter 2
	mov si, 160		; set the one line's byte number(2 * 80) to SI register
	mul si			; multiply si and ax regiser 
	mov di, ax		; set di register with calculated y address
	
	;get final address with X location * 2
	mov ax, word [ bp + 4 ] ; set AX (X location) with parameter 1
	mov si, 2		; set the byte size(2) to Si register
	mul si			; multiply AX and Si register
	add di, ax		; add Y and X location to calculate video memory's address

	; address of string to print
	mov si, word [ bp + 8 ]	;

.MESSAGELOOP:
	mov cl, byte [ si ]	; copy the character is added SI and message1's address to CL register(1 byte)
	cmp cl, 0		; compare with null byte
	je .MESSAGEEND	; if cl is null byte, this means the string is end, so jump to MESSAGEEND function

	mov byte [ es: di ], cl; if is not null byte, print the character to video memory address 0xB800:di

	add si, 1				; add 1 to string index
	add di, 2				; add 2 to video memory index (video memory is configured with [char, option] so we need to add 2)

	jmp .MESSAGELOOP

.MESSAGEEND:
	pop dx
	pop cx
	pop ax
	pop di
	pop si
	pop es
	pop bp
	ret

MESSAGE1: db 'MINT64 OS Boot Loader GAZUAAAAA!', 0; set message

DISKERRORMESSAGE:	db	'DISK ERROR~!!', 0
IMAGELOADINGMESSAGE:	db	'OS Image Loading...', 0
LOADINGCOMPLETEMESSAGE:	db	'Complete~!!', 0

SECTORNUMBER:		db	0x02	; save sector number
HEADNUMBER:		db	0x00	; save head number
TRACKNUMBER:		db	0x00	; save track number

times 510 - ($ - $$)	db 0x00; $ : this line's addr, $$ : .text's start addr, $-$$ : offset. => fill 0x00 until here to address 510

db 0x55						; address 511 - Boot Sector
db 0xAA						; address 512 - Boot Sector
