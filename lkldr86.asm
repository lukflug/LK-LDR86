; SPDX-License-Identifier: MIT
; LK-LDR/86
; Second Stage
%include 'fat.inc'
%include 'rombios.inc'
%include 'boot.inc'

use16
cpu 8086
org 0x0800

			mov word [BootSector.LOAD_OFFSET], message						; Load after end of file
			mov si, file													; Copy file name	
			mov di, BootSector.FILE_NAME
			mov cx, 11
			rep movsb
			mov bx, BootSector.DIR_BUFFER
			mov bp, sp														; Call address on stack
			call near [bp]

			mov si, message													; Display message
.loop			lodsb														; Get next byte
				test al, al													; Check if zero
				jz short .break
				mov ah, BIOS.VIDEO_TTY										; Print character
				xor bx, bx
				int BIOS.VIDEO_INT
				jmp .loop

.break		cli
			mov al, 0x34 ; set channel 0 to mode 2
			out 0x43, al
			mov al, 0xB4 ; set channel 2 to mode 2
			out 0x43, al

			xor al, al
			out 0x40, al ; write to channel 0
			out 0x40, al
			out 0x42, al ; write to channel 2
			out 0x42, al

			mov cx, 1000
.repeat		push cx
			mov al, 0x00 ; latch channel 0
			out 0x43, al
			mov al, 0x80 ; latch channel 2
			out 0x43, al

			in al, 0x40 ; read channel 0
			mov ah, al
			in al, 0x40
			xchg al, ah
			push ax
			call hexprt16

			mov al, 0x20
			out 0xE9, al
			;mov ax, 0x0E20 ; print space
			;xor bx, bx
			;int 10h

			in al, 0x42 ; read channel 2
			mov ah, al
			in al, 0x42
			xchg al, ah
			push ax
			call hexprt16

			mov al, 0x20
			out 0xE9, al
			;mov ax, 0x0E20 ; print space
			;xor bx, bx
			;int 10h

			pop ax ; print difference
			pop bx
			sub ax, bx
			call hexprt16

			mov al, 0x0D
			out 0xE9, al
			mov al, 0x0A
			out 0xE9, al
			;mov ax, 0x0E0D ; print newline
			;xor bx, bx
			;int 10h
			;mov ax, 0x0E0A
			;xor bx, bx
			;int 10h
			pop cx
			loop .repeat
			jmp $


hexprt16:
			push ax
			mov al, ah
			call hexprt8
			pop ax
			call hexprt8
			ret

hexprt8:
			push ax
			mov cl, 4
			shr al, cl
			call hexprt4
			pop ax
			call hexprt4
			ret

hexprt4:
			and al, 0x0F
			mov bx, .table
			xlat
			out 0xE9, al
			;mov ah, BIOS.VIDEO_TTY
			;xor bx, bx
			;int BIOS.VIDEO_INT
			ret

.table		db '0123456789ABCDEF'


file					db 'LKLDR   FS '

message:
