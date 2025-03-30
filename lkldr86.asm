; SPDX-License-Identifier: MIT
; LK-LDR/86
; Second stage
%include 'fat.inc'
%include 'rombios.inc'
%include 'boot.inc'

use16
cpu 8086
org 0x0600

			mov word [BootSector.LOAD_OFFSET], message						; Load after end of file
			mov si, file													; Copy file name	
			mov di, BootSector.FILE_NAME
			mov cx, 11
			rep movsb
			mov bx, BootSector.DIR_BUFFER
			call BootSector.ENTRY_POINT

			mov si, message													; Display message
.loop			lodsb														; Get next byte
				test al, al													; Check if zero
				jz short .break
				mov ah, BIOS.VIDEO_TTY										; Print character
				xor bx, bx
				int BIOS.VIDEO_INT
				jmp .loop
.break		jmp $

file					db 'LKLDR   FS '

message:
