; SPDX-License-Identifier: MIT
; LK-LDR/86
; Second stage
%include 'fat.inc'
%include 'rombios.inc'
%include 'boot.inc'

use16
cpu 8086
org 0x0600

							dw BootSector.BOOT_SIGNATURE, BootSector.BOOT_SIGNATURE

			mov si, message													; Display message
.loop			lodsb														; Get next byte
				test al, al													; Check if zero
				jz short .break
				mov ah, BIOS.VIDEO_TTY										; Print character
				xor bx, bx
				int BIOS.VIDEO_INT
				jmp .loop
.break		jmp $

message						db 'Hello world!', 0x0D, 0x0A, 0x00
