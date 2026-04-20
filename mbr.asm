; SPDX-License-Identifier: MIT
; LK-LDR/86
; Boot Sector for Master Boot Record
; Relocates itself to 0x0600.
; Loads and jumps to VBR of active partition.
%include 'rombios.inc'
%include 'mbr.inc'

STACK_BASE					equ 0x8000
SAVED_DX					equ STACK_BASE-0x0006
PNP_DI						equ STACK_BASE-0x0004
PNP_ES						equ STACK_BASE-0x0002

use16
cpu 8086
org MBR.RELOC_BASE


start:
			jmp short bootloader
			nop

; Protective BPB for quirky USB boot and IBM El Torito emulation
; https://github.com/freebsd/freebsd-src/blob/82a21151cf1d7a3e9e95b9edbbf74ac10f386d6a/stand/i386/boot2/boot1.S
; https://github.com/Limine-Bootloader/Limine/blob/9c3ead93866318400c04f1b9e588e0435e00c847/stage1/hdd/bootsect.asm
oemName						db 'LKLDR86 '
bytesPerSector				dw SECTOR_SIZE
sectorsPerCluster			db 0x00
reservedSectors				dw 0x0000
fatCount					db 0x00
rootEntries					dw 0x0000
sectorCount					dw 0x0000
mediaDescriptor				db 0x00
sectorsPerFAT				dw 0x0000
sectorsPerTrack				dw 0x0012
headCount					dw 0x0002
hiddenSectors				dd 0x00000000
sectorCount32				dd 0x00000000
bpbDriveNumber				db 0x00
reservedByte				db 0x00
bootSignature				db 0x00
serialNumber				dd 0x00000000
volumeLabel					db 'LKLDR86    '
fileSystem					times 8 db 0x00


; Set up segments and stack
; Relocate MBR to 0x0600
; Scans for active partition
; Attempts to load VBR first by using drive number passed by BIOS, followed by drive number stored in partition status byte
; If it fails, display error message, wait for key stroke, and run int 18h
; dl - boot drive number
; Never returns
bootloader:
			cli
			cld
			xor ax, ax														; Initialize stack
			push ax															; Interrupts need to be disabled here to guard against early 8086/8088 lacking interrupt shadow (https://www.pcjs.org/documents/manuals/intel/8086/)
			pop ss
			mov sp, STACK_BASE
			sti
			push es															; Save es:di pointer to "$PnP" structure
			push di
			push dx															; Save dx
			push ax															; Initialize other segments
			pop ds
			push ax
			pop es

			mov si, MBR.LOAD_BASE											; Self-relocate to 0x0600
			mov di, MBR.RELOC_BASE
			mov cx, MBR.END
			rep movsb
			jmp 0x0000:.jump												; Jump to relocated code

.jump		mov cx, 0x0004													; Scan for active partition
			mov si, partition1
.loop			test byte [si+PartitionEntry.STATUS], PartitionEntry.ACTIVE_MASK
				jnz short .found
				add si, PartitionEntry.SIZE
				inc byte [bootMessage.number]
				loop .loop

			mov si, noMessage												; Print error message
			mov cx, noMessage.end-noMessage
			call printMessage
			mov si, promptMessage
			mov cx, promptMessage.end-promptMessage
.error		call printMessage
			xor ah, ah														; Wait for key and boot to BASIC (set ah = 0)
			int BIOS.KEY_INT
			int BIOS.BASIC_INT
			jmp $															; Some BIOSes return on int 18h (http://www.ctyme.com/intr/rb-2241.htm)

.found		push si
			mov si, bootMessage
			mov cx, bootMessage.end-bootMessage
			call printMessage
			pop si

			call loadVBR													; Try dx passed by BIOS
			mov dl, [si+PartitionEntry.STATUS]								; Next try partition status
			call loadVBR
			mov dl, [bpbDriveNumber]										; Try BPB drive number, if it has been changed
			test dl, dl
			jnz short .error
			call loadVBR
			mov si, errorMessage											; Display error message
			mov cx, promptMessage.end-errorMessage
			jmp .error


; Print message
; cx - Length of string
; si - Pointer to string
printMessage:
				lodsb														; Get next byte
				mov ah, BIOS.VIDEO_TTY										; Print character
				xor bx, bx
				int BIOS.VIDEO_INT											; bp may not be preserved if it scrolls (http://www.ctyme.com/intr/rb-0106.htm)
				loop printMessage
			ret


							align 16, db 0x00
dap:
.size						dw BIOS.DISK_DAP_SIZE
.sectors					dw 0x0001
.buffer_offset				dw MBR.LOAD_BASE
.buffer_segment				dw 0x0000
.lba						dq 0x0000000000000000

							times MBR.TIMESTAMP_START-($-$$) db 0x00
							dw 0x0000
timestampDriveNumber		db 0x00
seconds						db 0x00
minutes						db 0x00
hours						db 0x00


; Load active partition VBR at 0x7C00
; dl - drive number
; si - Pointer to partition entry
; Only returns on error
loadVBR:
			xor ax, ax														; If LBA partition start is zero, use CHS
			lea di, [si+PartitionEntry.LBA_START]
			scasw
			jne short .test_size2
			scasw
			jne short .test_size
			jmp short .chs

.test_size2 scasw
.test_size	scasw															; If LBA partition size is zero, use CHS
			jne short .test_lba
			scasw
			jne short .test_lba

.chs		mov cx, [si+PartitionEntry.BEGIN_CYLINDER_SECTOR]				; Get CHS address of VBR
			mov dh, [si+PartitionEntry.BEGIN_HEAD]
			test cx, cx														; Check if non-zero
			jnz short .chs_ok
			test dh, dh
			jnz short .chs_ok
.error		ret

.chs_ok		test cl, 0x3F													; Check if sector number is non-zero
			jz short .error
			mov di, 0x0004													; Reset error counter
.read			mov bx, MBR.LOAD_BASE										; Read sector (and guard against various int 13h bugs, http://www.ctyme.com/intr/rb-0607.htm)
				mov ax, BIOS.DISK_READ1
				push dx
				stc
				int BIOS.DISK_INT
				sti
				pop dx
				jnc short .success

				dec di														; Retry 3 times (due to motor spin-up of floppy drives, http://www.ctyme.com/intr/rb-0607.htm)
				jz short .error
				cbw															; Reset disk (set ah = 0)
				int BIOS.DISK_INT
				jmp .read

.success	mov dh, [SAVED_DX+0x0001]										; Restore PnP info
			les di, [PNP_DI]
			mov bp, si														; Set ds:si and ds:bp to partition entry
			jmp MBR.LOAD_BASE												; Transfer control to VBR

.test_lba	mov ah, BIOS.DISK_LBA_CHECK										; Check if LBA supported
			mov bx, BIOS.DISK_LBA_CHECK_IN
			int BIOS.DISK_INT
			jc short .chs
			cmp bx, BIOS.DISK_LBA_CHECK_OUT
			jne short .chs

			push si															; Save partition entry pointer
			add si, PartitionEntry.LBA_START								; Set partition start LBA
			mov di, dap.lba
			movsw
			movsw
			mov si, dap														; Invoke int 13h extension
			mov ah, BIOS.DISK_LBA_READ
			int BIOS.DISK_INT												; Don't retry, as we assume that floppies don't support LBA
			pop si															; Restore si
			jnc short .success
			jmp .error


bootMessage					db 'Booting from partition '
.number						db '1 ...', 0x0D, 0x0A
.end:
noMessage					db 'No active partition found!'
.end:
errorMessage				db 'Error!'
promptMessage				db ' Press any key to reboot ...', 0x0D, 0x0A
.end:

							times MBR.DISK_SIGNATURE-($-$$) db 0x00
diskSignature				dd 0x00000000
							dw 0x0000
partition1					times 16 db 0x00
partition2					times 16 db 0x00
partition3					times 16 db 0x00
partition4					times 16 db 0x00
							dw MBR.BOOT_SIGNATURE
