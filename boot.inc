; SPDX-License-Identifier: MIT
; LK-LDR/86
; Boot sector interface header
%ifndef SECTOR_SIZE
%define SECTOR_SIZE 0x0200
%define SECTOR_SHIFT 0x09
%endif

; Boot sector structure
BootSector:
.BASE						equ 0x7C00
.FAT_BUFFER					equ .BASE-SECTOR_SIZE
.DIR_BUFFER					equ .BASE-2*SECTOR_SIZE
.DRIVE_NUMBER				equ .BASE+EBPB.RESERVED_BYTE
.END						equ .BASE+0x0200
.LOAD_OFFSET				equ .END-0x0012
.FILE_NAME					equ .END-0x0010
.ENTRY_POINT				equ .END-0x0005
.SIGNATURE					equ .END-0x0002
.FILE_SIZE					equ .END
.FIRST_DATA_CLUSTER			equ .END+0x0002
.BOOT_DRIVE					equ .END+0x0004
.STACK_BASE					equ .END+0x0200
.SECTORS_PER_CYLINDER		equ .STACK_BASE-0x0002
.BOOT_SIGNATURE				equ 0xAA55
