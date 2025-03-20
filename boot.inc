; SPDX-License-Identifier: MIT
; LK-LDR/86
; Boot sector interface header
%ifndef SECTOR_SIZE
%define SECTOR_SIZE 0x0200
%define SECTOR_SHIFT 0x09
%endif

; Boot sector file structure
BootFile:
.LOAD_OFFSET				equ 0x0000
.FILENAME					equ 0x0002
.FIRST_CLUSTER				equ 0x0003
.FILE_SIZE					equ 0x0005
.SIZE						equ 0x000D

; Boot sector structure
BootSector:
.BASE						equ 0x7C00
.STACK_BASE					equ 0x8000
.FAT_BUFFER					equ .BASE-SECTOR_SIZE
.DIR_BUFFER					equ .BASE-2*SECTOR_SIZE
.DRIVE_NUMBER				equ .BASE+EBPB.RESERVED_BYTE
.FILE0						equ .BASE+0x01FE-2*BootFile.SIZE
.FILE1						equ .BASE+0x01FE-BootFile.SIZE
.SIGNATURE					equ .BASE+SECTOR_SIZE-0x0002
.BOOT_SIGNATURE				equ 0xAA55
