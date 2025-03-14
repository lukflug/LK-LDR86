; SPDX-License-Identifier: MIT
; LK-LDR/86
; Boot sector interface header
SECTOR_SIZE					equ 0x0200

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
.DRIVE_NUMBER				equ .BASE+BPB.RESERVED_BYTE
.FILE0						equ .BASE+SECTOR_SIZE-0x0002-2*BootFile.SIZE
.FILE1						equ .BASE+SECTOR_SIZE-0x0002-BootFile.SIZE
.SIGNATURE					equ .BASE+SECTOR_SIZE-0x0002
.BOOT_SIGNATURE				equ 0xAA55
