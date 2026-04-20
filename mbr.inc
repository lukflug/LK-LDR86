; SPDX-License-Identifier: MIT
; Master Boot Record header
%ifndef SECTOR_SIZE
%define SECTOR_SIZE 0x0200
%define SECTOR_SHIFT 0x09
%endif

; MBR structure
MBR:
.LOAD_BASE					equ 0x7C00
.RELOC_BASE					equ 0x0600
.TIMESTAMP_START			equ 0x00DA
.DRIVE_NUMBER				equ 0x00DC
.TIMESTAMP_SECOND			equ 0x00DD
.TIMESTAMP_MINUTE			equ 0x00DE
.TIMESTAMP_HOUR				equ 0x00DF
.TIMESTAMP_END				equ 0x00E0
.DISK_SIGNATURE				equ 0x01B8
.PARTITION_TABLE			equ 0x01BE
.SIGNATURE					equ 0x01FE
.END						equ 0x0200
.BOOT_SIGNATURE				equ 0xAA55

; Partition entry
PartitionEntry:
.STATUS						equ 0x0000
.ACTIVE_MASK				equ 0x80
.BEGIN_HEAD					equ 0x0001
.BEGIN_CYLINDER_SECTOR		equ 0x0002
.TYPE						equ 0x0004
.END_HEAD					equ 0x0005
.END_CYLINDER_SECTOR		equ 0x0006
.LBA_START					equ 0x0008
.LBA_SIZE					equ 0x000C
.SIZE						equ 0x0010
