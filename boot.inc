; SPDX-License-Identifier: MIT
; LK-LDR/86
; Boot sector interface header
; The volume boot record expects to be loaded in real mode at linear address
; 0x07C00 into a PC-compatible BIOS environment. It loads a fragmented file on a
; FAT partition (the boot sector comes in three variants for FAT12, FAT16, and
; FAT32, which need to match the partition the boot sector is installed on).
; The boot drive is passed to the boot sector via dl. The partition is
; identified based on the hidden sector field on the BPB. If loading fails for
; whatever reasons, the drive number in the EBPB field is tried before
; displaying an error message. If the file is successfully loaded, the boot
; sector jumps to the first byte of the file.
;
; The memory is laid out as follows:
;         +-------------------------+
; 0x07800 |                         | BootSector.DIR_BUFFER
;         | directory sector buffer |
; 0x079FF |                         |
;         +-------------------------+
; 0x07A00 |                         | BootSector.FAT_BUFFER
;         |    FAT sector buffer    |
; 0x07BFF |                         |
;         +-------------------------+
; 0x07C00 |                         | BootSector.BASE
;         |    boot sector code     |
; 0x07DFF |                         |
;         +-------------------------+
; 0x07E00 |                         | BootSector.END
;         |    boot sector data     |
; 0x07FFF |                         |
;         +-------------------------+
; 0x08000 |                         | BootSector.STACK_BASE
; The boot sector code is structured as follows (both in memory and on disk):
;         +-------------------------+
; 0x07C00 |        jmp short        |
; 0x07C01 |          $+??           |
; 0x07C02 |           nop           |
;         +-------------------------+
; 0x07C03 |                         |
;         |       "LKLDR86 "        |
; 0x07C0A |                         |
;         +-------------------------+
; 0x07C0B |                         |
;         |      BPB and EBPB       |
; 0x07C?? |                         |
;         +-------------------------+
;         |                         |
;         ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;         ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;         |                         |
;         +-------------------------+
; 0x07DEE |    file load offset     | BootSector.LOAD_OFFSET
; 0x07DEF |                         |
;         +-------------------------+
; 0x07DF0 |     short filename      | BootSector.FILE_NAME
;         | (in FAT on-disk format) |
; 0x07DFA |  (padded with spaces)   |
;         +-------------------------+
; 0x07DFB |   read file function    | BootSector.ENTRY_POINT
; 0x07DFC |       entry point       |
; 0x07DFD |                         |
;         +-------------------------+
; 0x07DFE |           0x55          | BootSector.BOOT_SIGNATURE
; 0x07DFF |           0xAA          |
;         +-------------------------+
; The boot sector data is structured as follows (filled by the VBR code):
;         +-------------------------+
; 0x07E00 |   size of loaded file   | BootSector.FILE_SIZE
; 0x07E01 |                         |
;         +-------------------------+
;         |                         |
;         ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;         ~~~~~~~~~~~~~~~~~~~~~~~~~~~
;         |       ^^ stack ^^       |
;         +-------------------------+
; 0x07E08 |   int 13h boot drive    | BootSector.BOOT_DRIVE
; 0x07E09 |                         |
;         +-------------------------+
; 0x07FFA |  sectors per cylinder   | BootSector.SECTORS_PER_CYLINDER
; 0x07FFB |                         |
;         +-------------------------+
; 0x07FFC |    LSN of cluster 2     | BootSector.FIRST_DATA_CLUSTER
; 0x07FFD |                         |
; 0x07FFE |                         |
; 0x07FFF |                         |
;         +-------------------------+
;
; By calling BootSector.ENTRY_POINT, additional files may be loaded after the
; VBR loaded the initial file. This entry point assumes:
; - bx = BootSector.DIR_BUFFER
; - df = 0
; - if = 1
; - cs = ds = es = ss = 0
; - the fields in the boot sector data (except the stack) are not modified
; - [Boot.LOAD_OFFSET] and [BootSector.FILE_NAME] contain load offset and
;   filename of the file to be loaded
; The function returns:
; - [BootSector.FILE_SIZE] is set to the size of the loaded file
; - cf is set on error and clear on success
; General-purpose registers and flags other than df and if must not be assumed
; to be preserved. In addition, the first sector of the root directory entry and
; file allocation table must provide enough information to load the file.
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
.STACK_BASE					equ .END+0x0200
.BOOT_DRIVE					equ .STACK_BASE-0x0008
.SECTORS_PER_CYLINDER		equ .STACK_BASE-0x0006
.FIRST_DATA_CLUSTER			equ .STACK_BASE-0x0004
.BOOT_SIGNATURE				equ 0xAA55
