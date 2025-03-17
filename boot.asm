; SPDX-License-Identifier: MIT
; LK-LDR/86
; Boot sector for volume boot record
; Loads two fragmented files, assumes 512 byte sectors, 2 FATs.
; Files must be located in first 32 root entries and must be below cluster 341.
; Everything must be in first 65536 sectors of partition!
; Files must be loaded below 0x7800 (otherwise they're truncated).
%include 'fat.inc'
%include 'rombios.inc'
%include 'boot.inc'

%ifndef FILE_COUNT
%define FILE_COUNT 0x0002
%endif
%ifndef FAT_TYPE
%define FAT_TYPE 12
%endif

use16
cpu 8086
org BootSector.BASE


start:
			jmp short bootloader
			nop

oemName						db 'LKLDR86 '
bytesPerSector				dw SECTOR_SIZE
sectorsPerCluster			db 0x01
reservedSectors				dw 0x0001
fatCount					db 0x02
rootEntries					dw 0x00E0
sectorCount					dw 0x0B40
mediaDescriptor				db 0xF0
sectorsPerFAT				dw 0x0009
sectorsPerTrack				dw 0x0012
headCount					dw 0x0002
hiddenSectors				dd 0x00000000
totalSectors				dd 0x00000000
driveNumber					db 0x00
reserved					db 0x00
bootSignature				db BPB.EBPB_SIGNATURE_B
serialNumber				dd 0x00000000
volumeLabel					db 'LKLDR86    '
fileSystem					db 'FAT12   '


; Set up segment registers and stack and pre-calculate LSN of relevant structures.
; dl - boot drive number
bootloader:
			cli
			cld
			jmp 0x0000:.jump												; Force code segment to 0, because BIOS might use 0x07C0:0x0000 instead of 0x0000:0x7C00

.jump		xor ax, ax														; Initialize segments and stack
			mov ds, ax
			mov es, ax
			mov ss, ax														; Interrupts need to be disabled here to guard against early 8086/8088 bug lacking interrupt shadow (https://www.pcjs.org/documents/manuals/intel/8086/)
			mov sp, BootSector.STACK_BASE
			sti
			mov [reserved], dl												; Save BIOS drive number

			mov ax, [sectorsPerTrack]										; Calculate sectors per cylinder
			mul word [headCount]
			push ax
			mov bx, BIOS.DISK_CHS_MAX_CYLINDERS								; Calculate maximum LBA accessible via CHS
			mul bx
			push dx
			push ax

			call loadFiles													; Try BIOS drive number first
			mov dl, [driveNumber]											; Try BPB drive number next
			mov [reserved], dl
			call loadFiles

			mov si, errorMessage											; Display error message
.loop			lodsb														; Get next byte
				test al, al													; Check if zero
				jz short .break
				mov ah, BIOS.VIDEO_TTY										; Print character
				xor bx, bx
				int BIOS.VIDEO_INT											; bp may not be preserved if it scrolls (http://www.ctyme.com/intr/rb-0106.htm)
				jmp .loop
.break		xor ah, ah														; Wait for key and boot to BASIC
			int BIOS.KEY_INT
			int BIOS.BASIC_INT
			jmp $															; Some BIOSes return on int 18h (http://www.ctyme.com/intr/rb-2241.htm)


; Load both source files
; dl - Drive number
loadFiles:
			xor ah, ah														; Reset disk controller
			int BIOS.DISK_INT

			mov ax, [sectorsPerFAT]											; Calculate LSN of first directory entry
			shl ax, 1
			add ax, [reservedSectors]
			mov bp, [rootEntries]											; Calculate LSN of first data cluster
			add bp, SECTOR_SIZE/FATEntry.SIZE-1
			mov cl, SECTOR_SHIFT-FATEntry.SHIFT
			shr bp, cl
			add bp, ax
			mov bx, BootSector.DIR_BUFFER									; Load directory sector
			call loadSector

.dirEntryLoop	mov cx, FILE_COUNT
				mov di, file0+BootFile.FILENAME								; Check for first file
				call checkEntry
%if FILE_COUNT > 1
				mov di, file1+BootFile.FILENAME								; Check for second file
				call checkEntry
%endif
				jcxz .foundAll
				add bx, FATEntry.SIZE										; Go to next entry
				cmp bh, (BootSector.DIR_BUFFER+SECTOR_SIZE)/256
				jb short .dirEntryLoop
.return		ret

.foundAll	mov ax, [reservedSectors]										; Load FAT
			mov bx, BootSector.FAT_BUFFER
			call loadSector
			mov si, file0													; Actually load the files
			call readFile
%if FILE_COUNT > 1
			mov si, file1
			call readFile
%endif

			mov ax, BootSector.BOOT_SIGNATURE								; Scan for entry point
			mov cx, [file0+BootFile.FILE_SIZE]
			shr cx, 1
			mov di, [file0+BootFile.LOAD_OFFSET]
.repeat			repne scasw
				jcxz .return
				scasw
				jne short .repeat
			jmp di															; Transfer control


; Compare filename against directory entry
; bx - Pointer to entry
; di - Pointer to filename
checkEntry:
			cmp byte [di], 0x00												; Check if filename already found
			je short .found
			push di
			push cx
			mov si, bx														; Compare filenames
			mov cx, FATEntry.ATTRIBUTES
			repe cmpsb
			pop cx
			pop di
			jne short .return

			xor al, al														; Mark file as found
			stosb
			lea si, [bx+FATEntry.FIRST_CLUSTER]
			movsw															; Get first cluster number

%if FILE_COUNT > 1
%ifndef SECOND_FILE_FIXED
			cmp di, file0+BootFile.FILE_SIZE								; Check if have to set the offset for the second file to the end of the first
			jne short .skip
			mov ax, [file0+BootFile.LOAD_OFFSET]							; offset1 = offset0 + size0
			add ax, [si]
			mov [file1+BootFile.LOAD_OFFSET], ax
%endif
%endif

.skip		movsw															; Get file size
.found		dec cx
.return		ret


; Read a file into memory
; si - Pointer to file offset
readFile:
			lodsw															; Get offset
			mov bx, ax
			inc si															; Get first cluster
			lodsw
.clusterLoop	push ax
				times 2 dec ax												; Check EOF
%if FAT_TYPE == 12
				cmp ax, FAT12.MAX_CLUSTER-FAT.MIN_CLUSTER
%elif FAT_TYPE == 16
				cmp ax, FAT16.MAX_CLUSTER-FAT.MIN_CLUSTER
%else
%error "FAT_TYPE must be 12, 16, or 32!"
%endif
				jae short .eof
				mov cl, [sectorsPerCluster]									; Convert CN to LSN
				xor ch, ch
				mul cx
				add ax, bp
				mov dx, ax
				pop ax
				mov di, cx													; Save sectors per cluster

%if FAT_TYPE == 12
				mov si, ax													; Calculate offset inside FAT
				shr si, 1
				pushf
				add si, ax
				cmp si, SECTOR_SIZE-1										; Check if it is still inside first sector
				jae short .error
				mov ax, [si+BootSector.FAT_BUFFER]							; Get entry value
				popf
				jnc short .even
				mov cl, 0x04
				shr ax, cl
.even			and ax, FAT12.CLUSTER_MASK
%elif FAT_TYPE == 16
				mov si, ax													; Calculate offset inside FAT
				shl si, 1
				cmp si, SECTOR_SIZE-1										; Check if it is still inside first sector
				jae short .error
				mov ax, [si+BootSector.FAT_BUFFER]							; Get entry value
%else
%error "FAT_TYPE must be 12, 16, or 32!"
%endif

				push ax
				mov ax, dx													; Get LSN
.sectorLoop			cmp bh, BootSector.DIR_BUFFER/256						; If read would overflow (e.g. the cluster size is really large), assume EOF
					je short .eof
					push ax
					call loadSector											; Load sector
					pop ax
					add bh, SECTOR_SIZE/256									; Increase buffer offset
					inc ax													; Increase LSN
					dec di
					jnz short .sectorLoop
				pop ax
				jmp .clusterLoop

.error		mov sp, BootSector.STACK_BASE-0x000A
.eof		pop ax
.return		ret


; Convert LSN to CHS and load sector
; ax - LSN
; bx - Buffer offset
loadSector:
			xor dx, dx
			add ax, [hiddenSectors]											; Convert LSN to LBA
			adc dx, [hiddenSectors+0x0002]
			cmp dx, [BootSector.STACK_BASE-0x0004]							; Check if doing CHS or LBA
			ja short .lba
			jb short .chs
			cmp ax, [BootSector.STACK_BASE-0x0006]
			jae short .lba
.chs		div word [BootSector.STACK_BASE-0x0002]							; ax = cylinder = lba/sectorsPerCylinders, dx = blockInCylinder = lba%sectorsPerCylinders
			mov ch, al														; Set cylinder number
			xor al, al
			times 2 shr ax, 1
			mov cl, al

			mov ax, dx														; ax = head = blockInCylinder/headCount, dx = sector-1 = blockInCylinder%headCount
			xor dx, dx
			div word [sectorsPerTrack]
			inc dl															; Store sector number
			or cl, dl
			mov dh, al														; Set head number
			mov dl, [reserved]												; Set drive number

			mov si, 0x0003													; Reset error counter
.read			mov ax, BIOS.DISK_READ1										; Read sector (and guard against various int 13h bugs, http://www.ctyme.com/intr/rb-0607.htm)
				push dx
				stc
				int BIOS.DISK_INT
				sti
				pop dx
				jnc short .return

.retry			dec si														; Retry 3 times (due to unreliability of floppy drives, http://www.ctyme.com/intr/rb-0607.htm)
				jz short .error
				xor ah, ah													; Reset disk
				int BIOS.DISK_INT
				jc short .error
				jmp .read

.return		equ readFile.return
.error		equ readFile.error

.lba		xor si, si														; Construct DAP on stack
			push si															; Higher dword of LBA = 0
			push si
			push dx															; Lower dword of LBA
			push ax
			push es															; Buffer address
			push bx
			inc si															; Transfer one word
			push si
			mov cx, BIOS.DISK_DAP_SIZE										; Size of packet
			push cx
			mov dl, [reserved]												; Set drive number
			mov si, sp														; Invoke int 13h extension
			mov ah, BIOS.DISK_LBA_READ
			int BIOS.DISK_INT
			jc short .error													; Don't retry, as we assume that floppies don't support LBA
			add sp, cx														; Clean stack
			jmp .return


errorMessage				db 'Error!', 0x0D, 0x0A, 0x00

							times (BootSector.FILE0-BootSector.BASE)-($-$$) db 0x00
file0						dw 0x0600
							db 'LKLDR86 BIN'
file1						dw 0x0000
							db 'LKLDR   FS '
							dw BootSector.BOOT_SIGNATURE
