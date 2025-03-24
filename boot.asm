; SPDX-License-Identifier: MIT
; LK-LDR/86
; Boot sector for volume boot record
; Loads a fragmented file on a FAT12, FAT16, or FAT32 partition. Assumes:
; - logical sector size = int 13h sector size
; - exactly two FATs
; - directory entry of file located in first sector of root directory (i.e. first 32 entries)
; - FAT chain of file is fully contained within the first sector of the FAT
; - everything must be within the first 65536 sectors of partition
; - files must be loaded below 0x7800
; Scans to find signature 0xAA55AA55 preceding entry point.
%include 'fat.inc'
%include 'rombios.inc'
%include 'boot.inc'

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
sectorCount32				dd 0x00000000
%if FAT_TYPE == 32
sectorsPerFAT32				dd 0x00000000
mirroringFlags				dw 0x0000
fat32Version				dw 0x0000
rootCluster					dd 0x00000000
fsInfoSector				dd 0x00000000
reserved					times 12 db 0x00
%endif
driveNumber					db 0x00
reservedByte				db 0x00
bootSignature				db EBPB.EBPB_SIGNATURE_B
serialNumber				dd 0x00000000
volumeLabel					db 'LKLDR86    '
fileSystem					db 'FAT12   '


; Set up segment registers and stack and calculate sectors per cylinder
; Attempt to load file first using drive number passed by BIOS, then drive number in BPB
; If it fails, display error message, wait for key stroke, and run int 18h
; dl - boot drive number
; Never returns
bootloader:
			cli
			cld
			jmp 0x0000:.jump												; Force code segment to 0, because BIOS might use 0x07C0:0x0000 instead of 0x0000:0x7C00

.jump		push cs															; Initialize segments and stack
			pop ds
			push cs
			pop es
			push cs															; Interrupts need to be disabled here to guard against early 8086/8088 lacking interrupt shadow (https://www.pcjs.org/documents/manuals/intel/8086/)
			pop ss
			mov sp, BootSector.STACK_BASE
			sti

			mov [BootSector.BOOT_DRIVE], dl									; Save BIOS drive number
			push dx
			mov ax, [sectorsPerTrack]										; Calculate sectors per cylinder
			mul word [headCount]
			pop dx
			push ax
			call loadFile													; Try BIOS drive number first
			mov dl, [driveNumber]											; Try BPB drive number next
			mov [BootSector.BOOT_DRIVE], dl
			call loadFile

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


; Load FAT and directory sectors, and read out file
; dl - drive number
; [BootSector.BOOT_DRIVE] - drive number
; [BootSector.SECTORS_PER_CYLINDER] - pre-calculated sectors per cylinder
; Only returns on error
loadFile:
			xor ah, ah														; Reset disk controller
			int BIOS.DISK_INT

			mov si, loadSector.patch+0x0001									; Reset to CHS
			mov byte [si], 0x00
			mov ah, BIOS.DISK_LBA_CHECK										; Check if LBA supported
			mov bx, BIOS.DISK_LBA_CHECK_IN
			int BIOS.DISK_INT
			jc short .chs
			cmp bx, BIOS.DISK_LBA_CHECK_OUT
			jne short .chs
			mov byte [si], loadSector.lba-loadSector.chs					; Set to LBA if supported

.chs		mov ax, [reservedSectors]										; Load FAT
			mov bp, ax
			mov bx, BootSector.FAT_BUFFER
			call loadSector
			jc short .error

%if FAT_TYPE != 32
			mov ax, [sectorsPerFAT]											; Calculate LSN of first directory entry (= ax)
			shl ax, 1
			add ax, bp
			mov bp, [rootEntries]											; Calculate LSN of first data cluster (= bp)
			add bp, SECTOR_SIZE/FATEntry.SIZE-1
			mov cl, SECTOR_SHIFT-FATEntry.SHIFT
			shr bp, cl
			add bp, ax
			mov [BootSector.FIRST_DATA_CLUSTER], bp							; Store first data cluster
%else
			mov ax, [sectorsPerFAT32]										; Calculate LSN of first data cluster (= bp)
			shl ax, 1
			add bp, ax
			mov [BootSector.FIRST_DATA_CLUSTER], bp							; Store first data cluster
			mov ax, [rootCluster]											; Get first LSN of root directory (= ax)
			times 2 dec ax
			call cn2lsn
%endif
			mov bx, BootSector.DIR_BUFFER									; Load directory sector
			call loadSector
			jc short .error

			call readFile													; Find file and read into memory
			jc short .error

			mov ax, BootSector.BOOT_SIGNATURE								; Scan for entry point
			mov cx, [BootSector.FILE_SIZE]
			shr cx, 1
			mov di, [BootSector.LOAD_OFFSET]
.repeat			repne scasw
				jcxz .error
				scasw
				jne short .repeat
			jmp di															; Transfer control

.error		equ readFile.return


; Read file
; bx = BootSector.DIR_BUFFER
; [BootSector.BOOT_DRIVE] - drive number
; [BootSector.SECTORS_PER_CYLINDER] - pre-calculated sectors per cylinder
; [BootSector.FIRST_DATA_CLUSTER] - LSN of cluster 2 (first cluster of data area)
; cf - set on error; clear otherwise
readFile:
.dirEntryLoop	mov di, fileName											; Compare filenames
				mov si, bx
				mov cx, FATEntry.ATTRIBUTES
				repe cmpsb
				je short .found

				add bx, FATEntry.SIZE										; Go to next entry
				cmp bh, (BootSector.DIR_BUFFER+SECTOR_SIZE) >> 8
				jb short .dirEntryLoop
.error		stc
.return		ret

.found:
%if FAT_TYPE == 32
			cmp word [bx+FATEntry.FIRST_CLUSTER_HIGH], 0x0000				; Check if high word of first cluster number is zero (if FAT32)
			jne short .error
%endif
			lea si, [bx+FATEntry.FIRST_CLUSTER]
			lodsw															; Get first cluster number
			mov bp, [si]													; Get file size
			mov [BootSector.FILE_SIZE], bp
			mov bx, [BootSector.LOAD_OFFSET]								; Get load offset
			add bp, bx														; Save end of file in memory for later

.clusterLoop	push ax
				times 2 dec ax												; Check EOF
%if FAT_TYPE == 12
				cmp ax, FAT12.MIN_EOC_CLUSTER-FAT.MIN_CLUSTER
				jae short .eof
%elif FAT_TYPE == 16
				cmp ax, FAT16.MIN_EOC_CLUSTER-FAT.MIN_CLUSTER
				jae short .eof
%elif FAT_TYPE == 32
				cmp ax, (FAT32.MIN_EOC_CLUSTER&0xFFFF)-FAT.MIN_CLUSTER
				jae short .eof
%else
%error "FAT_TYPE must be 12, 16, or 32!"
%endif
				call cn2lsn
				mov dx, ax
				pop ax
				mov di, cx													; Save sectors per cluster

%if FAT_TYPE == 12
				cmp ax, (SECTOR_SIZE*2)/3									; Check if it is still inside first sector
				jae short .error
				mov si, ax													; Calculate offset inside FAT
				shr si, 1
				pushf
				add si, ax
				popf
				mov ax, [si+BootSector.FAT_BUFFER]							; Get entry value
				jnc short .even
				mov cl, 0x04
				shr ax, cl
.even			and ax, FAT12.CLUSTER_MASK
%elif FAT_TYPE == 16
				cmp ax, SECTOR_SIZE >> 1									; Check if it is still inside first sector
				jae short .error
				mov si, ax													; Calculate offset inside FAT
				shl si, 1
				mov ax, [si+BootSector.FAT_BUFFER]							; Get entry value
%elif FAT_TYPE == 32
				cmp ax, SECTOR_SIZE >> 2									; Check if it is still inside first sector
				jae short .error
				mov si, ax													; Calculate offset inside FAT
				times 2 shl si, 1
				add si, BootSector.FAT_BUFFER								; Load FAT entry
				lodsw
				mov cx, [si]
				and cx, FAT32.CLUSTER_MASK >> 16							; Check if upper 12 bits are zero
				jz short .in_range
				cmp cx, FAT32.CLUSTER_MASK >> 16							; Check if EOF
				jne short .error
				cmp ax, FAT32.MIN_EOC_CLUSTER&0xFFFF
				jb short .error
.in_range:
%else
%error "FAT_TYPE must be 12, 16, or 32!"
%endif

				push ax
				mov ax, dx													; Get LSN
.sectorLoop			cmp bx, bp												; If read would be beyond file size (e.g. the cluster size is really large), assume EOF
					jae short .eof
					push ax
					call loadSector											; Load sector
					pop ax
					jc short .eof
					add bh, SECTOR_SIZE >> 8								; Increase buffer offset
					inc ax													; Increase LSN
					dec di
					jnz short .sectorLoop
				pop ax
				jmp .clusterLoop

.eof		pop ax															; jae = jnc, so carry is clear
			ret


; Conver CN to LSN
; ax - CN-2
cn2lsn:
			mov cl, [sectorsPerCluster]									; Convert CN to LSN
			xor ch, ch
			mul cx
			add ax, [BootSector.FIRST_DATA_CLUSTER]
			ret


; Convert LSN to CHS and load sector
; ax - LSN
; bx - Buffer offset
loadSector:
			xor dx, dx
			add ax, [hiddenSectors]											; Convert LSN to LBA
			adc dx, [hiddenSectors+0x0002]
.patch		jmp short .chs

.chs		div word [BootSector.SECTORS_PER_CYLINDER]						; ax = cylinder = lba/sectorsPerCylinders, dx = blockInCylinder = lba%sectorsPerCylinders
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
			mov dl, [BootSector.BOOT_DRIVE]									; Set drive number

			mov si, 0x0003													; Reset error counter
.read			mov ax, BIOS.DISK_READ1										; Read sector (and guard against various int 13h bugs, http://www.ctyme.com/intr/rb-0607.htm)
				push dx
				stc
				int BIOS.DISK_INT
				sti
				pop dx
				jnc short .return

.retry			dec si														; Retry 3 times (due to unreliability of floppy drives, http://www.ctyme.com/intr/rb-0607.htm)
				jz short .return
				xor ah, ah													; Reset disk
				int BIOS.DISK_INT
				jnc short .read
.return		ret

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
			mov dl, [BootSector.BOOT_DRIVE]									; Set drive number
			mov si, sp														; Invoke int 13h extension
			mov ah, BIOS.DISK_LBA_READ
			int BIOS.DISK_INT												; Don't retry, as we assume that floppies don't support LBA
.stackLoop	inc sp															; Clean stack (and conserve carry)
			loop .stackLoop
			ret


%if FAT_TYPE != 32
errorMessage				db 'Error! Press any key to reboot ...', 0x0D, 0x0A, 0x00
%else
errorMessage				db 'Error!', 0x0D, 0x0A, 0x00
%endif

							times (BootSector.LOAD_OFFSET-BootSector.BASE)-($-$$) db 0x00
fileOffset					dw 0x0600
fileName					db 'LKLDR86 BIN'
			jmp near readFile
							dw BootSector.BOOT_SIGNATURE
