# LK-LDR/86
LK-LDR/86 is a WIP bootloader designed for operating systems targetting x86 real mode on IBM PC and 100% compatible computers.

To-do list:
- [ ] volume boot record (FAT12, FAT16, FAT32 variants)
- [ ] master boot record (MBR and GPT variants)
- [ ] second stage drive enumeration (int 13h BIOS drives that are either unpartitioned, MBR, or GPT)
- [ ] second stage FAT implementation (read-only FAT12/16/32 implementation)
- [ ] second stage memory manager (with dynamic self-relocation)
- [ ] Forth interpreter
- [ ] debugger with assembler
