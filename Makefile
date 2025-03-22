
ASM_SRC = boot.asm lkldr86.asm
DOCS = $(wildcard doc/*)

BUILD_DIRS = img bin dep
ASM_BIN = $(ASM_SRC:%.asm=bin/%.bin)
EXCLUDE_BIN = bin/boot.bin
DEPFILES = $(ASM_SRC:%.asm=dep/%.d)
IMGFILES = img/160kB.img img/180kB.img img/320kB.img img/360kB.img \
  img/720kB.img img/1200kB.img img/1440kB.img img/16384kB.img img/65536kB.img

ASM = nasm $< -o $@ -MT $@ -MD -MP -MF dep/$*.d -fbin -w-label-redef-late -Ilib
VOLUME_LABEL = LKLDR86

.PHONY: all testqemu clean

all: $(IMGFILES)

testqemu: img/1440kB.img
	qemu-system-i386 -fda img/1440kB.img -soundhw pcspk

testbochs: img/1440kB.img
	-bochs

testbochs-hdd: img/65536kB.img
	-bochs -f hdd.bochsrc

$(IMGFILES): img/%kB.img: $(ASM_BIN) boot.asm | img
	-rm -v $@
	mkdosfs -F 12 -Cvn $(VOLUME_LABEL) $@ $*
	dd if=bin/boot.bin of=$@ bs=1 count=11 conv=notrunc
	dd if=bin/boot.bin of=$@ bs=1 count=450 seek=62 skip=62 conv=notrunc
#	dd if=bin/boot.bin of=$@ bs=1 count=422 seek=90 skip=90 conv=notrunc
	mcopy -D o -onvi $@ $(filter-out $(EXCLUDE_BIN) img, $^) ::/
	mattrib -i $@ -a +r \*.\*
	mattrib -i $@ +s lkldr86.bin boot.asm

$(ASM_BIN): bin/%.bin: %.asm dep/%.d | bin dep
	$(ASM)
	touch $@

$(DEPFILES):

$(BUILD_DIRS):
	mkdir -pv $@/

clean:
	-rm -rvf $(BUILD_DIRS)

include $(wildcard $(DEPFILES))
