
VERSION := 0.0.0
BUILD := 0000

ASM_SRC := lkldr86.asm
BUILD_DIRS := img bin dep
ASM_BIN := $(ASM_SRC:%.asm=bin/%.bin)
BOOT_BIN := bin/boot12.bin bin/boot16.bin bin/boot32.bin
EXCLUDE_BIN := $(BOOT_BIN) bin/lkldr
DEPFILES := $(ASM_SRC:%.asm=dep/%.d) $(BOOT_BIN:bin/%.bin=dep/%.d)

ASM := nasm
ASMFLAGS := -w-label-redef-late
override ASMFLAGS += -MD -MP -fbin

CC := cc
CFLAGS := -ansi -Wall -Wextra -pedantic -O2 -pipe

IMGFILES := img/160kB.img img/180kB.img img/320kB.img img/360kB.img \
  img/720kB.img img/1200kB.img img/1440kB.img img/16384kB.img img/65536kB.img
VOLUME_LABEL := LKLDR86

.PHONY: all testqemu testbochs testbochs-hdd clean

all: $(IMGFILES)

testqemu: img/1440kB.img
	qemu-system-i386 -debugcon stdio -fda img/1440kB.img

testbochs: img/1440kB.img
	-bochs

testbochs-hdd: img/65536kB.img
	-bochs -f hdd.bochsrc

$(IMGFILES): img/%kB.img: $(ASM_BIN) $(BOOT_BIN) lkldr.fs bin/lkldr | img
	-rm -v $@
	mkdosfs -Cvn $(VOLUME_LABEL) $@ $*
	bin/lkldr vbrinstall --verbose $@
	mcopy -D o -onvi $@ $(filter-out $(EXCLUDE_BIN) img, $^) ::/
	mattrib -i $@ -a +r \*.\*
	mattrib -i $@ +s lkldr86.bin lkldr.fs

bin/lkldr: lkldr.c bin/lkldr.h | bin
	$(CC) $(CFLAGS) -Ibin -DVERSION=\"$(VERSION)\" -DBUILD=\"$(BUILD)\" $< -o $@

bin/lkldr.h: $(BOOT_BIN) | bin
	echo '' > $@
	echo 'static const unsigned char boot12[] = {' >> $@
	od -v -t x1 -A n bin/boot12.bin | sed 's/^ /\t0x/;s/ /, 0x/g;s/$$/,/;$$s/,$$//' >> $@
	echo '};' >> $@
	echo '' >> $@
	echo 'static const unsigned char boot16[] = {' >> $@
	od -v -t x1 -A n bin/boot16.bin | sed 's/^ /\t0x/;s/ /, 0x/g;s/$$/,/;$$s/,$$//' >> $@
	echo '};' >> $@
	echo '' >> $@
	echo 'static const unsigned char boot32[] = {' >> $@
	od -v -t x1 -A n bin/boot32.bin | sed 's/^ /\t0x/;s/ /, 0x/g;s/$$/,/;$$s/,$$//' >> $@
	echo '};' >> $@

$(ASM_BIN): bin/%.bin: %.asm dep/%.d | bin dep
	$(ASM) $< -o $@ -MF dep/$*.d $(ASMFLAGS)
	touch $@

$(BOOT_BIN): bin/boot%.bin: boot.asm dep/boot%.d | bin dep
	$(ASM) $< -o $@ -MF dep/boot$*.d -DFAT_TYPE=$* $(ASMFLAGS)
	touch $@

$(DEPFILES):

$(BUILD_DIRS):
	mkdir -pv $@/

clean:
	-rm -rvf $(BUILD_DIRS)

include $(wildcard $(DEPFILES))
