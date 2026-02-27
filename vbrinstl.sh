#!/bin/sh
# SPDX-License-Identifier: MIT

usage() {
	cat <<EOF
Usage: $0 <device> [bootsector] [options]

Write <bootsector> to volume boot record of <device>.
If <bootsector> is not specified, the script will automatically detect whether
it is a FAT12, FAT16, or FAT32 parition and install the corresponding boot
sector image (boot12.bin, boot16.bin, or boot32.bin).

Options:
  -f <filename>    specify custom second stage binary (default: LKLDR86.BIN)
  -h               display this help message
  -l <offset>      specify custom load offset (default: 0x0600)
  -o [name]        specify custom OEM name (default: LKLDR86)
  -p <LBA>         specify partition start LBA
  -v               enable verbose mode

The <filename> must be a valid FAT short 8.3 filename.
<offset> must be an unsigned integer smaller than 0x10000 that is not within
the range 0x7800 to 0x7FFF, and must be at least 0x0500.
If <name> is not specified, the OEM name is preserved. Otherwise, <name> must
be at most eight characters.
<LBA> must be a 32-bit unsigned integer.
EOF
}

OPTIND=1
partition=0
unknown=0
verbose=0
help=1

while getopts p:o:f:l:hv name
do
	case $name in
		p) partition=$OPTARG
		   if [ -z $OPTARG ]
		   then 
			   unknown=1
		   fi ;;
		o) oemname=$OPTARG ;;
		f) secondstage=$OPTARG
		   if [ -z $OPTARG ]
		   then 
			   unknown=1
		   fi ;;
		l) loadoffset=$OPTARG
		   if [ -z $OPTARG ]
		   then 
			   unknown=1
		   fi ;;
		h) help=1 ;;
		?) unknown=1 ;;
	esac
done

if [ $unknown -eq 1 ]
then
	usage
	exit 1
fi

if [ $help -eq 1 ]
then
	usage
	exit 0
fi
