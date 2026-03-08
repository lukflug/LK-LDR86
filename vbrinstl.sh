#!/bin/sh
# SPDX-License-Identifier: MIT

usage() {
	cat <<EOF
Usage: $0 [options] [--] <device> [bootsector]

Write <bootsector> to volume boot record of <device>.
If <bootsector> is not specified, the script will automatically detect whether
it is a FAT12, FAT16, or FAT32 partition and install the corresponding boot
sector image (boot12.bin, boot16.bin, or boot32.bin).

Options:
  -f <filename>    specify custom second stage binary (default: LKLDR86.BIN)
  -h               display this help message
  -l <offset>      specify custom load offset (default: 0x0600)
  -n               preserve previous OEM name (overrides -o)
  -o [name]        specify custom OEM name (default: LKLDR86)
  -p <LBA>         specify partition start LBA
  -v               enable verbose mode

The <filename> must be a valid FAT short 8.3 filename.
<offset> must be an unsigned integer smaller than 0x10000 that is not within
the range 0x7800 to 0x7FFF, and must be at least 0x0500.
<name> must be at most eight characters.
<LBA> must be a 32-bit unsigned integer.

Copyright (c) 2026 lukflug. Licensed under MIT.
EOF
}

OPTIND=1
# default value for arguments
partition=0
oemname=
secondstage=
loadoffset=
# binary switches
preserve=0
verbose=0
help=0
unknown=0

while getopts p:o:f:l:nhv name
do
	case $name in
		p) partition=$OPTARG
		   if [ -z "$OPTARG" ]
		   then 
			   unknown=1
		   fi ;;
		o) oemname=$OPTARG
		   if [ -z "$OPTARG" ]
		   then 
			   unknown=1
		   fi ;;
		f) secondstage=$OPTARG
		   if [ -z "$OPTARG" ]
		   then 
			   unknown=1
		   fi ;;
		l) loadoffset=$OPTARG
		   if [ -z "$OPTARG" ]
		   then 
			   unknown=1
		   fi ;;
		n) preserve=1 ;;
		v) verbose=1 ;;
		h) help=1 ;;
		?) unknown=1 ;;
	esac
done

if [ -n "$partition" ]
then
	[ "$partition" -eq "$partition" ] 2>/dev/null
	if [ $? -ne 0 ] || [ "$partition" -lt 0 ] || [ "$partition" -ge 4294967296 ]
	then
		echo "Partition LBA must be a 32-bit unsigned integer!" 1>&2
		unknown=1
	fi
fi

if [ -n "$loadoffset" ]
then
	[ "$loadoffset" -eq "$loadoffset" ] 2>/dev/null
	if [ $? -ne 0 ] || [ "$loadoffset" -lt 0x0500 ] || [ "$loadoffset" -ge 0x10000 ]
	then
		echo "Load offset must be between 0x0500 and 0x10000!" 1>&2
		unknown=1
	fi
	if [ "$loadoffset" -ge 0x7800 ] && [ "$loadoffset" -lt 0x8000 ]
	then
		echo "Load offset must not be between 0x7800 and 0x8000!" 1>&2
		unknown=1
	fi
fi

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
