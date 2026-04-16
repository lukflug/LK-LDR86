/*
 * SPDX-License-Identifier: MIT
 * LK-LDR/86
 * Installation Utility
 * Should work on any ANSI C-compliant (ANSI X3.159-1989 or ISO/IEC 9899:1990)
 * translation environment targeting a hosted execution environment using ASCII
 * as the basic character set.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>
#include <limits.h>
#include "lkldr.h"

#ifndef VERSION
#define VERSION "UNVERSIONED"
#endif
#ifndef BUILD
#define BUILD "NONE"
#endif

#define SECTOR_SIZE 512

#define BPB_OEM_NAME 0x0003
#define BPB_SECTOR_SIZE 0x000B
#define BPB_CLUSTER_SIZE 0x000D
#define BPB_RESERVED_SECTORS 0x000E
#define BPB_FAT_COUNT 0x0010
#define BPB_ROOT_ENTRIES 0x0011
#define BPB_SECTOR_COUNT 0x0013
#define BPB_MEDIA_DESCRIPTOR 0x0015
#define BPB_FAT_SIZE 0x0016
#define BPB_SECTORS_PER_TRACK 0x0018
#define BPB_HEAD_COUNT 0x001A
#define BPB_HIDDEN_SECTORS 0x001C
#define BPB_SECTOR_COUNT_32 0x0020

#define EBPB_DRIVE_NUMBER 0x0024
#define EBPB_RESERVED_BYTE 0x0025
#define EBPB_SIGNATURE 0x0026
#define EBPB_SERIAL_NUMBER 0x0027
#define EBPB_VOLUME_LABEL 0x002B
#define EBPB_FILE_SYSTEM 0x0036
#define EBPB_END 0x003E

#define EBPB32_FAT_SIZE_32 0x0024
#define EBPB32_FLAGS 0x0028
#define EBPB32_VERSION 0x002A
#define EBPB32_ROOT_DIR_CLUSTER 0x002C
#define EBPB32_FS_INFO_LSN 0x0030
#define EBPB32_RESERVED 0x0034
#define EBPB32_DRIVE_NUMBER 0x0040
#define EBPB32_RESERVED_BYTE 0x0041
#define EBPB32_SIGNATURE 0x0042
#define EBPB32_SERIAL_NUMBER 0x0043
#define EBPB32_VOLUME_LABEL 0x0047
#define EBPB32_FILE_SYSTEM 0x0052
#define EBPB32_END 0x005A

#define BOOT_LOAD_OFFSET (0x0200-0x0006)
#define BOOT_FILE_NAME (0x0200-0x0011)

static char *argv0, *argv1;


static void print_error(const char *diagnostic, const char *filename, int error) {
	if (error) {
		const char *msg = strerror(error);
		fprintf(stderr,"%s: %s: %s '%s': %s\n",argv0,argv1,diagnostic,filename,msg);
	} else fprintf(stderr,"%s: %s: %s '%s'!\n",argv0,argv1,diagnostic,filename);
}


static int checked_fopen(FILE **file, const char *filename, const char *mode) {
	errno = 0;
	if ((*file = fopen(filename,mode)) == NULL) {
		int error = errno;
		print_error("Could not open",filename,error);
		return 1;
	}
	return 0;
}


static int checked_fseek(FILE **file, const char *filename, long int offset, int whence) {
	errno = 0;
	if (fseek(*file,offset,whence)) {
		int error = errno;
		print_error("Failed to seek on",filename,error);
		return 1;
	}
	return 0;
}


static int checked_fread(FILE **file, const char *filename, void *buf, size_t size, size_t nmemb) {
	errno = 0;
	if (fread(buf,size,nmemb,*file) < nmemb) {
		int error = errno;
		print_error("Failed to read from",filename,error);
		return 1;
	}
	return 0;
}


static int checked_fwrite(FILE **file, const char *filename, void *buf, size_t size, size_t nmemb) {
	errno = 0;
	if (fwrite(buf,size,nmemb,*file) < nmemb) {
		int error = errno;
		print_error("Failed to write to",filename,error);
		return 1;
	}
	return 0;
}


enum option_type {
	NONE, OPTIONAL, MANDATORY
};

/*
 * Parse command line options in argc and argv
 * shortopt - string containing all short options
 * shortopt_type - specifies whether option expects (not) having an argument
 * shortopt_arg - will be filled with pointers to argument, or empty string if there is no such argument
 * longopt_count - amount of long options
 * longopt - array of pointers to longopt strings
 * longopt_type - specifies whether option expects (not) having an argument
 * longopt_arg - will be filled with pointers to argument, or empty string if there is no such argument
 * posarg - pointer to array of length argc-2, which will get filled with pointers to positional arguments
 * Return value is the amount of positional arguments
 */
static int parse_options(int argc, char **argv, const char *shortopt, const enum option_type *shortopt_type, const char **shortopt_arg,
                         int longopt_count, const char **longopt, const enum option_type *longopt_type, const char **longopt_arg, const char **posarg) {
	int posarg_count = 0;
	int shortopt_count = strlen(shortopt);
	int i;
	int error = 0;

	for (i = 2; i < argc; i++) {
		if (argv[i][0] != '-' || !argv[i][1]) {
			/* positional argument */
			posarg[posarg_count] = argv[i];
			posarg_count++;
		} else if (argv[i][1] != '-') {
			/* short option */
			char *p;
			for (p = argv[i]+1; *p; p++) {
                /* scan for option */
				char s[2] = {0,0};
				int opt;
				s[0] = *p;
				opt = strcspn(shortopt,s);

				if (opt < shortopt_count) {
					if (!argv[i][2] && shortopt_type[opt] != NONE && i+1 < argc && (argv[i+1][0] != '-' || !argv[i+1][1])) shortopt_arg[opt] = argv[++i];
					else if (shortopt_type[opt] != MANDATORY) shortopt_arg[opt] = "";
					else {
						fprintf(stderr,"%s: %s: Option '-%c' expects argument!\n",argv[0],argv[1],*p);
						error = 1;
					}
				} else {
					fprintf(stderr,"%s: %s: Unrecognized option '-%c'!\n",argv[0],argv[1],*p);
					error = 1;
				}
			}
		} else if (!argv[i][2]) {
			/* double dash => end of options */
			while (++i < argc) {
				posarg[posarg_count] = argv[i];
				posarg_count++;
			}
		} else {
			/* long option */
			int arg = strcspn(argv[i],"=");
			int equals_sign = argv[i][arg];
			int j;
			argv[i][arg] = '\0';

			/* scan for long options */
			for (j = 0; j < longopt_count; j++)
				if (!strcmp(argv[i]+2,longopt[j])) break;

			if (j < longopt_count) {
				/* deal with argument */
				if (equals_sign && longopt_type[j] == NONE) {
					fprintf(stderr,"%s: %s: Option '%s' does not expect argument!\n",argv[0],argv[1],argv[i]);
					error = 1;
				} else if (equals_sign && longopt_type[j] != NONE) longopt_arg[j] = argv[i]+arg+1;
				else if (longopt_type[j] != NONE && i+1 < argc && (argv[i+1][0] != '-' || !argv[i+1][1])) longopt_arg[j] = argv[++i];
				else if (longopt_type[j] != MANDATORY) longopt_arg[j] = "";
				else {
					fprintf(stderr,"%s: %s: Option '%s' expects argument!\n",argv[0],argv[1],argv[i]);
					error = 1;
				}
			} else {
				fprintf(stderr,"%s: %s: Unrecognized option '%s'!\n",argv[0],argv[1],argv[i]);
				error = 1;
			}
		}
	}

	if (!error) return posarg_count;
	return -1;
}


static int parse_fat_filename(char *dest, const char *filename) {
	static const int table[] = {
		1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 0, 0,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
		1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1, 1,
		1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
		0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 1, 1  /* Wikipedia says DEL is not allowed, but fatgen103.doc and MS-DOS source code allow it */
	};
	int file_extension = 0;
	int i;

	for (i = 0; i < 11; i++) {
		unsigned char c = *filename++;
		/* check for end of filename */
		if (!c) {
			while (i < 11) dest[i++] = ' ';
			return 0;
		}

		/* if dot, we've reached file extension */
		if (c == '.' && i <= 8) {
			/* first character cannot be space */
			if (!i) return 1;
			/* pad with space */
			while (i < 8) dest[i++] = ' ';
			file_extension = 1;
			c = *filename++;
		}
		/* if we surpassed 8 characters, and didn't reach dot yet, the filename is too long */
		if (i >= 8 && !file_extension) return 1;

		/* normalize character */
		if (c >= 'a' && c <= 'z') c -= 'a'-'A';
		/* validate character */
		if (c < ' ' || (!i && c == ' ') || (c <= 0x7F && !table[c-' '])) return 1;
		/* do 0xE5 replacement */
		if (!i && c == 0xE5) c = 0x05;
		/* store character */
		dest[i] = c;
	}

	/* if we reached end of filename, we successfully parsed it */
	return *filename != '\0';
}


enum bpb_type {
	RAW, FAT12, FAT16, FAT32
};

static int write_vbr(int verbose, const char *device_path, unsigned long partition_offset, const char *binary_path, enum bpb_type type,
                     int write_oem_name, const char *oem_name, long load_offset, const char *second_stage) {
	int error = 0;
	FILE *device;
	static unsigned char device_buf[SECTOR_SIZE], binary_buf[SECTOR_SIZE]; /* 1 kiB of stack space might be an imposition on some systems */
	const unsigned char *bootsector = binary_buf;
	int i;

	if (verbose) printf("Writing to device '%s' at LBA %lu\n",device_path,partition_offset);
	/* open device */
	if (checked_fopen(&device,device_path,"r+b")) return 1;
	/* ensure we're not buffering */
	setbuf(device,NULL);
	/* make sure seek offset doesn't overflow */
	if (partition_offset >= LONG_MAX/SECTOR_SIZE) {
		fprintf(stderr,"%s: vbrinstall: Integer overflow from partition offset!",argv0);
		error = 1;
		goto end;
	}
	/* seek and read boot sector */
	if (checked_fseek(&device,device_path,partition_offset*SECTOR_SIZE,SEEK_SET)
	    || checked_fread(&device,device_path,device_buf,sizeof(char),SECTOR_SIZE)) {
		error = 1;
		goto end;
	}

	if (binary_path == NULL) {
		unsigned char cluster_size = device_buf[BPB_CLUSTER_SIZE];
		unsigned int reserved_sectors = ((unsigned int)device_buf[BPB_RESERVED_SECTORS+1] << 8) + device_buf[BPB_RESERVED_SECTORS];
		unsigned int root_entries = ((unsigned int)device_buf[BPB_ROOT_ENTRIES+1] << 8) + device_buf[BPB_ROOT_ENTRIES];
		unsigned long total_sectors = ((unsigned int)device_buf[BPB_SECTOR_COUNT+1] << 8) + device_buf[BPB_SECTOR_COUNT];
		unsigned int fat_size_16 = ((unsigned int)device_buf[BPB_FAT_SIZE+1] << 8) + device_buf[BPB_FAT_SIZE];
		unsigned long fat_size = fat_size_16;
		unsigned long first_data_sector, cluster_count = 0;

		if (!total_sectors) total_sectors = ((unsigned long)device_buf[BPB_SECTOR_COUNT_32+3] << 24)
		                                  + ((unsigned long)device_buf[BPB_SECTOR_COUNT_32+2] << 16)
		                                  + ((unsigned long)device_buf[BPB_SECTOR_COUNT_32+1] << 8)
		                                  + device_buf[BPB_SECTOR_COUNT_32];
		if (!fat_size) fat_size = ((unsigned long)device_buf[EBPB32_FAT_SIZE_32+3] << 24)
		                        + ((unsigned long)device_buf[EBPB32_FAT_SIZE_32+2] << 16)
		                        + ((unsigned long)device_buf[EBPB32_FAT_SIZE_32+1] << 8)
		                        + device_buf[EBPB32_FAT_SIZE_32];

		/* sanity check BPB */
		/* logical sector size must be 512 */
		if (device_buf[BPB_SECTOR_SIZE] != (SECTOR_SIZE&0xFF) || device_buf[BPB_SECTOR_SIZE+1] != SECTOR_SIZE >> 8) error = 1;
		/* reserved sector count must be non-zero */
		if (!reserved_sectors) error = 1;
		/* FAT count must be two */
		if (device_buf[BPB_FAT_COUNT] != 2) error = 1;
		/* amount of root entries must be divisible by 32 */
		if (root_entries % (SECTOR_SIZE/32)) error = 1;
		/* total sector count must be non-zero */
		if (!total_sectors) error = 1;
		/* FAT size must be non-zero and not cause numerical overflow */
		if (!fat_size || fat_size > ULONG_MAX/2) error = 1;

		/* determine cluster count */
		first_data_sector = (unsigned long)reserved_sectors + root_entries/(SECTOR_SIZE/32);
		first_data_sector += fat_size*2;
		/* check for integer overflow */
		if (first_data_sector < fat_size*2) error = 1;
		/* ensure there's more total sectors than first data sector */
		if (total_sectors <= first_data_sector) error = 1;
		/* sectors per cluster must be non-zero */
		if (cluster_size) cluster_count = (total_sectors - first_data_sector) / cluster_size;
		/* cluster count must be positive */
		if (!cluster_count) error = 1;

		/* determine FAT type */
		type = FAT32;
		if (cluster_count < 4085) type = FAT12;
		else if (cluster_count < 65525) type = FAT16;
		/* sanity check stuff */
		if (type == FAT32) {
			/* root entries and 16-bit FAT size field must be zero */
			if (root_entries) error = 1;
			if (fat_size_16) error = 1;
			/* check EBPB signature */
			if (device_buf[EBPB32_SIGNATURE] != 0x28 && device_buf[EBPB32_SIGNATURE] != 0x29) error = 1;
			/* check version number */
			if (device_buf[EBPB32_VERSION] || device_buf[EBPB32_VERSION+1]) error = 1;
		} else {
			/* root entries and 16-bit FAT size field must be non-zero */
			if (!root_entries) error = 1;
			if (!fat_size_16) error = 1;
			/* check EBPB signature */
			if (device_buf[EBPB_SIGNATURE] != 0x28 && device_buf[EBPB_SIGNATURE] != 0x29) error = 1;
		}

		if (error) {
			fprintf(stderr,"%s: vbrinstall: Partition must be valid FAT volume for automatic detection!\n",argv0);
			goto end;
		}

		if (type == FAT12) {
			bootsector = boot12;
			if (verbose) printf("FAT type detected: FAT12\n");
		} else if (type == FAT16) {
			bootsector = boot16;
			if (verbose) printf("FAT type detected: FAT16\n");
		} else if (type == FAT32) {
			bootsector = boot32;
			if (verbose) printf("FAT type detected: FAT32\n");
		}
	} else {
		FILE *binary;
		/* open bootsector binary */
		if (verbose) printf("Reading from bootsector binary '%s'\n",binary_path);
		if ((error = checked_fopen(&binary,binary_path,"rb"))) goto end;
		/* read from it */
		error = checked_fread(&binary,binary_path,binary_buf,sizeof(char),SECTOR_SIZE);
		fclose(binary);
		if (error) goto end;
	}

	/* copy first three bytes */
	for (i = 0; i < BPB_OEM_NAME; i++) device_buf[i] = bootsector[i];
	/* copy the stuff after the BPB */
	switch (type) {
	case RAW:
		i = BPB_OEM_NAME + 8;
		if (verbose) printf("Assuming no BPB\n");
		break;
	case FAT12:
	case FAT16:
		i = EBPB_END;
		if (verbose) printf("Assuming FAT12/16 (E)BPB\n");
		break;
	case FAT32:
		i = EBPB32_END;
		if (verbose) printf("Assuming FAT32 (E)BPB\n");
		break;
	}
	for (; i < SECTOR_SIZE; i++) device_buf[i] = bootsector[i];

	/* copy OEM name, if desired */
	if (write_oem_name) {
		const unsigned char *p = bootsector + BPB_OEM_NAME;
		/* if custom OEM name was specified use that instead of the bootsector OEM name */
		if (oem_name[0]) {
			p = (const unsigned char *)oem_name;
			if (verbose) printf("Setting OEM name to '%s'\n",oem_name);
		}
		for (i = 0; i < 8; i++) device_buf[BPB_OEM_NAME + i] = p[i];
	} else if (verbose) printf("Not overwriting OEM name\n");
	/* write custom load offset, if necessary */
	if (load_offset >= 0) {
		if (verbose) printf("Setting load offset to 0x%x\n",(unsigned int)load_offset);
		device_buf[BOOT_LOAD_OFFSET] = load_offset & 0xFF;
		device_buf[BOOT_LOAD_OFFSET+1] = load_offset >> 8;
	}
	/* write custom second stage file name */
	if (second_stage[0]) {
		for (i = 0; i < 11; i++) device_buf[BOOT_FILE_NAME+i] = second_stage[i];
		if (verbose) printf("Setting second stage filename to '%s'\n",second_stage);
	}
	/* write back */
	if (checked_fseek(&device,device_path,partition_offset*SECTOR_SIZE,SEEK_SET)
	    || checked_fwrite(&device,device_path,device_buf,sizeof(char),SECTOR_SIZE)) {
		error = 1;
		goto end;
	}

end:
	fclose(device);
	return error;
}


static void vbrinstall_usage(void) {
	printf(
		"Usage: %s vbrinstall [<options>] [--] <device> [<bootsector> <bpb>]\n"
		"\n"
		"Write <bootsector> to volume boot record of <device>.\n"
		"<bpb> must be none, fat12, fat16, or fat32, and determines what bytes of the\n"
		"BIOS parameter block (BPB) should not be overwritten.\n"
		"If <bootsector> is not specified, the utility will automatically detect whether\n"
		"it is a FAT12, FAT16, or FAT32 partition and install the corresponding boot\n"
		"sector image.\n"
		"\n", argv0);
	printf(
		"Options:\n"
		"  -h, --help                        display this help message\n"
		"      --load-offset=<offset>        specify custom load offset (default:\n"
		"                                    0x0600)\n"
		"      --oem-name[=<name>]           specify custom OEM name, or don't overwrite\n"
		"                                    previous OEM name if no argument is passed\n"
		"                                    (default: LKLDR86)\n"
		"      --partition-offset=<LBA>      specify partition start LBA (default: 0)\n");
	printf(
		"      --second-stage=<filename>     specify custom second stage binary\n"
		"                                    (default: LKLDR86.BIN)\n"
		"  -v, --verbose                     enable verbose mode\n"
		"\n"
		"The <filename> must be a valid FAT short 8.3 filename.\n"
		"<offset> must be an unsigned integer within the range 0x0500 to 0x%04X.\n"
		"<name> must be at most eight characters.\n"
		"<LBA> must be a 32-bit unsigned integer.\n",0x7800-SECTOR_SIZE);
}


static int vbrinstall(int argc, char **argv) {
	int error = 0;
	/* parse options */
	const enum option_type shortopt_type[2] = {NONE,NONE};
	const char *shortopt_arg[2] = {NULL,NULL};
	const char *longopt[6] = {"help","load-offset","oem-name","partition-offset","second-stage","verbose"};
	const enum option_type longopt_type[6] = {NONE,MANDATORY,OPTIONAL,MANDATORY,MANDATORY,NONE};
	const char *longopt_arg[6] = {NULL,NULL,NULL,NULL,NULL,NULL};
	const char **posarg = calloc(argc-2,sizeof(const char **));
	int posarg_count;

	if (posarg == NULL) {
		fprintf(stderr,"%s: panic: Memory allocation failed!\n",argv[0]);
		return EXIT_FAILURE;
	}
	posarg_count = parse_options(argc,argv,"hv",shortopt_type,shortopt_arg,6,longopt,longopt_type,longopt_arg,posarg);

	if (posarg_count >= 0) {
		/* interpret results */
		int verbose = shortopt_arg[1] != NULL || longopt_arg[5] != NULL;
		if (shortopt_arg[0] != NULL || longopt_arg[0] != NULL) vbrinstall_usage();
		else if (posarg_count < 1) {
			fprintf(stderr,"%s: vbrinstall: Device path expected!\n",argv[0]);
			error = 1;
		} else if (posarg_count == 2) {
			fprintf(stderr,"%s: vbrinstall: BPB type expected!\n",argv[0]);
		} else if (posarg_count > 3) {
			fprintf(stderr,"%s: vbrinstall: Too many arguments! Expected 3, but %i provided.\n",argv[0],posarg_count);
			error = 1;
		} else {
			long load_offset = -1;
			unsigned long partition_offset = 0;
			/* use OEM name from boot sector binary by default */
			int write_oem_name = 1;
			char oem_name[9] = "", second_stage[12] = "";
			oem_name[8] = second_stage[11] = '\0';

			if (longopt_arg[1] != NULL) {
				/* parse load offset argument */
				char *end;
				load_offset = strtol(longopt_arg[1],&end,0);
				if (end != longopt_arg[1]+strlen(longopt_arg[1]) || load_offset < 0x0500 || load_offset > 0x7800-SECTOR_SIZE) {
					fprintf(stderr,"%s: vbrinstall: Load offset must be an integer between 0x0500 and 0x%04X!\n",argv[0],0x7800-SECTOR_SIZE);
					error = 1;
				}
			}

			if (longopt_arg[2] != NULL) {
				/* parse OEM name argument */
				int i;
				if (strlen(longopt_arg[2]) > 8) {
					fprintf(stderr,"%s: vbrinstall: OEM name must be at most 8 characters long!\n",argv[0]);
					error = 1;
				}
				/* if argument empty, don't overwrite, otherwise pad it out */
				if (!longopt_arg[2][0]) write_oem_name = 0;
				else {
					for (i = 0; longopt_arg[2][i]; i++) oem_name[i] = longopt_arg[2][i];
					while (i < 8) oem_name[i++] = ' ';
				}
			}

			if (longopt_arg[3] != NULL) {
				/* parse partition offset argument */
				char *end;
				partition_offset = strtoul(longopt_arg[3],&end,0);
				if (end != longopt_arg[3]+strlen(longopt_arg[3]) || partition_offset > 0xFFFFFFFF) {
					fprintf(stderr,"%s: vbrinstall: Partition offset must be 32-bit unsigned integer!\n",argv[0]);
					error = 1;
				}
			}

			if (longopt_arg[4] != NULL) {
				/* parse second stage filename argument */
				if (parse_fat_filename(second_stage,longopt_arg[4])) {
					fprintf(stderr,"%s: vbrinstall: Second stage filename must be legal FAT filename!\n",argv[0]);
					error = 1;
				}
			}

			if (!error) {
				enum bpb_type type = RAW;
				if (posarg_count == 3) {
					if (!strcmp(posarg[2],"none")) type = RAW;
					else if (!strcmp(posarg[2],"fat12")) type = FAT12;
					else if (!strcmp(posarg[2],"fat16")) type = FAT16;
					else if (!strcmp(posarg[2],"fat32")) type = FAT32;
					else {
						fprintf(stderr,"%s: vbrinstall: BPB type must be none, FAT12, FAT16, or FAT32.\n",argv[0]);
						error = 1;
						goto end;
					}
				}
				error = write_vbr(verbose, posarg[0], partition_offset, posarg_count == 3 ? posarg[1] : NULL, type,
				                  write_oem_name, oem_name, load_offset, second_stage);
				goto end;
			}
		}
	}

	if (error) fprintf(stderr,"For more information, try '%s vbrinstall --help'.\n",argv[0]);
end:
	free(posarg);
	return error ? EXIT_FAILURE : EXIT_SUCCESS;
}


int main(int argc, char **argv) {
	if (argc >= 2) {
		argv0 = argv[0];
		argv1 = argv[1];
		if (!strcmp(argv[1],"help") || !strcmp(argv[1],"--help") || !strcmp(argv[1],"-h")) {
			printf(
				"Usage: %s <command> [<args>]\n"
				"\n"
				"List of commands:\n"
				"  help        display this message\n"
				"  vbrinstall  install volume boot record\n"
				"  version     display version and copyright information\n", argv[0]);
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[1],"version")) {
			printf(
				"LK-LDR/86 Installation Utility version " VERSION " build " BUILD "\n"
				"Copyright (c) 2025, 2026 lukflug.\n"
				"Distributed under the terms of the MIT license.\n"
				"There is ABSOLUTELY NO WARRANTY, to the extent permitted by law.\n"
			);
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[1],"vbrinstall")) return vbrinstall(argc,argv);
		fprintf(stderr,"%s: Unrecognized command '%s'!\n",argv[0],argv[1]);
	} else fprintf(stderr,"%s: Command expected!\n",argv[0]);

	fprintf(stderr,"For more information, try '%s help'.\n",argv[0]);
	return EXIT_FAILURE;
}
