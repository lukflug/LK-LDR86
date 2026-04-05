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

#define COPYRIGHT "Copyright (c) 2026 lukflug. Licensed under MIT."
#ifndef VERSION
#define VERSION "UNVERSIONED"
#endif
#ifndef BUILD
#define BUILD "NONE"
#endif


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
 * posarg - pointer to array of length argc-2, which will get filled with the argv indices of positional arguments
 * Return value is the amount of positional arguments
 */
static int parse_options(int argc, char **argv, char *shortopt, enum option_type *shortopt_type, char **shortopt_arg,
                         int longopt_count, char **longopt, enum option_type *longopt_type, char **longopt_arg, int *posarg) {
	int posarg_count = 0;
	int shortopt_count = strlen(shortopt);
	int i;
	int error = 0;

	for (i = 2; i < argc; i++) {
		if (argv[i][0] != '-' || !argv[i][1]) {
			/* positional argument */
			posarg[posarg_count] = i;
			posarg_count++;
		} else if (argv[i][1] != '-' && !argv[i][2]) {
			/* single short option */
			int opt = strcspn(shortopt,argv[i]+1);
			if (opt < shortopt_count) {
				if (shortopt_type[opt] != NONE && i+1 < argc && (argv[i+1][0] != '-' || !argv[i+1][1])) shortopt_arg[opt] = argv[++i];
				else if (shortopt_type[opt] != MANDATORY) shortopt_arg[opt] = "";
				else {
					fprintf(stderr,"%s: Option '%s' expects argument!\n",argv[0],argv[i]);
					error = 1;
				}
			} else {
				fprintf(stderr,"%s: Unrecognized option '%s'!\n",argv[0],argv[i]);
				error = 1;
			}
		} else if (argv[i][1] != '-' && argv[i][2]) {
			/* multiple short options */
			char *p;
			for (p = argv[i]+1; *p; p++) {
                /* scan for option */
				char s[2] = {0,0};
				int opt;
				s[0] = *p;
				opt = strcspn(shortopt,s);

				if (opt < shortopt_count) {
					if (shortopt_type[opt] != MANDATORY) shortopt_arg[opt] = "";
					else {
						fprintf(stderr,"%s: Option '-%c' expects argument!\n",argv[0],*p);
						error = 1;
					}
				} else {
					fprintf(stderr,"%s: Unrecognized option '-%c'!\n",argv[0],*p);
					error = 1;
				}
			}
		} else if (!argv[i][2]) {
			/* double dash => end of options */
			while (++i < argc) {
				posarg[posarg_count] = i;
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
					fprintf(stderr,"%s: Option '%s' does not expect argument!\n",argv[0],argv[i]);
					error = 1;
				} else if (equals_sign && longopt_type[j] != NONE) {
					argv[i] += arg+1;
					longopt_arg[j] = argv[i];
				} else if (longopt_type[j] != NONE && i+1 < argc && (argv[i+1][0] != '-' || !argv[i+1][1])) longopt_arg[j] = argv[++i];
				else if (longopt_type[j] != MANDATORY) longopt_arg[j] = "";
				else {
					fprintf(stderr,"%s: Option '%s' expects argument!\n",argv[0],argv[i]);
					error = 1;
				}
			} else {
				fprintf(stderr,"%s: Unrecognized option '%s'!\n",argv[0],argv[i]);
				error = 1;
			}
		}
	}

	if (!error) return posarg_count;
	return -1;
}


static int parse_fat_filename (char *dest, char *filename) {
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
		if (c == '\0') {
			while (i < 11) dest[i++] = ' ';
			return 0;
		}

		/* if dot, we've reached file extension */
		if (c == '.' && i <= 8) {
			/* first character cannot be space */
			if (i == 0) return 1;
			/* pad with space */
			while (i < 8) dest[i++] = ' ';
			file_extension = 1;
			c = *filename++;
		}
		/* if we surpassed 8 characters, and didn't reach dot yet, the filename is too long */
		if (i >= 8 && !file_extension) return 1;

		/* normalize character */
		if (c >= 'a' && c <= 'z') c -= 0x20;
		/* validate character */
		if (c < ' ' || (i == 0 && c == ' ') || (c <= 0x7F && !table[c-' '])) return 1;
		/* do 0xE5 replacement */
		if (i == 0 && c == 0xE5) c = 0x05;
		/* store character */
		dest[i] = c;
	}

	/* if we reached end of filename, we successfully parsed it */
	return *filename != '\0';
}


static void vbrinstall_usage(char *argv0) {
	printf(
		"Usage: %s vbrinstall [<options>] [--] <device> [bootsector]\n"
		"\n"
		"Write <bootsector> to volume boot record of <device>.\n"
		"If <bootsector> is not specified, the script will automatically detect whether\n"
		"it is a FAT12, FAT16, or FAT32 partition and install the corresponding boot\n"
		"sector image (boot12.bin, boot16.bin, or boot32.bin).\n"
		"\n", argv0);
	printf(
		"Options:\n"
		"  -h, --help                        display this help message\n"
		"      --load-offset=<offset>        specify custom load offset (default:\n"
		"                                    0x0600)\n"
		"      --oem-name=[<name>]           specify custom OEM name, or preserve\n"
		"                                    previous OEM name if no argument is passed\n"
		"                                    (default: use bootsector OEM name)\n"
		"      --partition-offset=<LBA>      specify partition start LBA (default: 0)\n");
	printf(
		"      --second-stage=<filename>     specify custom second stage binary\n"
		"                                    (default: LKLDR86.BIN)\n"
		"  -v, --verbose                     enable verbose mode\n"
		"\n"
		"The <filename> must be a valid FAT short 8.3 filename.\n"
		"<offset> must be an unsigned integer smaller than 0x10000 that is not within\n"
		"the range 0x7800 to 0x7FFF, and must be at least 0x0500.\n"
		"<name> must be at most eight characters.\n"
		"<LBA> must be a 32-bit unsigned integer.\n");
}


static int vbrinstall(int argc, char **argv) {
	int error = 0;
	/* parse options */
	int *posarg = calloc(argc-2,sizeof(int));
	enum option_type shortopt_type[2] = {NONE,NONE};
	char *shortopt_arg[2] = {NULL,NULL};
	char *longopt[6] = {"help","load-offset","oem-name","partition-offset","second-stage","verbose"};
	enum option_type longopt_type[6] = {NONE,MANDATORY,OPTIONAL,MANDATORY,MANDATORY,NONE};
	char *longopt_arg[6] = {NULL,NULL,NULL,NULL,NULL,NULL};
	int posarg_count;

	if (!posarg) {
		fprintf(stderr,"%s: panic: Memory allocation failed!\n",argv[0]);
		return EXIT_FAILURE;
	}
	posarg_count = parse_options(argc,argv,"hv",shortopt_type,shortopt_arg,5,longopt,longopt_type,longopt_arg,posarg);

	if (posarg_count >= 0) {
		/* interpret results */
		int verbose = shortopt_arg[1] != NULL || longopt_arg[5] != NULL;
		if (shortopt_arg[0] != NULL || longopt_arg[0] != NULL) vbrinstall_usage(argv[0]);
		else if (posarg_count < 1) {
			fprintf(stderr,"%s: vbrinstall: Device path expected!\n",argv[0]);
			error = 1;
		} else if (posarg_count > 2) {
			fprintf(stderr,"%s: vbrinstall: Too many arguments! Expected 2, but %i provided.\n",argv[0],posarg_count);
			error = 1;
		} else {
			long load_offset = 0x0600;
			unsigned long partition_offset = 0;
			/* use OEM name from boot sector binary by default */
			int write_oem_name = 1;
			char oem_name[8] = "", second_stage[] = "LKLDR86 BIN";

			if (longopt_arg[1] != NULL) {
				/* parse load offset argument */
				char *end;
				load_offset = strtol(longopt_arg[1],&end,0);
				if (end != longopt_arg[1]+strlen(longopt_arg[1]) || load_offset < 0x0500 || load_offset >= 0x10000) {
					fprintf(stderr,"%s: vbrinstall: Load offset must be an integer between 0x0500 and 0x10000!\n",argv[0]);
					error = 1;
				}
				if (load_offset >= 0x7800 && load_offset < 0x8000) {
					fprintf(stderr,"%s: vbrinstall: Load offset must not be between 0x7800 and 0x8000!\n",argv[0]);
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
				if (end != longopt_arg[1]+strlen(longopt_arg[3])) {
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
				FILE *device;
				errno = 0;
				device = fopen(argv[posarg[0]],"r+b");
				if (!device) {
					if (errno) {
						char *msg = strerror(errno);
						fprintf(stderr,"%s: vbrinstall: Could not open '%s': %s\n",argv[0],argv[posarg[0]],msg);
					} else fprintf(stderr,"%s: vbrinstall: Could not open '%s'!\n",argv[0],argv[posarg[0]]);
					goto abort;
				}
				setbuf(device,NULL);
			}
		}
	}

	if (error) fprintf(stderr,"For more information, try '%s vbrinstall --help'.\n",argv[0]);
abort:
	free(posarg);
	return error ? EXIT_FAILURE : EXIT_SUCCESS;
}


int main(int argc, char **argv) {
	if (argc >= 2) {
		if (!strcmp(argv[1],"help") || !strcmp(argv[1],"--help") || !strcmp(argv[1],"-h")) {
			printf(
				"Usage: %s <command> [<args>]\n"
				"\n"
				"List of commands:\n"
				"  help        display this message\n"
				"  vbrinstall  install volume boot record\n"
				"  version     display version and copyright information\n", argv[0]);
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[1],"version")) {
			printf("LK-LDR/86 Installation Utility version " VERSION " build " BUILD "\n" COPYRIGHT "\n");
			return EXIT_SUCCESS;
		} else if (!strcmp(argv[1],"vbrinstall")) return vbrinstall(argc,argv);
		fprintf(stderr,"%s: Unrecognized command '%s'!\n",argv[0],argv[1]);
	} else fprintf(stderr,"%s: Command expected!\n",argv[0]);

	fprintf(stderr,"For more information, try '%s help'.\n",argv[0]);
	return EXIT_FAILURE;
}
