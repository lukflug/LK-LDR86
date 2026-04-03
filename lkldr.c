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

#define COPYRIGHT "Copyright (c) 2026 lukflug. Licensed under MIT."
#ifndef VERSION
#define VERSION "UNVERSIONED"
#endif
#ifndef BUILD
#define BUILD "NONE"
#endif

static char *argv0;

static int version(void) {
	printf("LK-LDR/86 Installation Utility version " VERSION " build " BUILD "\n" COPYRIGHT "\n");
	return 0;
}

static void usage(void) {
	printf(
		"Usage: %s <command> [<args>]\n"
		"\n"
		"List of commands:\n"
		"  help        display this message\n"
		"  vbrinstall  install volume boot record\n"
		"  version     display version and copyright information\n", argv0);
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
			argv[i][arg] = 0;
			/* scan for long options */
			for (j = 0; j < longopt_count; j++)
				if (!strcmp(argv[i]+2,longopt[j])) break;
			if (j < longopt_count) {
				/* deal with argument */
				if (equals_sign && longopt_type[j] == NONE) {
					fprintf(stderr,"%s: Option '%s' does not expect argument!\n",argv[0],argv[i]);
					error = 1;
				} else if (equals_sign && longopt_type[j] != NONE) {
					argv[i] += arg;
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

int main(int argc, char **argv) {
	argv0 = argv[0];
	if (argc >= 2) {
		if (!strcmp(argv[1],"help")) {
			usage();
			return 0;
		} else if (!strcmp(argv[1],"version")) return version();
		else if (!strcmp(argv[1],"vbrinstall")) {
			int *posarg = calloc(argc-2,sizeof(int));
			enum option_type shortopt_type[2] = {NONE,NONE};
			char *shortopt_arg[2] = {NULL,NULL};
			int posarg_count = parse_options(argc,argv,"vh",shortopt_type,shortopt_arg,0,NULL,NULL,NULL,posarg);
			printf("%i\n",posarg_count);
			if (shortopt_arg[0]) printf("-v %s\n",shortopt_arg[0]);
			if (shortopt_arg[1]) printf("-h %s\n",shortopt_arg[1]);
			free(posarg);
			return 0;
		}
		fprintf(stderr,"%s: Unrecognized command '%s'!\n",argv[0],argv[1]);
	}

	usage();
	return 1;
}
