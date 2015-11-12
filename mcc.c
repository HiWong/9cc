/*
 * mcc
 * driver of the c compiler
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <time.h>
#include "sys.h"
#include "mcc.h"
#include "utils.h"

extern int cc_main(int argc, char *argv[]);

static const char *progname;
static int version = VERSION(0, 0);
struct env *ENV;

static void usage(void)
{
#define print_opt(opt, msg)     fprintf(stderr, "  %-20s%s\n", opt, msg)
    fprintf(stderr,
            "OVERVIEW: mcc - A Standard C Compiler v%d.%d\n\n"
            "USAGE: mcc [options] <files>\n\n"
            "OPTIONS:\n", MAJOR(version), MINOR(version));
    print_opt("-ast-dump",       "Only print abstract syntax tree");
    print_opt("-c",              "Only run preprocess, compile and assemble steps");
    print_opt("-E",              "Only run the preprocessor");
    print_opt("-h, --help",      "Display available options");
    print_opt("-I <dir>",        "Add directory to include search path");
    print_opt("-o <file>",       "Write output to <file>");
    print_opt("-S",              "Only run preprocess and compilation steps");
    print_opt("-v, --version",   "Display version and options");
#undef print_opt
}

static void init_env(void)
{
    ENV = zmalloc(sizeof(struct env));
    ENV->uname = get_uname();
    ENV->arch = get_arch();
    ENV->version = version;
}

static const char * tempname(const char *dir, const char *hint)
{
    static long index;
    const char *base = basename(strcopy(hint));
    const char *name = base;
    const char *path;

 beg:
    path = join(dir, name);
    if (file_exists(path)) {
	name = format("%s.%d", base, index++);
	goto beg;
    }
    return path;
}

static bool options_has(struct vector *v, const char *name)
{
    for (int i = 0; i < vec_len(v); i++) {
	const char *option = vec_at(v, i);
	if (!strcmp(option, name))
	    return true;
    }
    return false;
}

static int program(void *context)
{
    struct vector *data = (struct vector *)context;
    const char *ifile = (const char *)vec_at(data, 0);
    struct vector *options = (struct vector *)vec_at(data, 1);
    const char *ofile = NULL;
    if (vec_len(data) > 2)
	ofile = (const char *)vec_at(data, 2);
    
    if (!file_exists(ifile)) {
        fprintf(stderr, "input file '%s' not exists.\n", ifile);
        return EXIT_FAILURE;
    }

    struct vector *v = vec_new();
    vec_push(v, (void *)progname);
    vec_add(v, options);
    vec_push(v, (void *)ifile);
    if (ofile) {
	vec_push(v, (char *)"-o");
	vec_push(v, (void *)ofile);
    }

    return cc_main(vec_len(v), (char **)vtoa(v));
}

static int link(struct vector *ifiles, const char *ofile)
{
    struct vector *v = vec_new();
    vec_push(v, "ld");
    if (ofile) {
	vec_push(v, "-o");
	vec_push(v, (char *)ofile);
    }
    vec_add(v, ifiles);
    vec_push(v, "-lc");
#ifdef CONFIG_DARWIN
    vec_push(v, "-macosx_version_min");
    vec_push(v, "10.11");
#endif
    vec_push(v, "-arch");
    vec_push(v, (char *)ENV->arch);
    return callsys("ld", (char **)vtoa(v));
}

static int assemble(const char *ifile, const char *ofile)
{
    const char *argv[] = {"as", ifile, "-o", ofile, NULL};
    if (!ifile || !ofile)
	return EXIT_FAILURE;
    return callsys("as", (char **)argv);
}

static int translate(const char *ifile, struct vector *options, const char *ofile)
{
    struct vector *v = vec_new();
    vec_push(v, (void *)ifile);
    vec_push(v, options);
    if (ofile)
	vec_push(v, (void *)ofile);

    int ret = runproc(program, (void *)v);
    vec_free(v);
    return ret;
}

int main(int argc, char **argv)
{
    int ret = EXIT_SUCCESS;
    struct vector *inputs = vec_new();
    struct vector *options = vec_new();
    const char *tmpdir;
    const char *output_file = NULL;
    size_t fails = 0;

    progname = argv[0];
    setup_sys();
    init_env();
    
    for (int i=1; i < argc; i++) {
        char *arg = argv[i];
        if (!strcmp(arg, "-h") || !strcmp(arg, "--help") ||
            !strcmp(arg, "-v") || !strcmp(arg, "--version")) {
            usage();
	    return EXIT_SUCCESS;
        } else if (!strcmp(arg, "-o")) {
            if (++i >= argc)
                die("missing file name after '-o'");
            output_file = argv[i];
        } else if (arg[0] == '-') {
            vec_push(options, arg);
        } else {
            vec_push(inputs, arg);
        }
    }

    bool partial = 
	options_has(options, "-E") ||
	options_has(options, "-ast-dump") ||
	options_has(options, "-S") ||
	options_has(options, "-c");
    
    if (argc == 1) {
        usage();
        return EXIT_SUCCESS;
    } else if (vec_len(inputs) == 0) {
        fprintf(stderr, "no input file.\n");
        return EXIT_FAILURE;
    } else if (output_file && vec_len(inputs) > 1 && partial) {
	fprintf(stderr, "mcc: cannot specify -o when generating multiple output files\n");
	return EXIT_FAILURE;
    }

    if (!(tmpdir = mktmpdir()))
	die("Can't make temporary directory.");

    struct vector *objects = vec_new();
    
    for (int i = 0; i < vec_len(inputs); i++) {
	const char *ifile = vec_at(inputs, i);
	const char *iname = basename(strcopy(ifile));
	const char *ofile = NULL;
	int ret;
	if (options_has(options, "-E") || options_has(options, "-ast-dump")) {
	    if (output_file)
		ofile = output_file;
	    ret = translate(ifile, options, ofile);
	} else if (options_has(options, "-S")) {
	    if (output_file)
		ofile = output_file;
	    else
		ofile = replace_suffix(iname, "s");
	    ret = translate(ifile, options, ofile);
	} else if (options_has(options, "-c")) {
	    if (output_file)
		ofile = output_file;
	    else
		ofile = replace_suffix(iname, "o");
	    const char *sfile = tempname(tmpdir, ifile);
	    ret = translate(ifile, options, sfile);
	    if (ret == 0)
		ret = assemble(sfile, ofile);
	} else {
	    const char *sfile = tempname(tmpdir, ifile);
	    ret = translate(ifile, options, sfile);
	    if (ret == 0) {
		ofile = tempname(tmpdir, sfile);
		ret = assemble(sfile, ofile);
		vec_push(objects, (char *)ofile);
	    }
	}
	if (ret == EXIT_FAILURE)
	    fails++;
    }

    if (fails) {
	ret = EXIT_FAILURE;
	fprintf(stderr, "%lu succeed, %lu failed.\n", vec_len(inputs) - fails, fails);
    } else if (!partial) {
	// link
        ret = link(objects, output_file);
    }

    if (tmpdir)
	rmdir(tmpdir);
    return ret;
}
