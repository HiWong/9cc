#include "cc.h"

CCOptions cc_options;

static const char *input_file;
static const char *output_file;

static void cc_init()
{
    setlocale(LC_ALL, "");
    register_print_function('k', token_print_function);
    register_print_function('t', type_print_function);
    register_print_function('n', node_print_function);
    init_symbol();
    init_type();
    init_lexer();
}

int main(int argc, const char * argv[])
{
    FILE *fp;
    struct decl *n;
    
    for (int i=1; i < argc; i++) {
	const char *arg = argv[i];
        if (!strcmp(arg, "-o")) {
	    if (++i >= argc) {
		fprintf(stderr, "missing target file while -o option given.\n");
		return EXIT_FAILURE;
	    }
	    output_file = argv[i];
	} else if (arg[0] == '-') {
	    // options
	} else {
	    input_file = arg;
	}
    }

    if (!input_file || !output_file) {
	return EXIT_FAILURE;
    }

    fp = freopen(input_file, "r", stdin);
    if (fp == NULL) {
	perror(input_file);
	return EXIT_FAILURE;
    }
    
    cc_init();
    n = translation_unit();
    print_tree(NODE(n));

    fclose(fp);
	
    return errors > 0 ? EXIT_FAILURE : EXIT_SUCCESS;
}

