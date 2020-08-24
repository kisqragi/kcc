#include "kcc.h"

static char *input_path;

static void usage(int status) {
    fprintf(stderr, "kcc <file>\n");
    exit(status);
}

static void parse_args(int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--help"))
            usage(0);

        if (argv[i][0] == '-' && argv[i][1] != '\0')
            error("unknown argument: %s", argv[i]);

        input_path = argv[i];
    }

    if (!input_path)
        error("no input files");
}

int main(int argc, char **argv) {
    parse_args(argc, argv);

    Token *tok = tokenize_file(input_path);
    if (!tok)
        error("%s: %s", input_path, strerror(errno));

    tok = preprocess(tok);
    Program *prog = parse(tok);

    for (Function *fn = prog->fns; fn; fn = fn->next) {
        // オフセットの割り当て
        int offset = fn->is_variadic ? 128 : 32;
        for (Var *var = fn->locals; var; var = var->next) {
            offset = align_to(offset, var->align);
            offset += var->ty->size;
            var->offset = offset;
        }
        fn->stack_size = align_to(offset, 16);
    }

    codegen(prog);

    return 0;
}
