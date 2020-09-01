#include "kcc.h"

static char *input_path;

char **include_paths;
static bool opt_E;

static void usage(int status) {
    fprintf(stderr, "kcc <file>\n");
    exit(status);
}

static void add_include_path(char *path) {
    static int len = 2;
    include_paths = realloc(include_paths, sizeof(char *) * len);
    include_paths[len-2] = path;
    include_paths[len-1] = NULL;
    len++;
}

static void add_default_include_paths(char *argv0) {
    // kcc固有のインクルードファイルはkccと同じ場所にある
    // ./include にあると想定している。
    char *buf = calloc(1, strlen(argv0) + 10);
    sprintf(buf, "%s/include", dirname(strdup(argv0)));
    add_include_path(buf);

    add_include_path("/usr/local/include");
    add_include_path("/usr/include/x86_64-linux-gnu");
    add_include_path("/usr/include");
}

static void parse_args(int argc, char **argv) {
    for (int i = 1; i < argc; i++) {
        if (!strcmp(argv[i], "--help"))
            usage(0);

        if (!strcmp(argv[i], "-E")) {
            opt_E = true;
            continue;
        }

        if (!strncmp(argv[i], "-I", 2)) {
            add_include_path(argv[i]+2);
            continue;
        }

        if (argv[i][0] == '-' && argv[i][1] != '\0')
            error("unknown argument: %s", argv[i]);

        input_path = argv[i];
    }

    if (!input_path)
        error("no input files");
}

static void print_tokens(Token *tok) {
    int line = 1;
    for (; tok->kind != TK_EOF; tok = tok->next) {
        if (line > 1 && tok->at_bol)
            printf("\n");
        if (tok->has_space && !tok->at_bol)
            printf(" ");
        printf("%.*s", tok->len, tok->loc);
        line++;
    }
    printf("\n");
}

int main(int argc, char **argv) {
    add_default_include_paths(argv[0]);
    parse_args(argc, argv);

    Token *tok = tokenize_file(input_path);
    if (!tok)
        error("%s: %s", input_path, strerror(errno));

    tok = preprocess(tok);

    if (opt_E) {
        print_tokens(tok);
        exit(0);
    }

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
