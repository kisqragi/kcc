#include "kcc.h"

static int align_to(int n, int align) {
    return (n + align - 1) & ~(align - 1);
}

int main(int argc, char **argv) {
    if (argc != 2)
        error("%s: invalid number of arguments\n", argv[0]);

    Token *tok = tokenize(argv[1]);
    Program *prog = parse(tok);

    for (Function *fn = prog->fns; fn; fn = fn->next) {
        // オフセットの割り当て
        int offset = 32;    // callee-save registers用の32Byte
        for (Var *var = fn->locals; var; var = var->next) {
            offset += var->ty->size;
            var->offset = offset;
        }
        fn->stack_size = align_to(offset, 16);
    }

    codegen(prog);

    return 0;
}
