#include "kcc.h"

static int top;
static int labelseq = 1;

static char *reg(int idx) {
    static char *r[] = {"r10", "r11", "r12", "r13", "r14", "r15"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: %d", idx);
    return r[idx];
}

static void gen_expr(Node *node);

static void gen_addr(Node *node) {
    switch (node->kind) {
        case ND_VAR:
            printf("    lea %s, [rbp-%d]\n", reg(top++), node->var->offset);
            return;
        case ND_DEREF:
            gen_expr(node->lhs);
            return;
    }

    error_tok(node->tok, "not an lvalue");
}

static void load(void) {
    printf("    mov %s, [%s]\n", reg(top-1), reg(top-1));
}

static void store(void) {
    printf("    mov [%s], %s\n", reg(top-1), reg(top-2));
    top--;
}

static void gen_expr(Node *node) {
    switch (node->kind) {
        case ND_NUM:
            printf("    mov %s, %lu\n", reg(top++), node->val);
            return;
        case ND_VAR:
            gen_addr(node);
            load();
            return;
        case ND_DEREF:
            gen_expr(node->lhs);
            load();
            return;
        case ND_ADDR:
            gen_addr(node->lhs);
            return;
        case ND_ASSIGN:
            gen_expr(node->rhs);
            gen_addr(node->lhs);
            store();
            return;
    }


    gen_expr(node->lhs);
    gen_expr(node->rhs);

    char *rd = reg(top - 2);
    char *rs = reg(top - 1);
    top--;

    switch (node->kind) {
        case ND_ADD:
            printf("    add %s, %s\n", rd, rs);
            return;
        case ND_SUB:
            printf("    sub %s, %s\n", rd, rs);
            return;
        case ND_MUL:
            printf("    imul %s, %s\n", rd, rs);
            return;
        case ND_DIV:
            printf("    mov rax, %s\n", rd);
            // RAX -> RDX:RAX (128bitに拡張)
            printf("    cqo\n");
            printf("    idiv %s\n", rs);
            printf("    mov %s, rax\n", rd);
            return;
        case ND_EQ:
            printf("    cmp %s, %s\n", rd, rs);
            printf("    sete al\n");
            printf("    movzb %s, al\n", rd);
            return;
        case ND_NE:
            printf("    cmp %s, %s\n", rd, rs);
            printf("    setne al\n");
            printf("    movzb %s, al\n", rd);
            return;
        case ND_LT:
            printf("    cmp %s, %s\n", rd, rs);
            printf("    setl al\n");
            printf("    movzb %s, al\n", rd);
            return;
        case ND_LE:
            printf("    cmp %s, %s\n", rd, rs);
            printf("    setle al\n");
            printf("    movzb %s, al\n", rd);
            return;
        default:
            error_tok(node->tok, "invalid expression");
    }
}

static void gen_stmt(Node *node) {
    switch (node->kind) {
        case ND_IF: {
            int seq =  labelseq++;
            if (node->els) {
                gen_expr(node->cond);
                printf("    cmp %s, 0\n", reg(--top));
                printf("    je .L.else.%d\n", seq);
                gen_stmt(node->then);
                printf(".L.else.%d:\n", seq);
                gen_stmt(node->els);
                printf("L.end.%d:\n", seq);
            } else {
                gen_expr(node->cond);
                printf("    cmp %s, 0\n", reg(--top));
                printf("    je .L.end.%d\n", seq);
                gen_stmt(node->then);
                printf(".L.end.%d:\n", seq);
            }
            return;
        }
        case ND_FOR: {
            int seq = labelseq++;
            if (node->init)
                gen_stmt(node->init);
            printf(".L.begin.%d:\n", seq);
            if (node->cond) {
                gen_expr(node->cond);
                printf("    cmp %s, 0\n", reg(--top));
                printf("    je .L.end.%d\n", seq);
            }
            gen_stmt(node->then);
            if (node->inc)
                gen_stmt(node->inc);
            printf("    jmp .L.begin.%d\n", seq);
            printf(".L.end.%d:\n", seq);
            return;
        }
        case ND_BLOCK:
            for (Node *n = node->body; n; n = n->next)
                gen_stmt(n);
            return;
        case ND_RETURN:
            gen_expr(node->lhs);
            printf("    mov rax, %s\n", reg(--top));
            printf("    jmp .L.return\n");
            return;
        case ND_EXPR_STMT:
            gen_expr(node->lhs);
            top--;
            return;
        default:
            error_tok(node->tok, "invalid statement");
    }
}

void codegen(Function *prog) {
    
    printf(".intel_syntax noprefix\n");
    printf(".globl main\n");
    printf("main:\n");

    // プロローグ
    // callee-saved
    // 呼び出し先がレジスタを保存する　
    printf("    push rbp\n");
    printf("    mov rbp, rsp\n");
    printf("    sub rsp, %d\n", prog->stack_size);
    printf("    mov [rbp-8], r12\n");
    printf("    mov [rbp-16], r13\n");
    printf("    mov [rbp-24], r14\n");
    printf("    mov [rbp-32], r15\n");

    // アセンブリのコードを生成する
    for (Node *n = prog->node; n; n = n->next) {
        gen_stmt(n);
        assert(top == 0);
    }

    // 退避させたレジスタの値を元に戻す
    printf(".L.return:\n");
    printf("    mov r12, [rbp-8]\n");
    printf("    mov r13, [rbp-16]\n");
    printf("    mov r14, [rbp-24]\n");
    printf("    mov r15, [rbp-32]\n");
    printf("    mov rsp, rbp\n");
    printf("    pop rbp\n");
    printf("    ret\n");

}
