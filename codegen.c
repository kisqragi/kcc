#include "kcc.h"

static int top;
static int brknum;
static int contnum;
static int labelseq = 1;
static char *argreg8[] = {"dil", "sil", "dl", "cl", "r8b", "r9b"};
static char *argreg16[] = {"di", "si", "dx", "cx", "r8w", "r9w"};
static char *argreg32[] = {"edi", "esi", "edx", "ecx", "r8d", "r9d"};
static char *argreg64[] = {"rdi", "rsi", "rdx", "rcx", "r8", "r9"};
static Function *current_fn;

static char *reg(int idx) {
    static char *r[] = {"r10", "r11", "r12", "r13", "r14", "r15"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: %d", idx);
    return r[idx];
}

static char *xreg(Type *ty, int idx) {
    if (ty->base || ty->size == 8)
        return reg(idx);

    static char *r[] = {"r10d", "r11d", "r12d", "r13d", "r14d", "r15d"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: %d", idx);
    return r[idx];
}

static char *freg(int idx) {
    static char *r[] = {"xmm8", "xmm9", "xmm10", "xmm11", "xmm12", "xmm13"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: %d", idx);
    return r[idx];
}

static void gen_expr(Node *node);
static void gen_stmt(Node *node);

static void gen_addr(Node *node) {
    switch (node->kind) {
        case ND_VAR:
            if (node->var->is_local)
                printf("    lea %s, [rbp-%d]\n", reg(top++), node->var->offset);
            else
                //printf("    mov %s, offset %s\n", reg(top++), node->var->name);
                printf("    lea %s, QWORD PTR %s[rip]\n", reg(top++), node->var->name);
            return;
        case ND_DEREF:
            gen_expr(node->lhs);
            return;
        case ND_COMMA:
            gen_expr(node->lhs);
            top--;
            gen_addr(node->rhs);
            return;
        case ND_MEMBER:
            gen_addr(node->lhs);
            printf("    add %s, %d\n", reg(top-1), node->member->offset);
            return;
    }

    error_tok(node->tok, "not an lvalue");
}

static void load(Type *ty) {
    if (ty->kind == TY_ARRAY || ty->kind == TY_STRUCT || ty->kind == TY_FUNC)
        return;

    if (ty->kind == TY_FLOAT) {
        printf("    movss %s, [%s]\n", freg(top-1), reg(top-1));
        return;
    }

    if (ty->kind == TY_DOUBLE) {
        printf("    movsd %s, [%s]\n", freg(top-1), reg(top-1));
        return;
    }

    char *rs = reg(top-1);
    char *rd = xreg(ty, top-1);
    char *insn = ty->is_unsigned ? "movzx" : "movsx";

    if (ty->size == 1)
        printf("    %s %s, byte ptr [%s]\n", insn, rd, rs);
    else if (ty->size == 2)
        printf("    %s %s, word ptr [%s]\n", insn, rd, rs);
    else if (ty->size == 4)
        printf("    mov %s, dword ptr [%s]\n", rd, rs);
    else
        printf("    mov %s, [%s]\n", rd, rs);
}

static void store(Type *ty) {
    char *rd = reg(top - 1);
    char *rs = reg(top - 2);

    if (ty->kind == TY_STRUCT) {
        for (int i = 0; i < ty->size; i++) {
            printf("    mov al, [%s+%d]\n", rs, i);
            printf("    mov [%s+%d], al\n", rd, i);
        }
    }
    else if (ty->kind == TY_FLOAT)
        printf("    movss [%s], %s\n", rd, freg(top-2));
    else if (ty->kind == TY_DOUBLE)
        printf("    movsd [%s], %s\n", rd, freg(top-2));
    else if (ty->size == 1)
        printf("    mov [%s], %sb\n", rd, rs);
    else if (ty->size == 2)
        printf("    mov [%s], %sw\n", rd, rs);
    else if (ty->size == 4)
        printf("    mov [%s], %sd\n", rd, rs);
    else
        printf("    mov [%s], %s\n", rd, rs);
    top--;
}

static void cmp_zero(Type *ty) {
    if (ty->kind == TY_FLOAT) {
        printf("    xorps xmm0, xmm0\n");
        printf("    ucomiss %s, xmm0\n", freg(--top));
    }
    else if (ty->kind == TY_DOUBLE) {
        printf("    xorpd xmm0, xmm0\n");
        printf("    ucomisd %s, xmm0\n", freg(--top));
    } else {
        printf("    cmp %s, 0\n", reg(--top));
    }
}

static void cast(Type *from, Type *to) {
    if (to->kind == TY_VOID)
        return;

    char *r = reg(top - 1);
    char *fr = freg(top - 1);

    if (to->kind == TY_BOOL) {
        cmp_zero(from);
        printf("    setne %sb\n", reg(top));
        printf("    movsx %s, %sb\n", reg(top), reg(top));
        top++;
        return;
    }

    if (from->kind == TY_FLOAT) {
        if (to->kind == TY_FLOAT)
            return;

        if (to->kind == TY_DOUBLE)
            printf("    cvtss2sd %s, %s\n", fr, fr);
        else
            printf("    cvttss2si %s, %s\n", r, fr);
        return;
    }

    if (from->kind == TY_DOUBLE) {
        if (to->kind == TY_DOUBLE)
            return;

        if (to->kind == TY_FLOAT)
            printf("    cvtsd2ss %s, %s\n", fr, fr);
        else
            printf("    cvttsd2si %s, %s\n", r, fr);
        return;
    }

    if (to->kind == TY_FLOAT) {
        printf("    cvtsi2ss %s, %s\n", fr, r);
        return;
    }

    if (to->kind == TY_DOUBLE) {
        printf("    cvtsi2sd %s, %s\n", fr, r);
        return;
    }

    char *insn = to->is_unsigned ? "movzx" : "movsx";

    if (to->size == 1)
        printf("    %s %s, %sb\n", insn, r, r);
    else if (to->size == 2)
        printf("    %s %s, %sw\n", insn, r, r);
    else if (to->size == 4)
        printf("    mov %sd, %sd\n", r, r);
    else if (is_integer(from) && from->size < 8 && !from->is_unsigned)
        printf("    movsx %s, %sd\n", r, r);
}

static void divmod(Node *node, char *rs, char *rd, char *r64, char *r32) {
    if (node->ty->size == 8) {
        printf("    mov rax, %s\n", rd);
        if (node->ty->is_unsigned) {
            printf("    mov rdx, 0\n");
            printf("    div %s\n", rs);
        } else {
            printf("    cqo\n");
            printf("    idiv %s\n", rs);
        }
        printf("    mov %s, %s\n", rd, r64);
    } else {
        printf("    mov eax, %s\n", rd);
        if (node->ty->is_unsigned) {
            printf("    mov edx, 0\n");
            printf("    div %s\n", rs);
        } else {
            printf("    cdq\n");
            printf("    idiv %s\n", rs);
        }
        printf("    mov %s, %s\n", rd, r32);
    }
}

static void builtin_va_start(Node *node) {
    int gp = 0;
    int fp = 0;
    for (Var *var = current_fn->params; var; var = var->next) {
        if (is_flonum(var->ty))
            fp++;
        else
            gp++;
    }

    char *rd = reg(top);
    printf("    mov rax, [rbp-%d]\n", node->args[0]->offset);
    printf("    mov %s, %d\n", rd, gp*8);
    printf("    mov [rax], %s\n", rd);
    printf("    mov %s, %d\n", rd, 48 + fp * 8);
    printf("    mov [rax+4], %s\n", rd);
    printf("    mov [rax+16], rbp\n");
    printf("    subq [rax+16], 128\n");
    top++;
}

static void gen_expr(Node *node) {
    printf(".loc %d %d\n", node->tok->file_no, node->tok->line_no);
    switch (node->kind) {
        case ND_NUM:
            if (node->ty->kind == TY_FLOAT) {
                union { float x; int y; } val;
                val.x = node->fval;
                printf("    mov eax, %u # float %f\n", val.y, val.x);
                printf("    movd %s, eax\n", freg(top++));
            }
            else if (node->ty->kind == TY_DOUBLE) {
                union { double x; long y; } val;
                val.x = node->fval;
                printf("    movabs rax, %lu # double %f\n", val.y, val.x);
                printf("    movq %s, rax\n", freg(top++));
            }
            else if (node->ty->kind == TY_LONG)
                printf("    movabs %s, %lu\n", reg(top++), node->val);
            else
                printf("    mov %s, %lu\n", reg(top++), node->val);
            return;
        case ND_VAR:
        case ND_MEMBER:
            gen_addr(node);
            load(node->ty);
            return;
        case ND_DEREF:
            gen_expr(node->lhs);
            load(node->ty);
            return;
        case ND_ADDR:
            gen_addr(node->lhs);
            return;
        case ND_ASSIGN:
            if (node->ty->kind == TY_ARRAY)
                error_tok(node->tok, "not an lvalue");
            if (node->lhs->ty->is_const && !node->is_init)
                error_tok(node->tok, "cannot assign to a const variable");

            gen_expr(node->rhs);
            gen_addr(node->lhs);
            store(node->ty);
            return;
        case ND_STMT_EXPR:
            for (Node *n = node->body; n; n = n->next)
                gen_stmt(n);
            top++;
            return;
        case ND_NULL_EXPR:
            top++;
            return;
        case ND_COMMA:
            gen_expr(node->lhs);
            top--;
            gen_expr(node->rhs);
            return;
        case ND_CAST:
            gen_expr(node->lhs);
            cast(node->lhs->ty, node->ty);
            return;
        case ND_COND: {
            int seq = labelseq++;
            gen_expr(node->cond);
            cmp_zero(node->cond->ty);
            printf("    je .L.else.%d\n", seq);
            gen_expr(node->then);
            top--;
            printf("    jmp .L.end.%d\n", seq);
            printf(".L.else.%d:\n", seq);
            gen_expr(node->els);
            printf(".L.end.%d:\n", seq);
            return;
        }
        case ND_NOT:
            gen_expr(node->lhs);
            cmp_zero(node->lhs->ty);
            printf("    sete %sb\n", reg(top));
            printf("    movzx %s, %sb\n", reg(top), reg(top));
            top++;
            return;
        case ND_BITNOT:
            gen_expr(node->lhs);
            printf("    not %s\n", reg(top-1));
            return;
        case ND_LOGAND: {
            int seq = labelseq++;
            gen_expr(node->lhs);
            cmp_zero(node->lhs->ty);
            printf("    je .L.false.%d\n", seq);
            gen_expr(node->rhs);
            cmp_zero(node->lhs->ty);
            printf("    je .L.false.%d\n", seq);
            printf("    mov %s, 1\n", reg(top));
            printf("    jmp .L.end.%d\n", seq);
            printf(".L.false.%d:\n", seq);
            printf("    mov %s, 0\n", reg(top++));
            printf(".L.end.%d:\n", seq);
            return;
        }
        case ND_LOGOR: {
            int seq = labelseq++;
            gen_expr(node->lhs);
            cmp_zero(node->lhs->ty);
            printf("    jne .L.true.%d\n", seq);
            gen_expr(node->rhs);
            cmp_zero(node->lhs->ty);
            printf("    jne .L.true.%d\n", seq);
            printf("    mov %s, 0\n", reg(top));
            printf("    jmp .L.end.%d\n", seq);
            printf(".L.true.%d:\n", seq);
            printf("    mov %s, 1\n", reg(top++));
            printf(".L.end.%d:\n", seq);
            return;
        }
        case ND_FUNCALL: {
            if (node->lhs->kind == ND_VAR && 
                    !strcmp(node->lhs->var->name, "__builtin_va_start")) {
                builtin_va_start(node);
                return;
            }
            // caller-saved registers
            printf("    sub rsp, 64\n");
            printf("    mov [rsp], r10\n");
            printf("    mov [rsp+8], r11\n");
            printf("    movsd [rsp+16], xmm8\n");
            printf("    movsd [rsp+24], xmm9\n");
            printf("    movsd [rsp+32], xmm10\n");
            printf("    movsd [rsp+40], xmm11\n");
            printf("    movsd [rsp+48], xmm12\n");
            printf("    movsd [rsp+56], xmm13\n");

            gen_expr(node->lhs);


            // スタックから引数をロードする
            int gp = 0, fp = 0;
            for (int i = 0; i < node->nargs; i++) {
                Var *arg = node->args[i];
                int sz = arg->ty->size;

                if (is_flonum(arg->ty)) {
                    if (arg->ty->kind == TY_FLOAT)
                        printf("    movss xmm%d, [rbp-%d]\n", fp++, arg->offset);
                    else
                        printf("    movsd xmm%d, [rbp-%d]\n", fp++, arg->offset);
                    continue;
                }

                if (sz == 1)
                    printf("    movsx %s, byte ptr [rbp-%d]\n", argreg64[gp++], arg->offset);
                else if (sz == 2)
                    printf("    movsx %s, word ptr [rbp-%d]\n", argreg64[gp++], arg->offset);
                else if (sz == 4)
                    printf("    mov %s, dword ptr [rbp-%d]\n", argreg32[gp++], arg->offset);
                else
                    printf("    mov %s, [rbp-%d]\n", argreg64[gp++], arg->offset);
            }

            printf("    mov rax, %d\n", fp);
            printf("    call %s\n", reg(--top));
            
            if (node->ty->kind == TY_BOOL)
                printf("    movzx rax, al\n");

            printf("    mov r10, [rsp]\n");
            printf("    mov r11, [rsp+8]\n");
            printf("    movsd xmm8, [rsp+16]\n");
            printf("    movsd xmm9, [rsp+24]\n");
            printf("    movsd xmm10, [rsp+32]\n");
            printf("    movsd xmm11, [rsp+40]\n");
            printf("    movsd xmm12, [rsp+48]\n");
            printf("    movsd xmm13, [rsp+56]\n");
            printf("    add rsp, 64\n");

            if (node->ty->kind == TY_FLOAT)
                printf("    movss %s, xmm0\n", freg(top++));
            else if (node->ty->kind == TY_DOUBLE)
                printf("    movsd %s, xmm0\n", freg(top++));
            else
                printf("    mov %s, rax\n", reg(top++));
            return;
        }
    }


    gen_expr(node->lhs);
    gen_expr(node->rhs);

    char *rd = xreg(node->lhs->ty, top - 2);
    char *rs = xreg(node->lhs->ty, top - 1);
    char *fd = freg(top - 2);
    char *fs = freg(top - 1);
    top--;

    switch (node->kind) {
        case ND_ADD:
            if (node->ty->kind == TY_FLOAT)
                printf("    addss %s, %s\n", fd, fs);
            else if (node->ty->kind == TY_DOUBLE)
                printf("    addsd %s, %s\n", fd, fs);
            else
                printf("    add %s, %s\n", rd, rs);
            return;
        case ND_SUB:
            if (node->ty->kind == TY_FLOAT)
                printf("    subss %s, %s\n", fd, fs);
            else if (node->ty->kind == TY_DOUBLE)
                printf("    subsd %s, %s\n", fd, fs);
            else
                printf("    sub %s, %s\n", rd, rs);
            return;
        case ND_MUL:
            if (node->ty->kind == TY_FLOAT)
                printf("    mulss %s, %s\n", fd, fs);
            else if (node->ty->kind == TY_DOUBLE)
                printf("    mulsd %s, %s\n", fd, fs);
            else
                printf("    imul %s, %s\n", rd, rs);
            return;
        case ND_DIV:
            if (node->ty->kind == TY_FLOAT)
                printf("    divss %s, %s\n", fd, fs);
            else if (node->ty->kind == TY_DOUBLE)
                printf("    divsd %s, %s\n", fd, fs);
            else
                divmod(node, rs, rd, "rax", "eax");
            return;
        case ND_MOD:
            divmod(node, rs, rd, "rdx", "edx");
            return;
        case ND_BITAND:
            printf("    and %s, %s\n", rd, rs);
            return;
        case ND_BITOR:
            printf("    or %s, %s\n", rd, rs);
            return;
        case ND_BITXOR:
            printf("    xor %s, %s\n", rd, rs);
            return;
        case ND_EQ:
            if (node->lhs->ty->kind == TY_FLOAT)
                printf("    ucomiss %s, %s\n", fd, fs);
            else if (node->lhs->ty->kind == TY_DOUBLE)
                printf("    ucomisd %s, %s\n", fd, fs);
            else
                printf("    cmp %s, %s\n", rd, rs);
            printf("    sete al\n");
            printf("    movzb %s, al\n", rd);
            return;
        case ND_NE:
            if (node->lhs->ty->kind == TY_FLOAT)
                printf("    ucomiss %s, %s\n", fd, fs);
            else if (node->lhs->ty->kind == TY_DOUBLE)
                printf("    ucomisd %s, %s\n", fd, fs);
            else
                printf("    cmp %s, %s\n", rd, rs);
            printf("    setne al\n");
            printf("    movzb %s, al\n", rd);
            return;
        case ND_LT:
            if (node->lhs->ty->kind == TY_FLOAT) {
                printf("    ucomiss %s, %s\n", fd, fs);
                printf("    setb al\n");
            }
            else if (node->lhs->ty->kind == TY_DOUBLE) {
                printf("    ucomisd %s, %s\n", fd, fs);
                printf("    setb al\n");
            } else {
                printf("    cmp %s, %s\n", rd, rs);
                if (node->lhs->ty->is_unsigned)
                    printf("    setb al\n");
                else
                    printf("    setl al\n");
            }
            printf("    movzx %s, al\n", rd);
            return;
        case ND_LE:
            if (node->lhs->ty->kind == TY_FLOAT) {
                printf("    ucomiss %s, %s\n", fd, fs);
                printf("    setbe al\n");
            }
            else if (node->lhs->ty->kind == TY_DOUBLE) {
                printf("    ucomisd %s, %s\n", fd, fs);
                printf("    setbe al\n");
            } else {
                printf("    cmp %s, %s\n", rd, rs);
                if (node->lhs->ty->is_unsigned)
                    printf("    setbe al\n");
                else
                    printf("    setle al\n");
            }
            printf("    movzx %s, al\n", rd);
            return;
        case ND_SHL:
            printf("    mov rcx, %s\n", reg(top));
            printf("    sal %s, cl\n", rd);
            return;
        case ND_SHR:
            printf("    mov rcx, %s\n", reg(top));
            if (node->lhs->ty->is_unsigned)
                printf("    shr %s, cl\n", rd);
            else
                printf("    sar %s, cl\n", rd);
            return;
        default:
            error_tok(node->tok, "invalid expression");
    }
}

static void gen_stmt(Node *node) {
    printf(".loc %d %d\n", node->tok->file_no, node->tok->line_no);
    switch (node->kind) {
        case ND_IF: {
            int seq =  labelseq++;
            if (node->els) {
                gen_expr(node->cond);
                cmp_zero(node->cond->ty);
                printf("    je .L.else.%d\n", seq);
                gen_stmt(node->then);
                printf("    jmp .L.end.%d\n", seq);
                printf(".L.else.%d:\n", seq);
                gen_stmt(node->els);
                printf(".L.end.%d:\n", seq);
            } else {
                gen_expr(node->cond);
                cmp_zero(node->cond->ty);
                printf("    je .L.end.%d\n", seq);
                gen_stmt(node->then);
                printf(".L.end.%d:\n", seq);
            }
            return;
        }
        case ND_FOR: {
            int seq = labelseq++;
            int brk = brknum;
            int cont = contnum;
            brknum = contnum = seq;
            if (node->init)
                gen_stmt(node->init);
            printf(".L.begin.%d:\n", seq);
            if (node->cond) {
                gen_expr(node->cond);
                cmp_zero(node->cond->ty);
                printf("    je .L.break.%d\n", seq);
            }
            gen_stmt(node->then);
            printf(".L.continue.%d:\n", seq);
            if (node->inc) {
                gen_expr(node->inc);
                top--;
            }
            printf("    jmp .L.begin.%d\n", seq);
            printf(".L.break.%d:\n", seq);
            brknum = brk;
            contnum = cont;
            return;
        }
        case ND_DO: {
            int seq = labelseq++;
            int brk = brknum;
            int cont = contnum;
            brknum = contnum = seq;

            printf(".L.begin.%d:\n", seq);
            gen_stmt(node->then);
            printf(".L.continue.%d:\n", seq);
            gen_expr(node->cond);
            cmp_zero(node->cond->ty);
            printf("    jne .L.begin.%d\n", seq);
            printf(".L.break.%d:\n", seq);

            brknum = brk;
            contnum = cont;
            return;
        }
        case ND_SWITCH: {
            int seq = labelseq++;
            int brk = brknum;
            brknum = node->case_label = seq;

            gen_expr(node->cond);

            for (Node *n = node->case_next; n; n = n->case_next) {
                n->case_label = labelseq++;
                n->case_end_label = seq;
                printf("    cmp %s, %ld\n", reg(top-1), n->val);
                printf("    je .L.case.%d\n", n->case_label);
            }
            top--;

            if (node->default_case) {
                int i = labelseq++;
                node->default_case->case_end_label = seq;
                node->default_case->case_label = i;
                printf("    jmp .L.case.%d\n", i);
            }

            printf("    jmp .L.break.%d\n", seq);
            gen_stmt(node->then);
            printf(".L.break.%d:\n", seq);

            brknum = brk;
            return;
        }
        case ND_CASE:
            printf(".L.case.%d:\n", node->case_label);
            gen_stmt(node->lhs);
            return;
        case ND_BLOCK:
            for (Node *n = node->body; n; n = n->next)
                gen_stmt(n);
            return;
        case ND_BREAK:
            if (brknum == 0)
                error_tok(node->tok, "stray break");
            printf("    jmp .L.break.%d\n", brknum);
            return;
        case ND_CONTINUE:
            if (contnum == 0)
                error_tok(node->tok, "stray contnum");
            printf("    jmp .L.continue.%d\n", contnum);
            return;
        case ND_GOTO:
            printf("    jmp .L.label.%s.%s\n", current_fn->name, node->label_name);
            return;
        case ND_LABEL:
            printf(".L.label.%s.%s:\n", current_fn->name, node->label_name);
            gen_stmt(node->lhs);
            return;
        case ND_RETURN:
            if (node->lhs) {
                gen_expr(node->lhs);
                if (is_flonum(node->lhs->ty))
                    printf("    movsd xmm0, %s\n", freg(--top));
                else
                    printf("    mov rax, %s\n", reg(--top));
            }
            printf("    jmp .L.return.%s\n", current_fn->name);
            return;
        case ND_EXPR_STMT:
            gen_expr(node->lhs);
            top--;
            return;
        default:
            error_tok(node->tok, "invalid statement");
    }
}

static void emit_bss(Program *prog) {
    printf(".bss\n");

    for (Var *var = prog->globals; var; var = var->next) {
        if (var->init_data)
            continue;
        
        printf("    .align %d\n", var->align);
        if (!var->is_static)
            printf("    .globl %s\n", var->name);
        printf("    %s:\n", var->name);
        printf("    .zero %d\n", var->ty->size);
    }
}

static void emit_data(Program *prog) {
    printf(".data\n");

    for (Var *var = prog->globals; var; var = var->next) {
        if (!var->init_data)
            continue;

        printf("    .align %d\n", var->align);
        if (!var->is_static)
            printf("    .globl %s\n", var->name);
        printf("%s:\n", var->name);

        Relocation *rel = var->rel;
        int pos = 0;
        while (pos < var->ty->size) {
            if (rel && rel->offset == pos) {
                printf("    .quad %s+%ld\n", rel->label, rel->addend);
                rel = rel->next;
                pos += 8;
            } else {
                printf("    .byte %d\n", var->init_data[pos++]);
            }
        }
    }
}

static void emit_text(Program *prog) {
    printf(".text\n");

    for (Function *fn = prog->fns; fn; fn = fn->next) {
        if (!fn->is_static)
            printf(".globl %s\n", fn->name);
        printf("%s:\n", fn->name);
        current_fn = fn;

        // プロローグ
        // callee-saved
        // 呼び出し先がレジスタを保存する　
        printf("    push rbp\n");
        printf("    mov rbp, rsp\n");
        printf("    sub rsp, %d\n", fn->stack_size);
        printf("    mov [rbp-8], r12\n");
        printf("    mov [rbp-16], r13\n");
        printf("    mov [rbp-24], r14\n");
        printf("    mov [rbp-32], r15\n");

        if (fn->is_variadic) {
            printf("    mov [rbp-128], rdi\n");
            printf("    mov [rbp-120], rsi\n");
            printf("    mov [rbp-112], rdx\n");
            printf("    mov [rbp-104], rcx\n");
            printf("    mov [rbp-96], r8\n");
            printf("    mov [rbp-88], r9\n");
            printf("    movsd [rbp-80], xmm0\n");
            printf("    movsd [rbp-72], xmm1\n");
            printf("    movsd [rbp-64], xmm2\n");
            printf("    movsd [rbp-56], xmm3\n");
            printf("    movsd [rbp-48], xmm4\n");
            printf("    movsd [rbp-40], xmm5\n");
        }

        // スタックに引数を保存する
        int gp = 0, fp = 0;
        for (Var *var = fn->params; var; var = var->next) {
            if (is_flonum(var->ty))
                fp++;
            else
                gp++;
        }

        for (Var *var = fn->params; var; var = var->next) {
            if (var->ty->kind == TY_FLOAT)
                printf("    movss [rbp-%d], xmm%d\n", var->offset, --fp);
            else if (var->ty->kind == TY_DOUBLE)
                printf("    movsd [rbp-%d], xmm%d\n", var->offset, --fp);
            else {
                if (var->ty->size == 1)
                    printf("    mov [rbp-%d], %s\n", var->offset, argreg8[--gp]);
                else if (var->ty->size == 2)
                    printf("    mov [rbp-%d], %s\n", var->offset, argreg16[--gp]);
                else if (var->ty->size == 4)
                    printf("    mov [rbp-%d], %s\n", var->offset, argreg32[--gp]);
                else
                    printf("    mov [rbp-%d], %s\n", var->offset, argreg64[--gp]);
            }
        }

        // アセンブリのコードを生成する
        for (Node *n = fn->node; n; n = n->next) {
            gen_stmt(n);
            assert(top == 0);
        }

        // 退避させたレジスタの値を元に戻す
        printf(".L.return.%s:\n", fn->name);
        printf("    mov r12, [rbp-8]\n");
        printf("    mov r13, [rbp-16]\n");
        printf("    mov r14, [rbp-24]\n");
        printf("    mov r15, [rbp-32]\n");
        printf("    mov rsp, rbp\n");
        printf("    pop rbp\n");
        printf("    ret\n");
    }
}

void codegen(Program *prog) {
    char **paths = get_input_files();
    for (int i = 0; paths[i]; i++)
        printf("   .file %d \"%s\"\n", i+1, paths[i]);

    printf(".intel_syntax noprefix\n");
    emit_bss(prog);
    emit_data(prog);
    emit_text(prog);

}
