#include "kcc.h"

Type *ty_void = &(Type){TY_VOID, 1, 1};

Type *ty_char = &(Type){TY_CHAR, 1, 1};
Type *ty_short = &(Type){TY_SHORT, 2, 2};
Type *ty_int = &(Type){TY_INT, 4, 4};
Type *ty_long = &(Type){TY_LONG, 8, 8};

static Type *new_type(TypeKind kind, int size, int align) {
    Type *ty = calloc(1, sizeof(Type));
    ty->kind = kind;
    ty->size = size;
    ty->align = align;
    return ty;
}

bool is_integer(Type *ty) {
    return ty->kind == TY_CHAR  || ty->kind == TY_INT ||
           ty->kind == TY_SHORT || ty->kind == TY_LONG;
}

static bool is_scalar(Type *ty) {
    return is_integer(ty) || ty->base;
}

Type *copy_type(Type *ty) {
    Type *ret = calloc(1, sizeof(Type));
    *ret = *ty;
    return ret;
}

int align_to(int n, int align) {
    return (n + align - 1) & ~(align - 1);
}

// 何型に対してのポインタかを設定する
Type *pointer_to(Type *base) {
    Type *ty = new_type(TY_PTR, 8, 8);
    ty->base = base;
    return ty;
}

Type *array_of(Type *base, int len) {
    Type *ty = new_type(TY_ARRAY, base->size * len, base->align);
    ty->base = base;
    ty->array_len = len;
    return ty;
}

static Type *get_common_type(Type *ty1, Type *ty2) {
    if (ty1->base)
        return pointer_to(ty1->base);
    if (ty1->size == 8 || ty2->size == 8)
        return ty_long;
    return ty_int;
}

static void usual_arith_conv(Node **lhs, Node **rhs) {
    Type *ty = get_common_type((*lhs)->ty, (*rhs)->ty);
    *lhs = new_cast(*lhs, ty);
    *rhs = new_cast(*rhs, ty);
}

Type *func_type(Type *return_ty) {
    Type *ty = calloc(1, sizeof(Type));
    ty->kind = TY_FUNC;
    ty->return_ty = return_ty;
    return ty;
}

void add_type(Node *node) {
    if (!node || node->ty)
        return;

    add_type(node->lhs);
    add_type(node->rhs);
    add_type(node->cond);
    add_type(node->then);
    add_type(node->els);
    add_type(node->init);
    add_type(node->inc);

    for (Node *n = node->body; n; n = n->next)
        add_type(n);

    switch (node->kind) {
        case ND_NUM:
            node->ty = (node->val == (int)node->val) ? ty_int : ty_long;
            return;
        case ND_ADD:
        case ND_SUB:
        case ND_MUL:
        case ND_DIV:
            usual_arith_conv(&node->lhs, &node->rhs);
            node->ty = node->lhs->ty;
            return;
        case ND_ASSIGN:
            if (is_scalar(node->rhs->ty))
                node->rhs = new_cast(node->rhs, node->lhs->ty);
            node->ty = node->lhs->ty;
            return;
        case ND_EQ:
        case ND_NE:
        case ND_LT:
        case ND_LE:
            usual_arith_conv(&node->lhs, &node->rhs);
            node->ty = ty_int;
            return;
        case ND_VAR:
            node->ty = node->var->ty;
            return;
        case ND_COMMA:
            node->ty = node->rhs->ty;
            return;
        case ND_MEMBER:
            node->ty = node->member->ty;
            return;
        case ND_ADDR:
            if (node->lhs->ty->kind == TY_ARRAY)
                node->ty = pointer_to(node->lhs->ty->base);
            else
                node->ty = pointer_to(node->lhs->ty);
            return;
        case ND_DEREF:
            if (!node->lhs->ty->kind)
                error_tok(node->tok, "invalid pointer dereference");
            if (node->lhs->ty->base->kind == TY_VOID)
                error_tok(node->tok, "dereference a void pointer");
            node->ty = node->lhs->ty->base;
            return;
        case ND_STMT_EXPR: {
            Node *stmt = node->body;
            while (stmt->next)
                stmt = stmt->next;
            node->ty = stmt->lhs->ty;
            return;
        }
    }
}
