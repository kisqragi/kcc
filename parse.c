#include "kcc.h"

// ローカル変数,グローバル変数,typedefのスコープを表す
typedef struct VarScope VarScope;
struct VarScope {
    VarScope *next;
    char *name;
    int depth;
    Var *var;
    Type *type_def;
    Type *enum_ty;
    int enum_val;
};

// 構造体 or 共用体タグ or 列挙型 
typedef struct TagScope TagScope;
struct TagScope {
    TagScope *next;
    char *name;
    int depth;
    Type *ty;
};

// typedefやexternなどの変数属性
typedef struct {
    bool is_typedef;
    bool is_static;
} VarAttr;

// ローカル変数のリスト
static Var *locals;
static Var *globals;

static VarScope *var_scope;
static TagScope *tag_scope;

// scope_depthは"{"が来るたびに一つインクリメントされます。
// また、"}"が来た場合デクリメントされます。
static int scope_depth;

// 現在parse中の関数を示す
static Var *current_fn;

static bool is_typename(Token *tok);
static Type *declarator(Token **rest, Token *tok, Type *ty);
static Type *typespec(Token **rest, Token *tok, VarAttr *attr);
static Type *enum_specifier(Token **rest, Token *tok);
static Node *compound_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *new_add(Node *lhs, Node *rhs, Token *tok);
static Node *new_sub(Node *lhs, Node *rhs, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *cast(Token **rest, Token *tok);
static Type *struct_decl(Token **rest, Token *tok);
static Type *union_decl(Token **rest, Token *tok);
static Node *postfix(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

static void enter_scope(void) {
    scope_depth++;
}

static void leave_scope(void) {
    scope_depth--;
    while (var_scope && var_scope->depth > scope_depth)
        var_scope = var_scope->next;

    while (tag_scope && tag_scope->depth > scope_depth)
        tag_scope = tag_scope->next;
}

// 名前でローカル変数を検索する
static VarScope *find_var(Token *tok) {
    for (VarScope *sc = var_scope; sc; sc = sc->next)
        if (strlen(sc->name) == tok->len && !strncmp(tok->loc, sc->name, tok->len))
            return sc;
    return NULL;
}

static TagScope *find_tag(Token *tok) {
    for (TagScope *sc = tag_scope; sc; sc = sc->next)
        if (strlen(sc->name) == tok->len && !strncmp(tok->loc, sc->name, tok->len))
            return sc;
    return NULL;
}


static Node *new_node(NodeKind kind, Token *tok) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    node->tok = tok;
    return node;
}

// 二項演算
static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs, Token *tok) {
    Node *node = new_node(kind, tok);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

static Node *new_unary(NodeKind kind, Node *expr, Token *tok) {
    Node *node = new_node(kind, tok);
    node->lhs = expr;
    return node;
}

static Node *new_num(long val, Token *tok) {
    Node *node = new_node(ND_NUM, tok);
    node->val = val;
    return node;
}

static Node *new_var_node(Var *var, Token *tok) {
    Node *node = new_node(ND_VAR, tok);
    node->var = var;
    return node;
}

Node *new_cast(Node *expr, Type *ty) {
    add_type(expr);

    Node *node = calloc(1, sizeof(Node));
    node->kind = ND_CAST;
    node->tok = expr->tok;
    node->lhs = expr;
    node->ty = copy_type(ty);
    return node;
}

static VarScope *push_scope(char *name) {
    VarScope *sc = calloc(1, sizeof(VarScope));
    sc->next = var_scope;
    sc->name = name;
    sc->depth = scope_depth;
    var_scope = sc;
    return sc;
}

static Var *new_var(char *name, Type *ty) {
    Var *var = calloc(1, sizeof(Var));
    var->name = name;
    var->ty = ty;
    push_scope(name)->var = var;
    return var;
}

static Var *new_lvar(char *name, Type *ty) {
    Var *var = new_var(name, ty);
    var->is_local = true;
    var->next = locals;
    locals = var;
    return var;
}

static Var *new_gvar(char *name, Type *ty, bool is_definition) {
    Var *var = new_var(name, ty);
    var->is_local = false;
    if (is_definition) {
        var->next = globals;
        globals = var;
    }
    return var;
}

static char *new_gvar_name(void) {
    static int cnt = 0;
    char *buf = calloc(20, sizeof(char));
    sprintf(buf, ".L.data.%d", cnt++);
    return buf;
}

static Var *new_string_literal(char *p, int len) {
    Type *ty = array_of(ty_char, len);
    Var *var = new_gvar(new_gvar_name(), ty, true);
    var->init_data = p;
    return var;
}

static char *get_ident(Token *tok) {
    if (tok->kind != TK_IDENT)
        error_tok(tok, "expected an identifier");
    return strndup(tok->loc, tok->len);
}

static Type *find_typedef(Token *tok) {
    if (tok->kind == TK_IDENT) {
        VarScope *sc = find_var(tok);
        if (sc)
            return sc->type_def;
    }
    return NULL;
}

// トークンが数値の場合、値を返す
static long get_number(Token *tok) {
    if (tok->kind != TK_NUM)
        error_tok(tok, "expected a number");
    return tok->val;
}

static void push_tag_scope(Token *tok, Type *ty) {
    TagScope *sc = calloc(1, sizeof(TagScope));
    sc->next = tag_scope;
    sc->name = strndup(tok->loc, tok->len);
    sc->depth = scope_depth;
    sc->ty = ty;
    tag_scope = sc;
}

// funcdef = typespec declarator compound-stmt
static Function *funcdef(Token **rest, Token *tok) {
    locals = NULL;

    VarAttr attr = {};
    Type *ty = typespec(&tok, tok, &attr);
    ty = declarator(&tok, tok, ty);

    Function *fn = calloc(1, sizeof(Function));
    fn->name = get_ident(ty->name);
    fn->is_static = attr.is_static;

    enter_scope();

    for (Type *t = ty->params; t; t = t->next)
        new_lvar(get_ident(t->name), t);
    fn->params = locals;

    tok = skip(tok, "{");
    fn->node = compound_stmt(rest, tok)->body;
    fn->locals = locals;

    leave_scope();

    return fn;
}

// typespec = typename typename*
// typename = "void" | "_Bool" | "char" | "short" | "int" | "long"
//          | "struct" struct-decl | "union" union-struct | typedef-name
// typespec = type-specifier = 型指定子
static Type *typespec(Token **rest, Token *tok, VarAttr *attr) {
    enum {
        VOID  = 1 << 0,
        BOOL  = 1 << 2,
        CHAR  = 1 << 4,
        SHORT = 1 << 6,
        INT   = 1 << 8,
        LONG  = 1 << 10,
        OTHER = 1 << 12,

    };

    Type *ty = ty_int;
    int counter = 0;

    while (is_typename(tok)) {
        if (equal(tok, "typedef") || equal(tok, "static")) {
            if (!attr)
                error_tok(tok, "storage class specifier is not allowed in this context");

            if (equal(tok, "typedef"))
                attr->is_typedef = true;
            else
                attr->is_static = true;

            if (attr->is_typedef + attr->is_static > 1)
                error_tok(tok, "typedef and static may not be used together"); 

            tok = tok->next;
            continue;
        }

        Type *ty2 = find_typedef(tok);
        if (equal(tok, "struct") || equal(tok, "union") ||
            equal(tok, "enum")|| ty2) {
            if (counter)
                break;
            if (equal(tok, "struct"))
                ty = struct_decl(&tok, tok->next);
            else if (equal(tok, "union"))
                ty = union_decl(&tok, tok->next);
            else if (equal(tok, "enum"))
                ty = enum_specifier(&tok, tok->next);
            else {
                ty = ty2;
                tok = tok->next;
            }
            counter += OTHER;
            continue;
        }

        if (equal(tok, "void"))
            counter += VOID;
        else if (equal(tok, "_Bool"))
            counter += BOOL;
        else if (equal(tok, "char"))
            counter += CHAR;
        else if (equal(tok, "short"))
            counter += SHORT;
        else if (equal(tok, "int"))
            counter += INT;
        else if (equal(tok, "long"))
            counter += LONG;
        else
            error_tok(tok, "internal error");

        switch (counter) {
            case VOID:
                ty = ty_void;
                break;
            case BOOL:
                ty = ty_bool;
                break;
            case CHAR:
                ty = ty_char;
                break;
            case SHORT:
            case SHORT + INT:
                ty = ty_short;
                break;
            case INT:
                ty = ty_int;
                break;
            case LONG:
            case LONG + INT:
            case LONG + LONG:
            case LONG + LONG + INT:
                ty = ty_long;
                break;
            default:
                error_tok(tok, "invalid type");
        }

        tok = tok->next;
    }

    *rest = tok;
    return ty;
}

// func-params = (param ("," param)*)? ")"
// param       = typespec declarator
static Type *func_params(Token **rest, Token *tok, Type *ty) {
    Type head = {};
    Type *cur = &head;

    while (!equal(tok, ")")) {
        if (cur != &head)
            tok = skip(tok, ",");
        Type *basety = typespec(&tok, tok, NULL);
        Type *ty = declarator(&tok, tok, basety);
        cur = cur->next = copy_type(ty);
    }

    ty = func_type(ty);
    ty->params = head.next;
    *rest = tok->next;
    return ty;
}

// type-suffix = "(" func-params
//             = "[" num "]" type-suffix
//             = ε
static Type *type_suffix(Token **rest, Token *tok, Type *ty) {
    if (equal(tok, "("))
        return func_params(rest, tok->next, ty);

    if (equal(tok, "[")) {
        int sz = get_number(tok->next);
        tok = skip(tok->next->next, "]");
        ty = type_suffix(rest, tok, ty);
        return array_of(ty, sz);
    }

    *rest = tok;
    return ty;
}

// declarator = "*"* ("(" declarator ")" | ident) type-suffix
static Type *declarator(Token **rest, Token *tok, Type *ty) {
    while (consume(&tok, tok, "*"))
        ty = pointer_to(ty);

    if (equal(tok, "(")) {
        Type *placeholder = calloc(1, sizeof(Type));
        Type *new_ty = declarator(&tok, tok->next, placeholder);
        tok = skip(tok, ")");
        *placeholder = *type_suffix(rest, tok, ty);
        return new_ty;
    }

    if (tok->kind != TK_IDENT)
        error_tok(tok, "expected a variable name");

    ty = type_suffix(rest, tok->next, ty);
    ty->name = tok;
    return ty;
}

// abstract-declarator = "*"* ("(" abstract-declarator ")")? type-suffix
static Type *abstract_declarator(Token **rest, Token *tok, Type *ty) {
    while (equal(tok, "*")) {
        ty = pointer_to(ty);
        tok = tok->next;
    }

    if (equal(tok, "(")) {
        Type *placeholder = calloc(1, sizeof(Type));
        Type *new_ty = abstract_declarator(&tok, tok->next, placeholder);
        tok = skip(tok, ")");
        *placeholder = *type_suffix(rest, tok, ty);
        return new_ty;
    }

    return type_suffix(rest, tok, ty);
}
 
// type-name = typespec abstract-declarator
static Type *typename(Token **rest, Token *tok) {
    Type *ty = typespec(&tok, tok, NULL);
    return abstract_declarator(rest, tok, ty);
}

// enum-specifier = ident? "{" enum-list? "}"
//                | ident ("{" enum-list? "}")?
// enum-list      = ident ("=" num)? ("," ident ("=" num)?)*
static Type *enum_specifier(Token **rest, Token *tok) {
    Type *ty = enum_type();

    Token *tag = NULL;
    if (tok->kind == TK_IDENT) {
        tag = tok;
        tok = tok->next;
    }

    // Read a struct tag.
    if (tag && !equal(tok, "{")) {
        TagScope *sc = find_tag(tag);
        if (!sc)
            error_tok(tag, "unknown enum type");
        if (sc->ty->kind != TY_ENUM)
            error_tok(tag, "not an enum tag");
        *rest = tok;
        return sc->ty;
    }

    tok = skip(tok, "{");

    // Read an enum-list.
    int i = 0;
    int val = 0;
    while (!equal(tok, "}")) {
        if (i++ > 0)
            tok = skip(tok, ",");

        char *name = get_ident(tok);
        tok = tok->next;

        if (equal(tok, "=")) {
            val = get_number(tok->next);
            tok = tok->next->next;
        }

        VarScope *sc = push_scope(name);
        sc->enum_ty = ty;
        sc->enum_val = val++;
    }

    *rest = tok->next;

    if (tag)
        push_tag_scope(tag, ty);
    return ty;

}


// declaration = typespec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
static Node *declaration(Token **rest, Token *tok) {
    VarAttr attr = {};
    Type *basety = typespec(&tok, tok, &attr);

    Node head = {};
    Node *cur = &head;
    int cnt = 0;

    while (!equal(tok, ";")) {
        if (cnt++ > 0)
            tok = skip(tok, ",");

        Type *ty = declarator(&tok, tok, basety);

        if (attr.is_typedef) {
            push_scope(get_ident(ty->name))->type_def = ty;
            continue;
        }
        if (ty->kind == TY_VOID)
            error_tok(tok, "variable declared void");
        Var *var = new_lvar(get_ident(ty->name), ty);

        if (!equal(tok, "="))
            continue;

        Node *lhs = new_var_node(var, ty->name);
        Node *rhs = assign(&tok, tok->next);
        Node *node = new_binary(ND_ASSIGN, lhs, rhs, tok);
        cur = cur->next = new_unary(ND_EXPR_STMT, node, tok);

    }

    Node *node = new_node(ND_BLOCK, tok);
    node->body = head.next;
    *rest = tok->next;
    return node;
}

//==================================================
// [生成規則]
//
// program           = (funcdef | global-var)*
// funcdef           = typespec declarator compound-stmt
// declarator        = "*"* ident type-suffix
// func-params       = (param ("," param)*)? ")"
// param             = typespec declarator
// type-suffix       = "(" func-params
//                   = "[" num "]" type-suffix
//                   | ε
// typespec          = typename typename*
// typename          = "void" | "_Bool" | "char" | "short" | "int" | "long"
//                   | "struct" struct-decl | "union" union-struct
//                   | typedef-name
// struct-union-decl = ident? ("{" struct-members)?
// struct-decl       = struct-union-decl
// union-decl        = struct-union-decl
// struct-members    = (typespec declarator ("," declarator)* ";")* "}"
// compound-stmt     = (declaration | stmt)* "}"
// declaration       = typespec (declarator ("=" expr)? ("," declarator ("=" expr)?)*)? ";"
// stmt              = "return"? expr ";"
//                   | "{" stmt* "}"
//                   | "if" "(" expr ")" stmt ("else" stmt)?
//                   | "for" "(" expr? ";" expr? ";" expr? ")" stmt
//                   | "while" "(" expr ")" stmt
//                   | "{" compound_stmt
// expr-stmt         = expr
// expr              = assign ("," expr)?
// assign            = equality (assign-op assign)?
// assign-op         = "=" | "+=" | "-=" | "*=" | "/="
// equality          = relational ("==" relational | "!=" relational)*
// relational        = add ("<" add | "<=" | ">" add | ">=" add)*
// add               = mul ("+" mul | "-" mul)*
// mul               = cast ("*" cast | "/" cast)*
// cast              = "(" type-name ")" cast | unary
// unary             = ("+" | "-" | "*" | "&")? unary
//                   | postfix 
// postfix           = primary ("[" epxr "]" | "." ident | "->" ident)*
// primary           = "(" "{" stmt stmt* "}" ")"
//                   | "(" expr ")"
//                   | "sizeof" "(" type-name ")"
//                   | "sizeof" unary
//                   | ident func-args?
//                   | str
// func-args         = "(" (assign ("," assign)*)? ")"
//
//==================================================

// stmt = "return"? expr ";"
//      | "{" stmt* "}"
//      | "if" "(" expr ")" stmt ("else" stmt)?
//      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
//      | "while" "(" expr ")" stmt
//      | "{" compound_stmt
static Node *stmt(Token **rest, Token *tok) {
    Node *node;

    if (equal(tok, "return")) {
        node = new_node(ND_RETURN, tok);
        Node *exp = expr(&tok, tok->next);
        *rest = skip(tok, ";");

        add_type(exp);
        node->lhs = new_cast(exp, current_fn->ty->return_ty);
        return node;
    }

    if (equal(tok, "if")) {
        node = new_node(ND_IF, tok);
        tok = skip(tok->next, "(");
        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");
        node->then = stmt(&tok, tok);
        if (equal(tok, "else"))
            node->els = stmt(&tok, tok->next);
        *rest = tok;
        return node;
    }

    if (equal(tok, "for")) {
        node = new_node(ND_FOR, tok);
        tok = skip(tok->next, "(");

        enter_scope();

        if (is_typename(tok)) {
            node->init = declaration(&tok, tok);
        } else {
            node->init = expr_stmt(&tok, tok);
            tok = skip(tok, ";");
        }

        // condは比較結果の値を返す
        if (!equal(tok, ";"))
            node->cond = expr(&tok, tok);
        tok = skip(tok, ";");

        if (!equal(tok, ")"))
            node->inc = expr_stmt(&tok, tok);

        tok = skip(tok, ")");

        node->then = stmt(rest, tok);
        leave_scope();
        return node;
    }

    if (equal(tok, "while")) {
        node = new_node(ND_FOR, tok);
        tok = skip(tok->next, "(");

        node->cond = expr(&tok, tok);
        tok = skip(tok, ")");

        node->then = stmt(rest, tok);
        return node;
    }

    if (equal(tok, "{")) {
        return compound_stmt(rest, tok->next);
    }

    node = new_unary(ND_EXPR_STMT, expr(&tok, tok), tok);
    *rest = skip(tok, ";");
    return node;
}

static bool is_typename(Token *tok) {
    static char *kw[] = {
        "void", "_Bool", "char", "short", "int", "long", "struct",
        "union", "typedef", "enum", "static"
    };

    for (int i = 0; i < sizeof(kw) / sizeof(*kw); i++)
        if (equal(tok, kw[i]))
            return true;
    return find_typedef(tok);
}

// compound-stmt = (declaration | stmt)* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
    Node *node = new_node(ND_BLOCK, tok);
    Node head = {};
    Node *cur = &head;

    enter_scope();

    while (!equal(tok, "}")) {
        if (is_typename(tok)) {
            cur = cur->next = declaration(&tok, tok);
        }
        else
            cur = cur->next = stmt(&tok, tok);
        add_type(cur);
    }

    leave_scope();

    node->body = head.next;
    *rest = tok->next;
    return node;
}

// expr-stmt = expr
static Node *expr_stmt(Token **rest, Token *tok) {
    Node *node = new_node(ND_EXPR_STMT, tok);
    node->lhs = expr(rest, tok);
    return node;
}

// expr = assign ("," expr)?
static Node *expr(Token **rest, Token *tok) {
    Node *node = assign(&tok, tok);

    if (equal(tok, ","))
        return new_binary(ND_COMMA, node, expr(rest, tok->next), tok);
    *rest = tok;
    return node;
}

// convert `A op= B` to `tmp = &A, *tmp = *tmp op B`
static Node *to_assign(Node *binary) {
    add_type(binary->lhs);
    add_type(binary->rhs);

    Var *var = new_lvar("", pointer_to(binary->lhs->ty));
    Token *tok = binary->tok;

    Node *expr1 = new_binary(ND_ASSIGN, new_var_node(var, tok),
            new_unary(ND_ADDR, binary->lhs, tok), tok);

    Node *expr2 = new_binary(
            ND_ASSIGN,
            new_unary(ND_DEREF, new_var_node(var, tok), tok),
            new_binary(
                binary->kind,
                new_unary(ND_DEREF, new_var_node(var, tok), tok),
                binary->rhs,
                tok
            ),
        tok);

    return new_binary(ND_COMMA, expr1, expr2, tok);
}

// assign   = equality (assign-op assign)?
// assign-op = "=" | "+=" | "-=" | "*=" | "/="
static Node *assign(Token **rest, Token *tok) {
    Node *node = equality(&tok, tok);
    if (equal(tok, "="))
        node = new_binary(ND_ASSIGN, node, assign(&tok, tok->next), tok);

    if (equal(tok, "+="))
        return to_assign(new_add(node, assign(rest, tok->next), tok));

    if (equal(tok, "-="))
        return to_assign(new_sub(node, assign(rest, tok->next), tok));

    if (equal(tok, "*="))
        return to_assign(new_binary(ND_MUL, node, assign(rest, tok->next), tok));

    if (equal(tok, "/="))
        return to_assign(new_binary(ND_DIV, node, assign(rest, tok->next), tok));

    *rest = tok;
    return node;
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
    Node *node = relational(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (equal(tok, "==")) {
            node = new_binary(ND_EQ, node, relational(&tok, tok->next), start);
            continue;
        }

        if (equal(tok, "!=")) {
            node = new_binary(ND_NE, node, relational(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

static Node *new_add(Node *lhs, Node *rhs, Token *tok) {
    add_type(lhs);
    add_type(rhs);

    // num + num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
        return new_binary(ND_ADD, lhs, rhs, tok);

    if (lhs->ty->base && rhs->ty->base)
        error_tok(tok, "invalid operands");

    // `num + ptr` を `ptr + num` へ整型する
    if (!lhs->ty->base && rhs->ty->base) {
        Node *tmp = lhs;
        lhs = rhs;
        rhs = tmp;
    }

    // ptr + num
    // 8バイト単位(今サポートしているint型の単位)
    rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
    return new_binary(ND_ADD, lhs, rhs, tok);
}

static Node *new_sub(Node *lhs, Node *rhs, Token *tok) {
    add_type(lhs);
    add_type(rhs);

    // num - num
    if (is_integer(lhs->ty) && is_integer(rhs->ty))
        return new_binary(ND_SUB, lhs, rhs, tok);

    // ptr - num
    if (lhs->ty->base && is_integer(rhs->ty)) {
        rhs = new_binary(ND_MUL, rhs, new_num(lhs->ty->base->size, tok), tok);
        return new_binary(ND_SUB, lhs, rhs, tok);
    }

    // ptr - ptr, ptr - ptr の結果をサイズで割った値が返る
    // それは要素数だが、結果は負の値にもなる
    if (lhs->ty->base && rhs->ty->base) {
        Node *node = new_binary(ND_SUB, lhs, rhs, tok);
        return new_binary(ND_DIV, node, new_num(lhs->ty->base->size, tok), tok);
    }

    error_tok(tok, "invalid operands");
}

// relational = add ("<" add | "<=" | ">" add | ">=" add)*
static Node *relational(Token **rest, Token *tok) {
    Node *node = add(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (equal(tok, "<")) {
            node = new_binary(ND_LT, node, add(&tok, tok->next), start);
            continue;
        }

        if (equal(tok, "<=")) {
            node = new_binary(ND_LE, node, add(&tok, tok->next), start);
            continue;
        }

        if (equal(tok, ">")) {
            node = new_binary(ND_LT, add(&tok, tok->next), node, start);
            continue;
        }

        if (equal(tok, ">=")) {
            node = new_binary(ND_LE, add(&tok, tok->next), node, start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// add = mul ("+" mul | "-" mul)*
static Node *add(Token **rest, Token *tok) {
    Node *node = mul(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (equal(tok, "+")) {
            node = new_add(node, mul(&tok, tok->next), start);
            continue;
        }

        if (equal(tok, "-")) {
            node = new_sub(node, mul(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// mul = cast ("*" cast | "/" cast)*
static Node *mul(Token **rest, Token *tok) {
    Node *node = cast(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (equal(tok, "*")) {
            node = new_binary(ND_MUL, node, cast(&tok, tok->next), start);
            continue;
        }

        if (equal(tok, "/")) {
            node = new_binary(ND_DIV, node, cast(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// cast = "(" type-name ")" cast | unary
static Node *cast(Token **rest, Token *tok) {
    if (equal(tok, "(") && is_typename(tok->next)) {
        Node *node = new_node(ND_CAST, tok);
        node->ty = typename(&tok, tok->next);
        tok = skip(tok, ")");
        node->lhs = cast(rest, tok);
        add_type(node->lhs);
        return node;
    }

    return unary(rest, tok);

}

// unary = ("+" | "-" | "*" | "&")? cast | postfix
static Node *unary(Token **rest, Token *tok) {
    if (equal(tok, "+"))
        return cast(rest, tok->next);

    if (equal(tok, "-"))
        return  new_binary(ND_SUB, new_num(0, tok), cast(rest, tok->next), tok);

    if (equal(tok, "&"))
        return new_unary(ND_ADDR, cast(rest, tok->next), tok);

    if (equal(tok, "*"))
        return new_unary(ND_DEREF, cast(rest, tok->next), tok);

    return postfix(rest, tok);
}

// struct-members = (typespec declarator ("," declarator)* ";")* "}"
static Member *struct_members(Token **rest, Token *tok) {
    Member head = {};
    Member *cur = &head;

    while (!equal(tok, "}")) {
        Type *basety = typespec(&tok, tok, NULL);
        int cnt = 0;

        while (!consume(&tok, tok, ";")) {
            if (cnt++)
                tok = skip(tok, ",");

            Member *mem = calloc(1, sizeof(Member));
            mem->ty = declarator(&tok, tok, basety);
            mem->name = mem->ty->name;
            cur = cur->next = mem;
        }
    }

    *rest = tok->next;
    return head.next;
}

// struct-union-decl = ident? ("{" struct-members)?
static Type *struct_union_decl(Token **rest, Token *tok) {
    // 構造体タグを読み取る
    Token *tag = NULL;
    if (tok->kind == TK_IDENT) {
        tag = tok;
        tok = tok->next;
    }

    if (tag && !equal(tok, "{")) {
        TagScope *sc = find_tag(tag);
        if (!sc)
            error_tok(tag, "unknown struct type");
        *rest = tok;
        return sc->ty;
    }

    // 構造体オブジェクトの作成
    Type *ty = calloc(1, sizeof(Type));
    ty->kind = TY_STRUCT;
    ty->members = struct_members(rest, tok->next);

    if (tag)
        push_tag_scope(tag, ty);

    return ty;
}

// struct-decl = struct-union-decl
static Type *struct_decl(Token **rest, Token *tok) {
    Type *ty = struct_union_decl(rest, tok);

    // 構造体内のオフセットをメンバに割り当てる
    int offset = 0;
    for (Member *mem = ty->members; mem; mem = mem->next) {
        offset = align_to(offset, mem->ty->align);
        mem->offset = offset;
        offset += mem->ty->size;

        if (ty->align < mem->ty->align)
            ty->align = mem->ty->align;
    }
    ty->size = align_to(offset, ty->align);
    return ty;
}

// union-decl = struct-union-decl
static Type *union_decl(Token **rest, Token *tok) {
    Type *ty = struct_union_decl(rest, tok);

    for (Member *mem = ty->members; mem; mem = mem->next) {
        if (ty->align < mem->ty->align)
            ty->align = mem->ty->align;
        if (ty->size < mem->ty->size)
            ty->size = mem->ty->size;
    }
    ty->size = align_to(ty->size, ty->align);
    return ty;
}

static Member *get_struct_member(Type *ty, Token *tok) {
    for (Member *mem = ty->members; mem; mem = mem->next)
        if (mem->name->len == tok->len &&
            !strncmp(mem->name->loc, tok->loc, tok->len))
            return mem;
    error_tok(tok, "no such member");
}

static Node *struct_ref(Node *lhs, Token *tok) {
    add_type(lhs);
    if (lhs->ty->kind != TY_STRUCT)
        error_tok(lhs->tok, "not a struct");

    Node *node = new_unary(ND_MEMBER, lhs, tok);
    node->member = get_struct_member(lhs->ty, tok);
    return node;
}

// postfix = primary ("[" epxr "]" | "." ident | "->" ident)*
static Node *postfix(Token **rest, Token *tok) {
    Node *node = primary(&tok, tok);

    while (true) {
        if (equal(tok, "[")) {
            // x[y] == *(x+y)
            Token *start = tok;
            Node *idx = expr(&tok, tok->next);
            tok = skip(tok, "]");
            node = new_unary(ND_DEREF, new_add(node, idx, start), start);
            continue;
        }

        if (equal(tok, ".")) {
            node = struct_ref(node, tok->next);
            tok = tok->next->next;
            continue;
        }

        if (equal(tok, "->")) {
            // x->yは(*x).yの省略形です
            node = new_unary(ND_DEREF, node, tok);
            node = struct_ref(node, tok->next);
            tok = tok->next->next;
            continue;
        }

        *rest = tok;
        return node;
    }
}

// func-args = "(" (assign ("," assign)*)? ")"
//
// foo(a,b,c) は ({ t1=a; t2=b; t3=c; foo(t1,t2,t3); }) とコンパイルされます。
// t1,t2,t3は新しいローカル変数です。
static Node *funcall(Token **rest, Token *tok) {

    Token *start = tok;
    tok = tok->next->next;

    VarScope *sc = find_var(start);
    Type *ty;
    if (sc) {
        if (!sc->var || sc->var->ty->kind != TY_FUNC)
            error_tok(start, "not a function");
        ty = sc->var->ty;
    } else {
        warn_tok(start, "implicit declaration of a function");
        ty = func_type(ty_int);
    }

    Node *node = new_node(ND_NULL_EXPR, tok);
    Var **args = NULL;
    int nargs = 0;
    Type *param_ty = ty->params;

    while (!equal(tok, ")")) {
        if (nargs)
            tok = skip(tok, ",");
        Node *arg = assign(&tok, tok);
        add_type(arg);

        if (param_ty) {
            arg = new_cast(arg, param_ty);
            param_ty = param_ty->next;
        }

        Var *var = arg->ty->base
            ? new_lvar("", pointer_to(arg->ty->base))
            : new_lvar("", arg->ty);

        args = realloc(args, sizeof(*args) * (nargs + 1));
        args[nargs] = var;
        nargs++;

        Node *expr = new_binary(ND_ASSIGN, new_var_node(var, tok), arg, tok);
        node = new_binary(ND_COMMA, node, expr, tok);
    }

    *rest = skip(tok, ")");

    Node *funcall = new_node(ND_FUNCALL, start);
    funcall->funcname = strndup(start->loc, start->len);
    funcall->func_ty = ty;
    funcall->ty = ty->return_ty;
    funcall->args = args;
    funcall->nargs = nargs;
    return new_binary(ND_COMMA, node, funcall, tok);
}

// primary = "(" "{" stmt stmt* "}" ")"
//         | "(" expr ")"
//         | "sizeof" "(" type-name ")"
//         | "sizeof" unary
//         | ident func-args?
//         | str
//         | num
static Node *primary(Token **rest, Token *tok) {

    if (equal(tok, "(") && equal(tok->next, "{")) {
        Node *node = new_node(ND_STMT_EXPR, tok);
        node->body = compound_stmt(&tok, tok->next->next)->body;
        *rest = skip(tok, ")");

        Node *cur = node->body;
        while (cur->next)
            cur = cur->next;

        if (cur->kind != ND_EXPR_STMT)
            error_tok(cur->tok, "statment expression returning void is not supported");
        return node;
    }

    if (equal(tok, "(")) {
        Node *node = expr(&tok, tok->next);
        *rest = skip(tok, ")");
        return node;
    }

    if (equal(tok, "sizeof")) {
        if (equal(tok->next, "(") && is_typename(tok->next->next)) {
            Type *ty = typename(&tok, tok->next->next);
            *rest = skip(tok, ")");
            return new_num(ty->size, tok);
        }
        Node *node = unary(rest, tok->next);
        add_type(node);
        return new_num(node->ty->size, tok);
    }

    if (tok->kind == TK_IDENT) {
        // 関数呼び出し
        if (equal(tok->next, "("))
            return funcall(rest, tok);

        VarScope *sc = find_var(tok);
        if (!sc || (!sc->var && !sc->enum_ty))
            error_tok(tok, "undefined variable");

        Node *node;
        if (sc->var)
            node = new_var_node(sc->var, tok);
        else
            node = new_num(sc->enum_val, tok);
        *rest = tok->next;
        return node;
    }

    if (tok->kind == TK_STR) {
        Var *var = new_string_literal(tok->contents, tok->cont_len);
        *rest = tok->next;
        return new_var_node(var, tok);
    }

    if (tok->kind != TK_NUM)
        error_tok(tok, "expected expression");

    Node *node = new_num(get_number(tok), tok);
    *rest = tok->next;
    return node;
}

// program = (funcdef | global-var)*
Program *parse(Token *tok) {
    Function head = {};
    Function *cur = &head;
    globals = NULL;

    while (tok->kind != TK_EOF) {
        Token *start = tok;
        VarAttr attr = {};
        Type *basety = typespec(&tok, tok, &attr);
        Type *ty = declarator(&tok, tok, basety);

        // Typedef
        if (attr.is_typedef) {
            while (true) {
                push_scope(get_ident(ty->name))->type_def = ty;
                if (consume(&tok, tok, ";"))
                    break;
                tok = skip(tok, ",");
                ty = declarator(&tok, tok, basety);
            }
            continue;
        }

        // Function
        if (ty->kind == TY_FUNC) {
            current_fn = new_gvar(get_ident(ty->name), ty, false);
            if (!consume(&tok, tok, ";"))
                cur = cur->next = funcdef(&tok, start);
            continue;
        }

        // グローバル変数
        while (true) {
            new_gvar(get_ident(ty->name), ty, true);
            if (consume(&tok, tok, ";"))
                break;
            tok = skip(tok, ",");
            ty = declarator(&tok, tok, basety);
        }
    }

    Program *prog = calloc(1, sizeof(Program));
    prog->globals = globals;
    prog->fns = head.next;
    return prog;
}
