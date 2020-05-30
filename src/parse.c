#include "kcc.h"

// ローカル変数のリスト
Var *locals;

static Node *compound_stmt(Token **rest, Token *tok);
static Node *stmt(Token **rest, Token *tok);
static Node *expr_stmt(Token **rest, Token *tok);
static Node *expr(Token **rest, Token *tok);
static Node *assign(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

// 名前でローカル変数を検索する
static Var *find_var(Token *tok) {
    for (Var *var = locals; var; var = var->next) {
        if (strlen(var->name) == tok->len &&
            !strncmp(tok->loc, var->name, tok->len))
            return var;
    }
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

static Var *new_lvar(char *name) {
    Var *var = calloc(1, sizeof(Var));
    var->name = name;
    var->next = locals;
    locals = var;
    return var;
}

// トークンが数値の場合、値を返す
static long get_number(Token *tok) {
    if (tok->kind != TK_NUM)
        error_tok(tok, "expected a number");
    return tok->val;
}

//==================================================
// [生成規則]
//
// program       = stmt*
// stmt          = "return"? expr ";"
//               | "{" stmt* "}"
//               | "if" "(" expr ")" stmt ("else" stmt)?
//               | "for" "(" expr? ";" expr? ";" expr? ")" stmt
//               | "while" "(" expr ")" stmt
//               | "{" compound_stmt
// compound-stmt = stmt* "}"
// expr-stmt     = expr
// expr          = assign
// assign        = equality ("=" assign)?
// equality      = relational ("==" relational | "!=" relational)*
// relational    = add ("<" add | "<=" | ">" add | ">=" add)*
// add           = mul ("+" mul | "-" mul)*
// mul           = unary ("*" unary | "/" unary)*
// unary         =  ("+" | "-" | "*" | "&")? unary
//               | primary
// primary       = "(" expr ")" | ident | num
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
        node->lhs = expr(&tok, tok->next);
        *rest = skip(tok, ";");
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

        // initとincは値を返さない(処理するだけ)
        if (!equal(tok, ";"))
            node->init = expr_stmt(&tok, tok);
        tok = skip(tok, ";");

        // condは比較結果の値を返す
        if (!equal(tok, ";"))
            node->cond = expr(&tok, tok);
        tok = skip(tok, ";");

        if (!equal(tok, ")"))
            node->inc = expr_stmt(&tok, tok);

        tok = skip(tok, ")");

        node->then = stmt(rest, tok);
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

// compound-stmt = stmt* "}"
static Node *compound_stmt(Token **rest, Token *tok) {
    Node *node = new_node(ND_BLOCK, tok);
    Node head;
    Node *cur = &head;
    while (!equal(tok, "}"))
        cur = cur->next = stmt(&tok, tok);

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

// expr = assign
static Node *expr(Token **rest, Token *tok) {
    return assign(rest, tok);
}

// assign = equality ("=" assign)?
static Node *assign(Token **rest, Token *tok) {
    Node *node = equality(&tok, tok);
    if (equal(tok, "="))
        node = new_binary(ND_ASSIGN, node, assign(&tok, tok->next), tok);
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
    rhs = new_binary(ND_MUL, rhs, new_num(8, tok), tok);
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
        rhs = new_binary(ND_MUL, rhs, new_num(8, tok), tok);
        return new_binary(ND_SUB, lhs, rhs, tok);
    }

    // ptr - ptr, ptr - ptr の結果をサイズで割った値が返る
    // それは要素数だが、結果は負の値にもなる
    if (lhs->ty->base && rhs->ty->base) {
        Node *node = new_binary(ND_SUB, lhs, rhs, tok);
        return new_binary(ND_DIV, node, new_num(8, tok), tok);
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

// mul = unary ("*" unary | "/" unary)*
static Node *mul(Token **rest, Token *tok) {
    Node *node = unary(&tok, tok);

    for (;;) {
        Token *start = tok;

        if (equal(tok, "*")) {
            node = new_binary(ND_MUL, node, unary(&tok, tok->next), start);
            continue;
        }

        if (equal(tok, "/")) {
            node = new_binary(ND_DIV, node, unary(&tok, tok->next), start);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// unary = ("+" | "-" | "*" | "&")? unary | primary
static Node *unary(Token **rest, Token *tok) {
    if (equal(tok, "+"))
        return unary(rest, tok->next);

    if (equal(tok, "-"))
        return  new_binary(ND_SUB, new_num(0, tok), unary(rest, tok->next), tok);

    if (equal(tok, "&"))
        return new_unary(ND_ADDR, unary(rest, tok->next), tok);

    if (equal(tok, "*"))
        return new_unary(ND_DEREF, unary(rest, tok->next), tok);

    return primary(rest, tok);
}

// primary = "(" expr ")" | ident | num
static Node *primary(Token **rest, Token *tok) {
    if (equal(tok, "(")) {
        Node *node = expr(&tok, tok->next);
        *rest = skip(tok, ")");
        return node;
    }

    if (tok->kind == TK_IDENT) {
        Var *var = find_var(tok);
        if (!var)
            var = new_lvar(strndup(tok->loc, tok->len));
        *rest = tok->next;
        return new_var_node(var, tok);
    }

    Node *node = new_num(get_number(tok), tok);
    *rest = tok->next;
    return node;
}

// program = stmt*
Function *parse(Token *tok) {
    tok = skip(tok, "{");

    Function *prog = calloc(1, sizeof(Function));
    prog->node = compound_stmt(&tok, tok)->body;
    prog->locals = locals;
    return prog;
}
