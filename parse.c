#include "kcc.h"

static Node *expr(Token **rest, Token *tok);
static Node *equality(Token **rest, Token *tok);
static Node *relational(Token **rest, Token *tok);
static Node *add(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

static Node *new_node(NodeKind kind) {
    Node *node = calloc(1, sizeof(Node));
    node->kind = kind;
    return node;
}

// 二項演算
static Node *new_binary(NodeKind kind, Node *lhs, Node *rhs) {
    Node *node = new_node(kind);
    node->lhs = lhs;
    node->rhs = rhs;
    return node;
}

static Node *new_num(long val) {
    Node *node = new_node(ND_NUM);
    node->val = val;
    return node;
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
// expr       = equality
// equality   = relational ("==" relational | "!=" relational)*
// relational = add ("<" add | "<=" | ">" add | ">=" add)*
// add        = mul ("+" mul | "-" mul)*
// mul        = unary ("*" unary | "/" unary)*
// unary      = ("+" | "-")? unary | primary
// primary    = "(" expr ")" | num
//
//==================================================

// expr = equality
static Node *expr(Token **rest, Token *tok) {
    return equality(rest, tok);
}

// equality = relational ("==" relational | "!=" relational)*
static Node *equality(Token **rest, Token *tok) {
    Node *node = relational(&tok, tok);

    for (;;) {

        if (equal(tok, "==")) {
            Node *rhs = relational(&tok, tok->next);
            node = new_binary(ND_EQ, node, rhs);
            continue;
        }

        if (equal(tok, "!=")) {
            Node *rhs = relational(&tok, tok->next);
            node = new_binary(ND_NE, node, rhs);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// relational = add ("<" add | "<=" | ">" add | ">=" add)*
static Node *relational(Token **rest, Token *tok) {
    Node *node = add(&tok, tok);

    for (;;) {

        if (equal(tok, "<")) {
            Node *rhs = add(&tok, tok->next);
            node = new_binary(ND_LT, node, rhs);
            continue;
        }

        if (equal(tok, "<=")) {
            Node *rhs = add(&tok, tok->next);
            node = new_binary(ND_LE, node, rhs);
            continue;
        }

        if (equal(tok, ">")) {
            Node *rhs = add(&tok, tok->next);
            node = new_binary(ND_LT, rhs, node);
            continue;
        }

        if (equal(tok, ">=")) {
            Node *rhs = add(&tok, tok->next);
            node = new_binary(ND_LE, rhs, node);
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
        if (equal(tok, "+")) {
            Node *rhs = mul(&tok, tok->next);
            node = new_binary(ND_ADD, node, rhs);
            continue;
        }

        if (equal(tok, "-")) {
            Node *rhs = mul(&tok, tok->next);
            node = new_binary(ND_SUB, node, rhs);
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

        if (equal(tok, "*")) {
            Node *rhs = unary(&tok, tok->next);
            node = new_binary(ND_MUL, node, rhs);
            continue;
        }

        if (equal(tok, "/")) {
            Node *rhs = unary(&tok, tok->next);
            node = new_binary(ND_DIV, node, rhs);
            continue;
        }

        *rest = tok;
        return node;
    }
}

// unary = ("+" | "-")? unary | primary
static Node *unary(Token **rest, Token *tok) {
    if (equal(tok, "+"))
        return unary(rest, tok->next);

    if (equal(tok, "-")) {
        Node *rhs = unary(rest, tok->next);
        /*
        Node *rhs = unary(&tok, tok->next);
        *rest = tok;
        */
        return  new_binary(ND_SUB, new_num(0), rhs);
    }

    return primary(rest, tok);
}

// primary = "(" expr ")" | num
static Node *primary(Token **rest, Token *tok) {
    if (equal(tok, "(")) {
        Node *node = expr(&tok, tok->next);
        *rest = skip(tok, ")");
        return node;
    }

    Node *node = new_num(get_number(tok));
    *rest = tok->next;
    return node;
}

Node *parse(Token *tok) {
    Node *node = expr(&tok, tok);
    if (tok->kind != TK_EOF)
        error_tok(tok, "extra token");
    return node;
}
