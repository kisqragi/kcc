#include "kcc.h"

// `#if`は入れ子にすることができるのでスタックを使って
// 入れ子にした`#if`を管理します。
typedef struct CondIncl CondIncl;
struct CondIncl {
    CondIncl *next;
    Token *tok;
};

static CondIncl *cond_incl;

static bool is_hash(Token *tok) {
    return tok->at_bol && equal(tok, "#");
}

// プリプロセッサディレクティブでの改行前の不要なトークンをスキップする
// ex. #include "hoge.h" aaa <-
static Token *skip_line(Token *tok) {
    if (tok->at_bol) 
        return tok;
    warn_tok(tok, "extra token");
    while (!tok->at_bol)
        tok = tok->next;
    return tok;
}

static Token *copy_token(Token *tok) {
    Token *t = malloc(sizeof(Token));
    *t = *tok;
    t->next = NULL;
    return t;
}

static Token *new_eof(Token *tok) {
    Token *t = copy_token(tok);
    t->kind = TK_EOF;
    t->len = 0;
    return t;
}

// tok1の末尾にtok2をつける
static Token *append(Token *tok1, Token *tok2) {
    if (!tok1 || tok1->kind == TK_EOF)
        return tok2;

    Token head = {};
    Token *cur = &head;

    while (tok1 && tok1->kind != TK_EOF) {
        cur = cur->next = copy_token(tok1);
        tok1 = tok1->next;
    }

    cur->next = tok2;
    return head.next;
}

// 次の`#endif`までスキップ
static Token *skip_cond_incl(Token *tok) {
    while (tok->kind != TK_EOF) {
        if (is_hash(tok) && equal(tok->next, "endif"))
            return tok;
        tok = tok->next;
    }
    return tok;
}

static Token *copy_line(Token **rest, Token *tok) {
    Token head = {};
    Token *cur = &head;

    for (; !tok->at_bol; tok = tok->next)
        cur = cur->next = copy_token(tok);

    cur->next = new_eof(tok);
    *rest = tok;
    return head.next;
}

static long eval_const_expr(Token **rest, Token *tok) {
    Token *expr = copy_line(rest, tok);
    Token *rest2;
    long val = const_expr(&rest2, expr);
    if (rest2->kind != TK_EOF)
        error_tok(rest2, "extra token");
    return val;
}

static CondIncl *push_cond_incl(Token *tok) {
    CondIncl *ci = calloc(1, sizeof(CondIncl));
    ci->next = cond_incl;
    ci->tok = tok;
    cond_incl = ci;
    return ci;
}

static Token *preprocess2(Token *tok) {
    Token head = {};
    Token *cur = &head;

    while (tok->kind != TK_EOF) {
        if (!is_hash(tok)) {
            cur = cur->next = tok;
            tok = tok->next;
            continue;
        }

        Token *start = tok;
        tok = tok->next;

        if (equal(tok, "include")) {
            tok = tok->next;

            if (tok->kind != TK_STR)
                error_tok(tok, "expected a filename");

            char *path = tok->contents;
            Token *tok2 = tokenize_file(path);
            if (!tok2)
                error_tok(tok, "%s", strerror(errno));

            tok = skip_line(tok->next);
            tok = append(tok2, tok);
            continue;
        }

        if (equal(tok, "if")) {
            long val = eval_const_expr(&tok, tok->next);
            push_cond_incl(start);
            if (!val)
                tok = skip_cond_incl(tok);
            continue;
        }

        if (equal(tok, "endif")) {
            if (!cond_incl)
                error_tok(start, "stray #endif");
            cond_incl = cond_incl->next;
            tok = skip_line(tok->next);
            continue;
        }

        // 注意: `#`のみの行は有効(null directives)
        if (tok->at_bol)
            continue;

        error_tok(tok, "invalid preprocessor directive");
    }

    cur->next = tok;
    return head.next;
}

Token *preprocess(Token *tok) {
    tok = preprocess2(tok);
    if (cond_incl)
        error_tok(cond_incl->tok, "unterminated conditional directive");
    convert_keywords(tok);
    return tok;
}
