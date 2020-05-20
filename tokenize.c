#include "kcc.h"

// 入力文字列
static char *current_input;

// エラーを表示して終了する
void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

// エラーメッセージを出力して終了する
static void verror_at(char *loc, char *fmt, va_list ap) {
    int pos = loc - current_input;
    fprintf(stderr, "%s\n", current_input);
    fprintf(stderr, "%*s", pos, "");
    fprintf(stderr, "^ ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

static void error_at(char *loc, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(loc, fmt, ap);
}

void error_tok(Token *tok, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->loc, fmt, ap);
}

// 現在のトークンが's'であることを確認する
bool equal(Token *tok, char *s) {
    return strlen(s) == tok->len &&
            !strncmp(tok->loc, s, tok->len);
}

// トークンが's'に一致する場合、次のトークンを返す
Token *skip(Token *tok, char *s) {
    if (!equal(tok, s))
        error_tok(tok, "expected '%s'", s);
    return tok->next;
}

// 新しいトークンを作成し、curの次のトークンとして追加する
static Token *new_token(TokenKind kind, Token *cur, char *loc, int len) {
    Token *tok = calloc(1, sizeof(Token));
    tok->kind = kind;
    tok->loc = loc;
    tok->len = len;
    cur->next = tok;
    return tok;
}

// 文字列pが文字列qから始まっているか
static bool startswith(char *p, char *q) {
    return strncmp(p, q, strlen(q)) == 0;
}

static bool is_keyword(Token *tok) {
    static char *kw[] = {"return", "if", "else", "for"};

    for (int i = 0; i < sizeof(kw) / sizeof(kw); i++)
        if (equal(tok, kw[i]))
            return true;
    return false;
}

static void convert_keywords(Token *tok) {
    for (Token *t = tok; t->kind != TK_EOF; t = t->next)
        if (t->kind == TK_IDENT && is_keyword(t))
            t->kind = TK_RESERVED;
}

Token *tokenize(char *p) {
    current_input = p;

    Token head;
    Token *cur = &head;

    while (*p) {
        // 空白文字をスキップ
        if (isspace(*p)) {
            p++;
            continue;
        }

        // 数値
        if (isdigit(*p)) {
            cur = new_token(TK_NUM, cur, p, 0);
            char *q = p;
            cur->val = strtoul(p, &p, 10);
            cur->len = p - q;
            continue;
        }

        if (isalpha(*p)) {
            char *q = p++;
            while (isalnum(*p)) {
                p++;
            }
            cur = new_token(TK_IDENT, cur, q, p - q);
            continue;
        }

        // ２文字の区切り文字
        if (startswith(p, "==") || startswith(p, "!=") ||
            startswith(p, ">=") || startswith(p, "<=")) {
            cur = new_token(TK_RESERVED, cur, p, 2);
            p += 2;
            continue;
        }

        // １文字の区切り文字
        if (ispunct(*p)) {
            cur = new_token(TK_RESERVED, cur, p++, 1);
            continue;
        }

        error_at(p, "invalid token");
    }

    new_token(TK_EOF, cur, p, 0);
    convert_keywords(head.next);
    return head.next;
}

