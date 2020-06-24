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

// トークンが期待するトークンだった場合、現在のトークンを消費して
// 真を返す。違う場合消費せず偽を返す
bool consume(Token **rest, Token *tok, char *str) {
    if (equal(tok, str)) {
        *rest = tok->next;
        return true;
    }
    *rest = tok;
    return false;
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
    static char *kw[] = {"return", "if", "else", "for", "while", "int", "sizeof", "char"};

    for (int i = 0; i < sizeof(kw) / sizeof(kw); i++)
        if (equal(tok, kw[i]))
            return true;
    return false;
}

static bool is_alpha(char p) {
    return isalpha(p) || p == '_';
}

static bool is_alnum(char p) {
    return isalnum(p) || p == '_';
}

static void convert_keywords(Token *tok) {
    for (Token *t = tok; t->kind != TK_EOF; t = t->next)
        if (t->kind == TK_IDENT && is_keyword(t))
            t->kind = TK_RESERVED;
}

static char read_escaped_char(char **new_pos, char *p) {
    if ('0' <= *p && *p <= '7') {
        // 8進数を読み取る
        int c = *p++ - '0';
        if ('0' <= *p && *p <= '7') {
            c = (c << 3) | (*p++ - '0');
            if ('0' <= *p && *p <= '7')
                c = (c << 3) | (*p++ - '0');
        }
        *new_pos = p;
        return c;
    }

    *new_pos = p + 1;

    switch (*p) {
        case 'a': return '\a';
        case 'b': return '\b';
        case 't': return '\t';
        case 'n': return '\n';
        case 'v': return '\v';
        case 'f': return '\f';
        case 'r': return '\r';
        default: return *p;
    }
}

static Token *read_string_literal(Token *cur, char *start) {
    char *p = start + 1;    // eat '"'
    char *end = p;

    // 終端のダブルクオートを見つける
    for (; *end != '"'; end++) {
        if (*end == '\0')
            error_at(start, "enclosed string literal");
        if (*end == '\\')
            end++;
    }

    char *buf = malloc(end - p + 1);
    int len = 0;

    while (*p != '"') {
        if (*p == '\\') {
            buf[len++] = read_escaped_char(&p, p + 1);
        } else {
            buf[len++] = *p++;
        }
    }

    buf[len++] = '\0';

    Token *tok = new_token(TK_STR, cur, start, p - start + 1);
    tok->contents = buf;
    tok->cont_len = len;
    return tok;
}

Token *tokenize(char *p) {
    current_input = p;

    Token head = {};
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

        // 文字列リテラル
        if (*p == '"') {
            cur = read_string_literal(cur, p);
            p += cur->len;
            continue;
        }

        // 識別子
        if (is_alpha(*p)) {
            char *q = p++;
            while (is_alnum(*p)) {
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

