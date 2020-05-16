#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum {
    TK_RESERVED,    // キーワード(予約語)と区切り記号
    TK_NUM,         // 数値
    TK_EOF          // End Of File
} TokenKind;

typedef struct Token Token;
struct Token {
    TokenKind kind; // トークンの種類
    Token *next;    // 次のトークン
    long val;       // TK_NUMの場合に値を格納するのに使う
    char *loc;      // トークンの位置
    int len;        // トークンの長さ
};

// 入力文字列
static char *current_input;

// エラーを表示して終了する
static void error(char *fmt, ...) {
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

static void error_tok(Token *tok, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->loc, fmt, ap);
}

// 現在のトークンが's'であることを確認する
static bool equal(Token *tok, char *s) {
    return strlen(s) == tok->len &&
            !strncmp(tok->loc, s, tok->len);
}

// トークンが's'に一致する場合、次のトークンを返す
static Token *skip(Token *tok, char *s) {
    if (!equal(tok, s))
        error_tok(tok, "expected '%s'", s);
    return tok->next;
}

// トークンが数値の場合、値を返す
static long get_number(Token *tok) {
    if (tok->kind != TK_NUM)
        error_tok(tok, "expected a number");
    return tok->val;
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

static Token *tokenize(void) {
    char *p = current_input;
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

        // 区切り文字
        if (ispunct(*p)) {
            cur = new_token(TK_RESERVED, cur, p++, 1);
            continue;
        }

        error_at(p, "invalid token");
    }

    new_token(TK_EOF, cur, p, 0);
    return head.next;
}

int main(int argc, char **argv) {
    if (argc != 2)
        error("%s: invalid number of arguments\n", argv[0]);

    current_input = argv[1];
    Token *tok = tokenize();

    printf(".intel_syntax noprefix\n");
    printf(".globl main\n");
    printf("main:\n");

    // 最初のトークンは数値でなければいけない
    printf("    mov rax, %ld\n", get_number(tok));
    tok = tok->next;

    while (tok->kind != TK_EOF) {
        if (equal(tok, "+")) {
            printf("    add rax, %ld\n", get_number(tok->next));
            tok = tok->next->next;
            continue;
        }

        tok = skip(tok, "-");
        printf("    sub rax, %ld\n", get_number(tok));
        tok = tok->next;
        continue;
    }

    printf("    ret\n");
    return 0;
}
