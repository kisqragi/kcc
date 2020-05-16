#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


//
// Tokenizer
//

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

//
// Parser
//

typedef enum {
    ND_ADD, // +
    ND_SUB, // -
    ND_MUL, // *
    ND_DIV, // /
    ND_NUM, // Integer
} NodeKind;

// ASTノード型
typedef struct Node Node;
struct Node {
    NodeKind kind;  // ノード型
    Node *lhs;      // 左辺
    Node *rhs;      // 右辺
    long val;       // ND_NUMの場合使う
};

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

static Node *expr(Token **rest, Token *tok);
static Node *mul(Token **rest, Token *tok);
static Node *unary(Token **rest, Token *tok);
static Node *primary(Token **rest, Token *tok);

//==================================================
// [生成規則]
//
// expr    = mul ("+" mul | "-" mul)*
// mul     = primary ("*" unary | "/" unary)*
// unary   = ("+" | "-")? unary
//         | primary
// primary = "(" expr ")" | num
//
//==================================================

// expr = mul ("+" mul | "-" mul)*
static Node *expr(Token **rest, Token *tok) {
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

// mul = primary ("*" primary | "/" primary)*
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

// unary   = ("+" | "-")? unary
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

//
// Code generator
//

static char *reg(int idx) {
    static char *r[] = {"r10", "r11", "r12", "r13", "r14", "r15"};
    if (idx < 0 || sizeof(r) / sizeof(*r) <= idx)
        error("register out of range: %d", idx);
    return r[idx];
}

static int top;

static void gen_expr(Node *node) {
    if (node->kind == ND_NUM) {
        printf("    mov %s, %lu\n", reg(top++), node->val);
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
        default:
            error("invalid expression");
    }
}

int main(int argc, char **argv) {
    if (argc != 2)
        error("%s: invalid number of arguments\n", argv[0]);

    current_input = argv[1];
    Token *tok = tokenize();
    Node *node = expr(&tok, tok);

    if (tok->kind != TK_EOF)
        error_tok(tok, "extra token");

    printf(".intel_syntax noprefix\n");
    printf(".globl main\n");
    printf("main:\n");

    // callee-saved
    // 呼び出し先がレジスタを保存する　
    printf("    push r12\n");
    printf("    push r13\n");
    printf("    push r14\n");
    printf("    push r15\n");

    // アセンブリのコードを生成する
    gen_expr(node);

    // 関数の返り値を設定
    printf("    mov rax, %s\n", reg(top - 1));

    // 退避させたレジスタの値を元に戻す
    printf("    pop r12\n");
    printf("    pop r13\n");
    printf("    pop r14\n");
    printf("    pop r15\n");
    printf("    ret\n");

    return 0;
}
