#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>


//
// tokenizer.c
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


void error(char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *s);
Token *skip(Token *tok, char *s);
Token *tokenize(char *p);

//
// parser.c
//

typedef enum {
    ND_ADD,         // +
    ND_SUB,         // -
    ND_MUL,         // *
    ND_DIV,         // /
    ND_EQ,          // ==
    ND_NE,          // !=
    ND_LT,          // <=
    ND_LE,          // >=
    ND_RETURN,      // "return"
    ND_EXPR_STMT,   // Expression statement
    ND_NUM,         // Integer
} NodeKind;

// ASTノード型
typedef struct Node Node;
struct Node {
    NodeKind kind;  // ノード型
    Node *next;     // 次ノード
    Node *lhs;      // 左辺
    Node *rhs;      // 右辺
    long val;       // ND_NUMの場合使う
};

Node *parse(Token *tok);

//
// codegen.c
//

void codegen(Node *node);
