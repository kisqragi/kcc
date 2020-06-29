#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef struct Type Type;

//
// tokenizer.c
//

typedef enum {
    TK_RESERVED,    // キーワード(予約語)と区切り記号
    TK_IDENT,       // 識別子
    TK_STR,         // 文字列リテラル
    TK_NUM,         // 数値
    TK_EOF,         // End Of File
} TokenKind;

// Token
typedef struct Token Token;
struct Token {
    TokenKind kind; // トークンの種類
    Token *next;    // 次のトークン
    long val;       // TK_NUMの場合に値を格納するのに使う
    char *loc;      // トークンの位置
    int len;        // トークンの長さ

    char *contents; // '\0'を含む文字列リテラル
    char cont_len;  // 文字列リテラルの長さ
};


void error(char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *s);
Token *skip(Token *tok, char *s);
bool consume(Token **rest, Token *tok, char *str);
Token *tokenize_file(char *filename);

//
// parser.c
//

// 変数
typedef struct Var Var;
struct Var {
    Var *next;
    char *name; // 変数名
    Type *ty;   // Type
    bool is_local;

    // ローカル変数
    int offset;

    // グローバル変数
    char *init_data;
};

typedef enum {
    ND_ADD,         // +
    ND_SUB,         // -
    ND_MUL,         // *
    ND_DIV,         // /
    ND_EQ,          // ==
    ND_NE,          // !=
    ND_LT,          // <=
    ND_LE,          // >=
    ND_ASSIGN,      // =
    ND_ADDR,        // &
    ND_DEREF,       // *
    ND_RETURN,      // "return"
    ND_IF,          // "if"
    ND_FOR,         // "for"
    ND_BLOCK,       // { ... }
    ND_FUNCALL,     // 関数呼び出し
    ND_EXPR_STMT,   // Expression statement
    ND_STMT_EXPR,   // Statement expression
    ND_VAR,         // Variable
    ND_NUM,         // Integer
} NodeKind;

// ASTノード型
typedef struct Node Node;
struct Node {
    NodeKind kind;  // ノード型
    Node *next;     // 次ノード
    Type *ty;       // 型、intやintへのポインタなど
    Token *tok;

    Node *lhs;      // 左辺
    Node *rhs;      // 右辺

    // "if" or "for" statement
    Node *cond;
    Node *then;
    Node *els;
    Node *init;
    Node *inc;

    // Block or statement expression
    Node *body;

    // 関数呼び出し
    char *funcname;
    Node *args;

    Var *var;       // ND_VARの場合、変数情報を格納するのに使う
    long val;       // ND_NUMの場合、値を格納するのに使う
};

typedef struct Function Function;
struct Function {
    Function *next;
    char *name;
    Var *params;    // 引数
    Node *node;
    Var *locals;
    int stack_size;
};

typedef struct {
    Var *globals;
    Function *fns;
} Program;

Program *parse(Token *tok);

//
// type.c
//

typedef enum {
    TY_CHAR,
    TY_INT,
    TY_PTR,
    TY_FUNC,
    TY_ARRAY,
} TypeKind;

struct Type {
    TypeKind kind;

    int size;

    // Pointer
    Type *base;

    // 宣言
    Token *name;

    // Array
    int array_len;

    // Function type
    Type *return_ty;
    Type *params;
    Type *next;
};

extern Type *ty_char;
extern Type *ty_int;

bool is_integer(Type *ty);
Type *copy_type(Type *ty);
Type *pointer_to(Type *base);
Type *array_of(Type *base, int len);
Type *func_type(Type *return_ty);
void add_type(Node *node);

//
// codegen.c
//

void codegen(Program *prog);
