#define _POSIX_C_SOURCE 200809L
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

typedef struct Type Type;
typedef struct Member Member;

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

    int line_no;    // 行番号
};


void error(char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
void warn_tok(Token *tok, char *fmt, ...);
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
    ND_COMMA,       // ,
    ND_ADDR,        // &
    ND_MEMBER,      // . (メンバ参照演算子)
    ND_DEREF,       // *
    ND_NOT,         // !
    ND_RETURN,      // "return"
    ND_IF,          // "if"
    ND_FOR,         // "for"
    ND_BLOCK,       // { ... }
    ND_FUNCALL,     // 関数呼び出し
    ND_EXPR_STMT,   // Expression statement
    ND_STMT_EXPR,   // Statement expression
    ND_NULL_EXPR,   // 何もしない式
    ND_VAR,         // Variable
    ND_NUM,         // Integer
    ND_CAST,        // type cast
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

    // 構造体メンバのアクセス
    Member *member;

    // 関数呼び出し
    char *funcname;
    Type *func_ty;
    Var **args;
    int nargs;

    Var *var;       // ND_VARの場合、変数情報を格納するのに使う
    long val;       // ND_NUMの場合、値を格納するのに使う
};

typedef struct Function Function;
struct Function {
    Function *next;
    char *name;
    Var *params;    // 引数
    bool is_static;
    Node *node;
    Var *locals;
    int stack_size;
};

typedef struct {
    Var *globals;
    Function *fns;
} Program;

Node *new_cast(Node *expr, Type *ty);
Program *parse(Token *tok);

//
// type.c
//

typedef enum {
    TY_VOID,
    TY_BOOL,
    TY_CHAR,
    TY_SHORT,
    TY_INT,
    TY_LONG,
    TY_ENUM,
    TY_PTR,
    TY_FUNC,
    TY_ARRAY,
    TY_STRUCT,
} TypeKind;

struct Type {
    TypeKind kind;

    int size;   // sizeof value
    int align;  // alignment

    // Pointer
    Type *base;

    // 宣言
    Token *name;

    // Array
    int array_len;

    // 構造体
    Member *members;

    // Function type
    Type *return_ty;
    Type *params;
    Type *next;
};

// 構造体メンバ
struct Member {
    Member *next;
    Type *ty;
    Token *name;
    int offset;
};

extern Type *ty_void;
extern Type *ty_bool;

extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;

bool is_integer(Type *ty);
Type *copy_type(Type *ty);
int align_to(int n, int align);
Type *pointer_to(Type *base);
Type *array_of(Type *base, int len);
Type *enum_type(void);
Type *func_type(Type *return_ty);
void add_type(Node *node);

//
// codegen.c
//

void codegen(Program *prog);
