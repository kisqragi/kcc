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
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>
#include <libgen.h>

typedef struct Type Type;
typedef struct Hideset Hideset;
typedef struct Member Member;
typedef struct Relocation Relocation;

//
// tokenize.c
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
    TokenKind kind;     // トークンの種類
    Token *next;        // 次のトークン
    long val;           // TK_NUMの場合に値を格納するのに使う
    double fval;
    char *loc;          // トークンの位置
    int len;            // トークンの長さ
    Type *ty;

    char *contents;     // '\0'を含む文字列リテラル
    char cont_len;      // 文字列リテラルの長さ

    char *filename;
    char *input;
    int line_no;        // 行番号
    int file_no;        // .locディレクティブのファイル番号
    bool at_bol;        // このトークンが行の先頭の場合true
    bool has_space;     // このトークンがスペースの後にある場合true
    Hideset *hideset;   // macro展開で利用
};


void error(char *fmt, ...);
void error_tok(Token *tok, char *fmt, ...);
void warn_tok(Token *tok, char *fmt, ...);
bool equal(Token *tok, char *s);
Token *skip(Token *tok, char *s);
bool consume(Token **rest, Token *tok, char *str);
void convert_keywords(Token *tok);
char **get_input_files(void);
Token *tokenize_file(char *filename);
Token *tokenize(char *filename, int file_no, char *p);

//
// preprocess.c
//

Token *preprocess(Token *tok);

//
// parser.c
//

// 変数
typedef struct Var Var;
struct Var {
    Var *next;
    char *name; // 変数名
    Type *ty;   // Type
    Token *tok;
    bool is_local;
    int align;  // alignment

    // ローカル変数
    int offset;

    // グローバル変数
    bool is_static;
    char *init_data;
    Relocation *rel;
};

struct Relocation {
    Relocation *next;
    int offset;
    char *label;
    long addend;
};

typedef enum {
    ND_ADD,         // +
    ND_SUB,         // -
    ND_MUL,         // *
    ND_DIV,         // /
    ND_MOD,         // %
    ND_EQ,          // ==
    ND_NE,          // !=
    ND_LT,          // <=
    ND_LE,          // >=
    ND_ASSIGN,      // =
    ND_COND,        // ?: (条件付/三項演算子, conditional operator)
    ND_COMMA,       // ,
    ND_ADDR,        // &
    ND_MEMBER,      // . (メンバ参照演算子)
    ND_DEREF,       // *
    ND_NOT,         // !
    ND_BITNOT,      // ~
    ND_BITAND,      // &
    ND_BITOR,       // | 
    ND_BITXOR,      // ^
    ND_SHL,         // <<
    ND_SHR,         // >>
    ND_RETURN,      // "return"
    ND_LOGAND,      // && 'Logical AND'
    ND_LOGOR ,      // ||
    ND_IF,          // "if"
    ND_FOR,         // "for"
    ND_DO,          // "do"
    ND_SWITCH,      // "switch"
    ND_CASE,        // "case"
    ND_BLOCK,       // { ... }
    ND_BREAK,       // "break"
    ND_CONTINUE,    // "continue"
    ND_GOTO,        // "goto"
    ND_LABEL,       // Labeled statement
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

    bool is_init;

    // Block or statement expression
    Node *body;

    // 構造体メンバのアクセス
    Member *member;

    // 関数呼び出し
    Type *func_ty;
    Var **args;
    int nargs;

    // Goto or labeled statement
    char *label_name;

    // Switch-cases
    Node *case_next;
    Node *default_case;
    int case_label;
    int case_end_label;

    Var *var;       // ND_VARの場合、変数情報を格納するのに使う
    long val;       // ND_NUMの場合、値を格納するのに使う
    double fval;
};

typedef struct Function Function;
struct Function {
    Function *next;
    char *name;
    Var *params;    // 引数
    bool is_static;
    bool is_variadic;

    Node *node;
    Var *locals;
    int stack_size;
};

typedef struct {
    Var *globals;
    Function *fns;
} Program;

Node *new_cast(Node *expr, Type *ty);
long const_expr(Token **rest, Token *tok);
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
    TY_FLOAT,
    TY_DOUBLE,
    TY_ENUM,
    TY_PTR,
    TY_FUNC,
    TY_ARRAY,
    TY_STRUCT,
} TypeKind;

struct Type {
    TypeKind kind;

    int size;         // sizeof value
    int align;        // alignment
    bool is_unsigned; // unsigned or signed
    bool is_const;

    // Pointer
    Type *base;

    // 宣言
    Token *name;
    Token *name_pos;

    // Array
    int array_len;

    // 構造体
    Member *members;

    // Function type
    Type *return_ty;
    Type *params;
    bool is_variadic;
    Type *next;
};

// 構造体メンバ
struct Member {
    Member *next;
    Type *ty;
    Token *name;
    int align;
    int offset;
};

extern Type *ty_void;
extern Type *ty_bool;

extern Type *ty_char;
extern Type *ty_short;
extern Type *ty_int;
extern Type *ty_long;

extern Type *ty_uchar;
extern Type *ty_ushort;
extern Type *ty_uint;
extern Type *ty_ulong;

extern Type *ty_float;
extern Type *ty_double;

bool is_integer(Type *ty);
bool is_flonum(Type *ty);
bool is_numeric(Type *ty);
Type *copy_type(Type *ty);
int align_to(int n, int align);
Type *pointer_to(Type *base);
Type *array_of(Type *base, int len);
Type *enum_type(void);
Type *struct_type(void);
Type *func_type(Type *return_ty);
void add_type(Node *node);

//
// codegen.c
//

void codegen(Program *prog);

//
// main.c
//

extern char **include_paths;
