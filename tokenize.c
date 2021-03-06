#include "kcc.h"

// 入力文字列
static char *current_input;

// 全ての入力ファイルのリスト
static char **input_files;

// 入力ファイル名
static char *current_filename;

static bool has_space;

// エラーを表示して終了する
void error(char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

// エラーメッセージを出力して終了する
// メッセージの形式
// foo.c:10: x = y + 1;
//               ^ <error message here>
static void verror_at(char *filename, char *input, int line_no, char *loc, char *fmt, va_list ap) {
    // locの存在する行を探す
    char *line = loc;
    while (input < line && line[-1] != '\n')
        line--;

    char *end = loc;
    while (*end && *end != '\n')
        end++;

    // 行を表示
    int ident = fprintf(stderr, "%s:%d: ", filename, line_no);
    fprintf(stderr, "%.*s\n", (int)(end - line), line);

    // エラーメッセージを表示する位置を計算
    int pos = loc - line + ident;

    fprintf(stderr, "%*s", pos, "");
    fprintf(stderr, "^ ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
}

static void error_at(char *loc, char *fmt, ...) {
    int line_no = 1;
    for (char *p = current_input; p < loc; p++)
        if (*p == '\n')
            line_no++;
    va_list ap;
    va_start(ap, fmt);
    verror_at(current_filename, current_input, line_no, loc, fmt, ap);
    exit(1);
}

void error_tok(Token *tok, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->filename, tok->input, tok->line_no, tok->loc, fmt, ap);
    exit(1);
}

void warn_tok(Token *tok, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->filename, tok->input, tok->line_no, tok->loc, fmt, ap);
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
    tok->filename = current_filename;
    tok->input = current_input;
    tok->at_bol = false;
    tok->has_space = has_space;
    cur->next = tok;

    has_space = false;

    return tok;
}

// 文字列pが文字列qから始まっているか
static bool startswith(char *p, char *q) {
    return strncmp(p, q, strlen(q)) == 0;
}

static bool is_keyword(Token *tok) {
    static char *kw[] = {
        "return", "if", "else", "for", "while", "int", "sizeof", "char",
        "struct", "union", "short", "long", "void", "typedef", "_Bool",
        "enum", "static", "break", "continue", "goto", "switch", "case",
        "default", "extern", "_Alignof", "_Alignas", "do", "signed", "unsigned",
        "const", "volatile", "register", "restrict", "_Noreturn", "float", "double",
    };

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

static bool is_hex(char c) {
    return ('0' <= c && c <= '9') ||
           ('a' <= c && c <= 'f') ||
           ('A' <= c && c <= 'F');
}

static int from_hex(char c) {
    if ('0' <= c && c <= '9')
        return c - '0';
    if ('a' <= c && c <= 'f')
        return c - 'a' + 10;
    return c - 'A' + 10;
}

void convert_keywords(Token *tok) {
    for (Token *t = tok; t->kind != TK_EOF; t = t->next)
        if (t->kind == TK_IDENT && is_keyword(t))
            t->kind = TK_RESERVED;
}

// 全てのトークンに行番号を割り当てる
static void add_line_info(Token *tok) {
    char *p = current_input;
    int line_no = 1;
    bool at_bol = true;

    do {
        if (p == tok->loc) {
            tok->line_no = line_no;
            tok->at_bol = at_bol;
            tok = tok->next;
        }
        if (*p == '\n') {
            line_no++;
            at_bol = true;
        } else if (!isspace(*p)) {
            at_bol = false;
        }
    } while (*p++);
}

static char read_escaped_char(char **new_pos, char *p) {
    if ('0' <= *p && *p <= '7') {
        // 8進数を読み取る
        int c = *p++ - '0';
        if ('0' <= *p && *p <= '7') {
            c = (c << 3) | (*p++ - '0');
            if ('0' <= *p && *p <= '7')
                c = (c << 3) | (*p++ - '0');
            if (c > 255)
                error_at(p, "octal escape sequence out of range");
        }
        *new_pos = p;
        return c;
    }

    if (*p == 'x') {
        // 16進数を読み取る
        p++;
        if (!is_hex(*p))
            error_at(p, "invalid hex escape sequence");

        int c = 0;
        for (; is_hex(*p); p++) {
            c = (c << 4) | from_hex(*p);
            if (c > 255)
                error_at(p, "hex escape sequence out of range");
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

static Token *read_char_literal(Token *cur, char *start) {
    char *p = start + 1;
    if (*p == '\0')
        error_at(start, "unclose char literal");

    char c;
    if (*p == '\\')
        c = read_escaped_char(&p, p + 1);
    else
        c = *p++;

    if (*p != '\'')
        error_at(p, "char literal too long");
    p++;

    Token *tok = new_token(TK_NUM, cur, start, p - start);
    tok->val = c;
    tok->ty = ty_int;
    return tok;
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

static Token *read_int_literal(Token *cur, char *start) {
    char *p = start;

    int base = 10;
    if (!strncasecmp(p, "0x", 2) && isxdigit(p[2])) {
        p += 2;
        base = 16;
    }
    else if (!strncasecmp(p, "0b", 2) && (p[2] == '0' || p[2] == '1')) {
        p += 2;
        base = 2;
    }
    else if (*p == '0') {
        base = 8;
    }

    long val = strtoul(p, &p, base);

    // Read U, L or LL suffixes.
    bool l = false;
    bool u = false;

    if (startswith(p, "LLU") || startswith(p, "LLu") || 
        startswith(p, "llU") || startswith(p, "llu") || 
        startswith(p, "ULL") || startswith(p, "Ull") || 
        startswith(p, "uLL") || startswith(p, "ull")) {
        p += 3;
        l = u = true;
    }
    else if (!strncasecmp(p, "lu", 2) || !strncasecmp(p, "ul", 2)) {
        p += 2;
        l = u = true;
    }
    else if (!strncasecmp(p, "LL", 2) || !strncasecmp(p, "ll", 2)) {
        p += 2;
        l = true;
    }
    else if (*p == 'L' || *p == 'l') {
        p++;
        l = true;
    }
    else if (*p == 'U' || *p == 'u') {
        p++;
        u = true;
    }

    Type *ty;
    if (base == 10) {
        if (l && u)
            ty = ty_ulong;
        else if (l)
            ty = ty_long;
        else if (u)
            ty = (val >> 32) ? ty_ulong : ty_uint;
        else
            ty = (val >> 31) ? ty_long : ty_int;
    } else {
        if (l && u)
            ty = ty_ulong;
        else if (l)
            ty = (val >> 63) ? ty_ulong : ty_long;
        else if (u)
            ty = (val >> 32) ? ty_ulong : ty_uint;
        else if (val >> 63)
            ty = ty_ulong;
        else if (val >> 32)
            ty = ty_long;
        else if (val >> 31)
            ty = ty_uint;
        else
            ty = ty_int;
    }

    Token *tok = new_token(TK_NUM, cur, start, p - start);
    tok->val = val;
    tok->ty = ty;
    return tok;
}

static Token *read_number(Token *cur, char *start) {
    Token *tok = read_int_literal(cur, start);
    if (!strchr(".eEfF", start[tok->len]))
        return tok;

    char *end;
    double val = strtod(start, &end);

    Type *ty;
    if (*end == 'f' || *end == 'F') {
        ty = ty_float;
        end++;
    }
    else if (*end == 'l' || *end == 'L') {
        ty = ty_double;
        end++;
    } else {
        ty = ty_double;
    }

    tok = new_token(TK_NUM, cur, start, end - start);
    tok->fval = val;
    tok->ty = ty;
    return tok;
}

Token *tokenize(char *filename, int file_no, char *p) {
    current_filename = filename;
    current_input = p;

    Token head = {};
    Token *cur = &head;

    has_space = false;

    while (*p) {

        // 行コメントをスキップ
        if (startswith(p, "//")) {
            p += 2;
            while (*p != '\n')
                p++;
            has_space = true;
            continue;
        }

        // ブロックコメントをスキップ
        if (startswith(p, "/*")) {
            char *q = strstr(p + 2, "*/");
            if (!q)
                error_at(p, "unclosed block comment");
            p = q + 2;
            has_space = true;
            continue;
        }

        // 空白文字をスキップ
        if (isspace(*p)) {
            p++;
            has_space = true;
            continue;
        }

        // 数値
        if (isdigit(*p) || (*p == '.' && isdigit(p[1]))) {
            cur = read_number(cur, p);
            p += cur->len;
            continue;
        }

        // 文字列リテラル
        if (*p == '"') {
            cur = read_string_literal(cur, p);
            p += cur->len;
            continue;
        }

        if (*p == '\'') {
            cur = read_char_literal(cur, p);
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

        // 3文字の区切り文字
        if (startswith(p, "<<=") || startswith(p, ">>=") || startswith(p, "...")) {
            cur = new_token(TK_RESERVED, cur, p, 3);
            p += 3;
            continue;
        }

        // ２文字の区切り文字
        if (startswith(p, "==") || startswith(p, "!=") ||
            startswith(p, ">=") || startswith(p, "<=") ||
            startswith(p, "->") || startswith(p, "+=") ||
            startswith(p, "-=") || startswith(p, "*=") ||
            startswith(p, "/=") || startswith(p, "%=") ||
            startswith(p, "++") || startswith(p, "--") ||
            startswith(p, "&=") || startswith(p, "|=") ||
            startswith(p, "^=") || startswith(p, "&&") ||
            startswith(p, "||") || startswith(p, "<<") ||
            startswith(p, ">>") || startswith(p, "##")) {
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

    for (Token *t = head.next; t; t = t->next)
        t->file_no = file_no;

    add_line_info(head.next);
    return head.next;
}

static char *read_file(char *path) {
    FILE *fp;

    if (!strcmp(path, "-")) {
        fp = stdin;
    } else {
        fp = fopen(path, "r");
        if (!fp)
            return NULL;
    }

    int buflen = 4096;
    int nread = 0;
    char *buf = malloc(buflen);

    // ファイル全体の読み取り
    while (true) {
        // 末尾の"\n","\0"のために2バイト確保
        int end = buflen - 2;
        int n = fread(buf + nread, 1, end - nread, fp);
        if (n == 0)
            break;

        nread += n;

        if (nread == end) {
            buflen *= 2;
            buf = realloc(buf, buflen);
        }
    }

    if (fp != stdin)
        fclose(fp);

    // ファイルが改行で終わっていない場合'\n'を追加する処理
    if (nread == 0 || buf[nread - 1] != '\n')
        buf[nread++] = '\n';
    buf[nread] = '\0';
    return buf;
}

char **get_input_files(void) {
    return input_files;
}

static void remove_backslash_newline(char *p) {
    char *q = p;

    int n = 0;
    
    while (*p) {
        if (startswith(p, "\\\n")) {
            p += 2;
            n++;
        }
        else if (*p == '\n') {
            *q++ = *p++;
            for (; n > 0; n--)
                *q ++ = '\n';
        } else {
            *q++ = *p++;
        }
    }
    *q = '\0';
}

Token *tokenize_file(char *path) {
    char *p = read_file(path);
    if (!p)
        return NULL;

    remove_backslash_newline(p);

    static int file_no;
    input_files = realloc(input_files, sizeof(char *) * (file_no + 2));
    input_files[file_no] = path;
    input_files[file_no + 1] = NULL;
    file_no++;

    return tokenize(path, file_no, p);
}

