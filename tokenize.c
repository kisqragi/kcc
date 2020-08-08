#include "kcc.h"

// 入力文字列
static char *current_input;

// 入力ファイル名
static char *current_filename;

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
static void verror_at(int line_no, char *loc, char *fmt, va_list ap) {
    // locの存在する行を探す
    char *line = loc;
    while (current_input < line && line[-1] != '\n')
        line--;

    char *end = loc;
    while (*end != '\n')
        end++;

    // 行を表示
    int ident = fprintf(stderr, "%s:%d: ", current_filename, line_no);
    fprintf(stderr, "%.*s\n", (int)(end - line), line);

    // エラーメッセージを表示する位置を計算
    int pos = loc - line + ident;

    fprintf(stderr, "%*s", pos, "");
    fprintf(stderr, "^ ");
    vfprintf(stderr, fmt, ap);
    fprintf(stderr, "\n");
    exit(1);
}

static void error_at(char *loc, char *fmt, ...) {
    int line_no = 1;
    for (char *p = current_input; p < loc; p++)
        if (*p == '\n')
            line_no++;
    va_list ap;
    va_start(ap, fmt);
    verror_at(line_no, loc, fmt, ap);
}

void error_tok(Token *tok, char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    verror_at(tok->line_no, tok->loc, fmt, ap);
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
    static char *kw[] = {
        "return", "if", "else", "for", "while", "int", "sizeof", "char",
        "struct", "union", "short", "long", "void",
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

static void convert_keywords(Token *tok) {
    for (Token *t = tok; t->kind != TK_EOF; t = t->next)
        if (t->kind == TK_IDENT && is_keyword(t))
            t->kind = TK_RESERVED;
}

// 全てのトークンに行番号を割り当てる
static void add_line_info(Token *tok) {
    char *p = current_input;
    int line_no = 1;

    do {
        if (p == tok->loc) {
            tok->line_no = line_no;
            tok = tok->next;
        }
        if (*p == '\n')
            line_no++;
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

static Token *tokenize(char *filename, char *p) {
    current_filename = filename;
    current_input = p;

    Token head = {};
    Token *cur = &head;

    while (*p) {

        // 行コメントをスキップ
        if (startswith(p, "//")) {
            p += 2;
            while (*p != '\n')
                p++;
            continue;
        }

        // ブロックコメントをスキップ
        if (startswith(p, "/*")) {
            char *q = strstr(p + 2, "*/");
            if (!q)
                error_at(p, "unclosed block comment");
            p = q + 2;
            continue;
        }

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
            startswith(p, ">=") || startswith(p, "<=") ||
            startswith(p, "->")) {
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
    add_line_info(head.next);
    convert_keywords(head.next);
    return head.next;
}

static char *read_file(char *path) {
    FILE *fp;

    if (!strcmp(path, "-")) {
        fp = stdin;
    } else {
        fp = fopen(path, "r");
        if (!fp)
            error("cannot open %s: %s", path, strerror(errno));
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

Token *tokenize_file(char *path) {
    return tokenize(path, read_file(path));
}

