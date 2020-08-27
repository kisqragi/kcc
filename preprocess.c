#include "kcc.h"

typedef struct MacroParam MacroParam;
struct MacroParam {
    MacroParam *next;
    char *name;
};

typedef struct MacroArg MacroArg;
struct MacroArg {
    MacroArg *next;
    char *name;
    Token *tok;
};

typedef struct Macro Macro;
struct Macro {
    Macro *next;
    char *name;
    bool is_objlike; // obj-like or func-like
    MacroParam *params;
    Token *body;
    bool deleted;
};

// `#if`は入れ子にすることができるのでスタックを使って
// 入れ子にした`#if`を管理します。
typedef struct CondIncl CondIncl;
struct CondIncl {
    CondIncl *next;
    enum { IN_THEN, IN_ELIF, IN_ELSE } ctx;
    Token *tok;
    bool included;
};

typedef struct Hideset Hideset;
struct Hideset {
    Hideset *next;
    char *name;
};

static Macro *macros;
static CondIncl *cond_incl;

static Token *copy_line(Token **rest, Token *tok);
static Token *preprocess2(Token *tok);

static bool is_hash(Token *tok) {
    return tok->at_bol && equal(tok, "#");
}

// プリプロセッサディレクティブでの改行前の不要なトークンをスキップする
// ex. #include "hoge.h" aaa <-
static Token *skip_line(Token *tok) {
    if (tok->at_bol) 
        return tok;
    warn_tok(tok, "extra token");
    while (!tok->at_bol)
        tok = tok->next;
    return tok;
}

static Token *copy_token(Token *tok) {
    Token *t = malloc(sizeof(Token));
    *t = *tok;
    t->next = NULL;
    return t;
}

static Token *new_eof(Token *tok) {
    Token *t = copy_token(tok);
    t->kind = TK_EOF;
    t->len = 0;
    return t;
}

static Hideset *new_hideset(char *name) {
    Hideset *hs = calloc(1, sizeof(Hideset));
    hs->name = name;
    return hs;
}

static Hideset *hideset_union(Hideset *hs1, Hideset *hs2) {
    Hideset head = {};
    Hideset *cur = &head;
    
    for (; hs1; hs1 = hs1->next)
        cur = cur->next = new_hideset(hs1->name);
    cur->next = hs2;
    return head.next;
}

static bool hideset_contains(Hideset *hs, char *s, int len) {
    for (; hs; hs = hs->next) {
        if (strlen(hs->name) == len && !strncmp(hs->name, s, len))
            return true;
    }
    return false;
}

static Token *add_hideset(Token *tok, Hideset *hs) {
    Token head = {};
    Token *cur = &head;

    for (; tok; tok = tok->next) {
        Token *t = copy_token(tok);
        t->hideset = hideset_union(t->hideset, hs);
        cur = cur->next = t;
    }
    return head.next;
}

// tok1の末尾にtok2をつける
static Token *append(Token *tok1, Token *tok2) {
    if (tok1->kind == TK_EOF)
        return tok2;

    Token head = {};
    Token *cur = &head;

    while (tok1->kind != TK_EOF) {
        cur = cur->next = copy_token(tok1);
        tok1 = tok1->next;
    }

    cur->next = tok2;
    return head.next;
}

static Macro *find_macro(Token *tok) {
    if (tok->kind != TK_IDENT)
        return NULL;

    for (Macro *m = macros; m; m = m->next) {
        if (strlen(m->name) == tok->len && !strncmp(m->name, tok->loc, tok->len))
            return m->deleted ? NULL : m;
    }
    return NULL;
}

static Macro *add_macro(char *name, bool is_objlike, Token *body) {
    Macro *m = calloc(1, sizeof(Macro));
    m->next = macros;
    m->name = name;
    m->is_objlike = is_objlike;
    m->body = body;
    macros = m;
    return m;
}

static MacroParam *read_macro_param(Token **rest, Token *tok) {
    MacroParam head = {};
    MacroParam *cur = &head;

    while (!equal(tok, ")")) {
        if (cur != &head)
            tok = skip(tok, ",");

        if (tok->kind != TK_IDENT)
            error_tok(tok, "expected an identifier");

        MacroParam *m = calloc(1, sizeof(MacroParam));
        m->name = strndup(tok->loc, tok->len);
        cur = cur->next = m;
        tok = tok->next;
    }
    *rest = tok->next;
    return head.next;
}

static void read_macro_definition(Token **rest, Token *tok) {
    if (tok->kind != TK_IDENT)
        error_tok(tok, "macro name must be an identifier");
    char *name = strndup(tok->loc, tok->len);
    tok = tok->next;

    if (!tok->has_space && equal(tok, "(")) {
        // Function-like macro}
        MacroParam *params = read_macro_param(&tok, tok->next);
        Macro *m = add_macro(name, false, copy_line(rest, tok));
        m->params = params;
    } else {
        // Object-like macro
        add_macro(name, true, copy_line(rest, tok));
    }
}

static MacroArg *read_macro_arg_one(Token **rest, Token *tok) {
    Token head = {};
    Token *cur = &head;
    int level = 0;

    while (level > 0 || (!equal(tok, ",") && !equal(tok, ")"))) {
        if (tok->kind == TK_EOF)
            error_tok(tok, "premature end of input");
        
        if (equal(tok, "("))
            level++;
        else if (equal(tok, ")"))
            level--;

        cur = cur->next = copy_token(tok);
        tok = tok->next;
    }

    cur->next = new_eof(tok);

    MacroArg *arg = calloc(1, sizeof(MacroArg));
    arg->tok = head.next;
    *rest = tok;
    return arg;
}

static MacroArg *read_macro_args(Token **rest, Token *tok, MacroParam *params) {
    Token *start = tok;
    tok = tok->next->next;

    MacroArg head = {};
    MacroArg *cur = &head;

    MacroParam *pp = params;
    for (; pp; pp = pp->next) {
        if (cur != &head)
            tok = skip(tok, ",");
        cur = cur->next = read_macro_arg_one(&tok, tok);
        cur->name = pp->name;
    }

    if (pp)
        error_tok(start, "too many argument");
    *rest = skip(tok, ")");
    return head.next;
}

static Token *find_arg(MacroArg *args, Token *tok) {
    for (MacroArg *ap = args; ap; ap = ap->next) {
        if (tok->len == strlen(ap->name) && !strncmp(tok->loc, ap->name, tok->len))
            return ap->tok;
    }
    return NULL;
}

static Token *subst(Token *tok, MacroArg *args) {
    Token head = {};
    Token *cur = &head;

    while (tok->kind != TK_EOF) {
        Token *arg = find_arg(args, tok);

        if (arg) {
            arg = preprocess2(arg);
            for (Token *t = arg; t->kind != TK_EOF; t = t->next)
                cur = cur->next = copy_token(t);
            tok = tok->next;
            continue;
        }

        cur = cur->next = copy_token(tok);
        tok = tok->next;
        continue;
    }

    cur->next = tok;
    return head.next;
}

static bool expand_macro(Token **rest, Token *tok) {
    if (hideset_contains(tok->hideset, tok->loc, tok->len))
        return false;

    Macro *m = find_macro(tok);
    if (!m)
        return false;

    if (m->is_objlike) {
        Hideset *hs = hideset_union(tok->hideset, new_hideset(m->name));
        Token *body = add_hideset(m->body, hs);
        *rest = append(body, tok->next);
        return true;
    }

    // funclikeマクロの後に引数が続かない場合、通常の識別子として扱う
    if (!equal(tok->next, "("))
        return  false;

    // func-like macro (no argument)
    MacroArg *args = read_macro_args(&tok, tok, m->params);
    *rest = append(subst(m->body, args), tok);
    return true;
}

static Token *skip_cond_incl2(Token *tok) {
    while (tok->kind != TK_EOF) {
        if (is_hash(tok) && 
                (equal(tok->next, "if") || equal(tok->next, "ifdef") ||
                 equal(tok->next, "ifndef"))) {
            tok = skip_cond_incl2(tok->next->next);
            continue;
        }
        if (is_hash(tok) && equal(tok->next, "endif"))
            return tok->next->next;
        tok = tok->next;
    }
    return tok;
}

// 次の`#endif`までスキップ
static Token *skip_cond_incl(Token *tok) {
    while (tok->kind != TK_EOF) {
        if (is_hash(tok) && 
                (equal(tok->next, "if") || equal(tok->next, "ifdef") ||
                 equal(tok->next, "ifndef"))) {
            tok = skip_cond_incl2(tok->next->next);
            continue;
        }
        if (is_hash(tok) && 
            (equal(tok->next, "else") || equal(tok->next, "endif") || equal(tok->next, "elif")))
                break;
        tok = tok->next;
    }
    return tok;
}

static Token *copy_line(Token **rest, Token *tok) {
    Token head = {};
    Token *cur = &head;

    for (; !tok->at_bol; tok = tok->next)
        cur = cur->next = copy_token(tok);

    cur->next = new_eof(tok);
    *rest = tok;
    return head.next;
}

static long eval_const_expr(Token **rest, Token *tok) {
    Token *expr = copy_line(rest, tok);
    expr = preprocess2(expr);

    Token *rest2;
    long val = const_expr(&rest2, expr);
    if (rest2->kind != TK_EOF)
        error_tok(rest2, "extra token");
    return val;
}

static CondIncl *push_cond_incl(Token *tok, bool included) {
    CondIncl *ci = calloc(1, sizeof(CondIncl));
    ci->next = cond_incl;
    ci->ctx = IN_THEN;
    ci->tok = tok;
    ci->included = included;
    cond_incl = ci;
    return ci;
}

static Token *preprocess2(Token *tok) {
    Token head = {};
    Token *cur = &head;

    while (tok->kind != TK_EOF) {
        if (expand_macro(&tok, tok))
            continue;

        if (!is_hash(tok)) {
            cur = cur->next = tok;
            tok = tok->next;
            continue;
        }

        Token *start = tok;
        tok = tok->next;

        if (equal(tok, "include")) {
            tok = tok->next;

            if (tok->kind != TK_STR)
                error_tok(tok, "expected a filename");

            char *path = tok->contents;
            Token *tok2 = tokenize_file(path);
            if (!tok2)
                error_tok(tok, "%s", strerror(errno));

            tok = skip_line(tok->next);
            tok = append(tok2, tok);
            continue;
        }

        if (equal(tok, "define")) {
            read_macro_definition(&tok, tok->next);
            continue;
        }

        if (equal(tok, "undef")) {
            tok = tok->next;
            if (tok->kind != TK_IDENT)
                error_tok(tok, "macro name must be an identifier");
            char *name = strndup(tok->loc, tok->len);
            tok = skip_line(tok->next);

            Macro *m = add_macro(name, true, NULL);
            m->deleted = true;
            continue;
        }

        if (equal(tok, "if")) {
            long val = eval_const_expr(&tok, tok->next);
            push_cond_incl(start, val);
            if (!val)
                tok = skip_cond_incl(tok);
            continue;
        }

        if (equal(tok, "ifdef")) {
            bool defined = find_macro(tok->next);
            push_cond_incl(tok, defined);
            tok = skip_line(tok->next->next);
            if (!defined)
                tok = skip_cond_incl(tok);
            continue;
        }

        if (equal(tok, "ifndef")) {
            bool defined = find_macro(tok->next);
            push_cond_incl(tok, !defined);
            tok = skip_line(tok->next->next);
            if (defined)
                tok = skip_cond_incl(tok);
            continue;
        }

        if (equal(tok, "elif")) {
            if (!cond_incl || cond_incl->ctx == IN_ELSE)
                error_tok(start, "stray #elif");
            cond_incl->ctx = IN_ELIF;

            if (!cond_incl->included && eval_const_expr(&tok, tok->next))
                cond_incl->included = true;
            else
                tok = skip_cond_incl(tok);
            continue;
        }

        if (equal(tok, "else")) {
            if (!cond_incl || cond_incl->ctx == IN_ELSE)
                error_tok(start, "stray #else");
            cond_incl->ctx = IN_ELSE;
            tok = skip_line(tok->next);

            if (cond_incl->included)
                tok = skip_cond_incl(tok);
            continue;
        }

        if (equal(tok, "endif")) {
            if (!cond_incl)
                error_tok(start, "stray #endif");
            cond_incl = cond_incl->next;
            tok = skip_line(tok->next);
            continue;
        }

        // 注意: `#`のみの行は有効(null directives)
        if (tok->at_bol)
            continue;

        error_tok(tok, "invalid preprocessor directive");
    }

    cur->next = tok;
    return head.next;
}

Token *preprocess(Token *tok) {
    tok = preprocess2(tok);
    if (cond_incl)
        error_tok(cond_incl->tok, "unterminated conditional directive");
    convert_keywords(tok);
    return tok;
}