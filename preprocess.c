#include "kcc.h"

Token *preprocess(Token *tok) {
    convert_keywords(tok);
    return tok;
}
