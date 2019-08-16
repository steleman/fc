#ifndef FC_TOKEN_KINDS_H
#define FC_TOKEN_KINDS_H

#include "llvm/ADT/StringSwitch.h"

namespace fc {

namespace tok {

enum TokenKind {
#define TOKEN(X) X,
#include "lex/TokenKinds.def"
  NUM_TOKENS
};

namespace utils {
TokenKind isKeyword(llvm::StringRef ref);

bool isKeywordToken(TokenKind kind);

const char *printTokenName(TokenKind kind);

bool isBinaryOp(TokenKind kind);

bool isRelationalOp(TokenKind kind);

bool isLogicalOp(TokenKind kind);

bool isOp(TokenKind kind);

bool isConstant(TokenKind kind);

bool isBlockEnd(TokenKind kind);
} // namespace utils

} // namespace tok

} // namespace fc
#endif