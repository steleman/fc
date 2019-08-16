#include "lex/TokenKinds.h"
using namespace fc;

namespace fc {
namespace tok {
namespace utils {
TokenKind isKeyword(llvm::StringRef ref) {
  return llvm::StringSwitch<TokenKind>(ref)
#define KEYWORD(X) .Case(#X, tok::kw_##X)
#include "lex/TokenKinds.def"
      .Default(identifier);
}

bool isKeywordToken(TokenKind kind) {
  switch (kind) {
#define KEYWORD(X) case tok::kw_##X:
#include "lex/TokenKinds.def"
    return true;
  default:
    return false;
  };
}

const char *printTokenName(TokenKind kind) {
  switch (kind) {
#define TOKEN(X)                                                               \
  case tok::X:                                                                 \
    return "tok::" #X;
#include "lex/TokenKinds.def"
  default:
    return "";
  };
}

bool isBinaryOp(TokenKind kind) {
  switch (kind) {
  default:
    return false;
  case tok::plus:
  case tok::minus:
  case tok::division:
  case tok::star:
  case tok::power:
  case tok::concat:
  case tok::unary_minus:
    return true;
  }
}

bool isRelationalOp(TokenKind kind) {
  switch (kind) {
  default:
    return false;
  case tok::eq:
  case tok::ne:
  case tok::gt:
  case tok::ge:
  case tok::lt:
  case tok::le:
    return true;
  }
}

bool isLogicalOp(TokenKind kind) {
  switch (kind) {
  default:
    return false;
  case tok::l_and:
  case tok::l_or:
  case tok::l_not:
  case tok::l_eqv:
  case tok::l_neqv:
    return true;
  }
}

bool isOp(TokenKind kind) {
  if (isBinaryOp(kind) || isRelationalOp(kind) || isLogicalOp(kind))
    return true;
  return false;
}

bool isConstant(TokenKind kind) {
  switch (kind) {
  default:
    return false;
  case tok::integer:
  case tok::real:
  case tok::complex:
  case tok::character:
  case tok::logical:
  case tok::binary:
  case tok::octal:
  case tok::hexadecimal:
  case tok::string:
    return true;
  }
}

// Check
bool isBlockEnd(TokenKind kind) {
  switch (kind) {
  default:
    return false;
  case tok::kw_end:
  case tok::kw_enddo:
  case tok::kw_else:
  case tok::kw_elseif:
  case tok::kw_endif:
  case tok::kw_contains:
  case tok::kw_elsewhere:
  case tok::kw_case:
  case tok::kw_default:
    return true;
  }
}

} // namespace utils
} // namespace tok
} // namespace fc
