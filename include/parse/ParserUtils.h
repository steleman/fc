#ifndef FC_PARSERUTILS_H
#define FC_PARSERUTILS_H

#include "AST/Type.h"
#include "lex/Token.h"

namespace fc {

class Type;

namespace parser {

bool isIntrinsicTypeSpec(TokenKind kind);
Type *getBaseType(fc::Type *type);
} // end of namespace parser
} // end of namespace fc
#endif
