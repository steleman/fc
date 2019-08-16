#include "parse/ParserUtils.h"
#include "llvm/Support/Casting.h"

using namespace fc;
using namespace parser;

bool fc::parser::isIntrinsicTypeSpec(TokenKind kind) {
  switch (kind) {
  case tok::kw_integer:
  case tok::kw_real:
  case tok::kw_double:
  case tok::kw_precision:
  case tok::kw_complex:
  case tok::kw_character:
  case tok::kw_logical:
    return true;
  default:
    return false;
  };
}

Type *fc::parser::getBaseType(Type *type) {
  if (auto arrayTy = llvm::dyn_cast<ArrayType>(type))
    return arrayTy->getElementTy();
  return type;
}
