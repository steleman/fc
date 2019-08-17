// Copyright (c) 2019, Compiler Tree Technologies Pvt Ltd.
// All rights reserved.
//
// Redistribution and use in source and binary forms, with or without
// modification, are permitted provided that the following conditions are met:
//
// 1. Redistributions of source code must retain the above copyright notice, this
//    list of conditions and the following disclaimer.
//
// 2. Redistributions in binary form must reproduce the above copyright notice,
//    this list of conditions and the following disclaimer in the documentation
//    and/or other materials provided with the distribution.
//
// THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
// AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
// IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
// DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
// FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
// DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
// SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
// CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
// OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
// OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#include "parse/Parser.h"

using namespace fc;
using namespace parser;

bool Parser::isKeyWordIdentifer(Token Tok) {
  if (!utils::isKeywordToken(Tok.Kind))
    return false;
  /*
  if (!context.currSymTable->isSymbolDeclared(Tok.getRef()))
    return false;
  */
  return true;
}

bool Parser::isKeyWordIntrinsic(Token Tok) {
  if (!utils::isKeywordToken(Tok.Kind))
    return false;
  switch (Tok.Kind) {
  default:
    return false;
  case tok::kw_real:
    return true;
  }
  return false;
}

// allowFail is allow to fail on different token.
Type *Parser::getTypeFor(TokenKind kind, int size, bool allowFail) {
  switch (kind) {
  case tok::kw_integer:
  case tok::integer: {
    switch (size) {
    case 2:
      return Type::getInt16Ty(FC);
    case 4:
    case -1:
      return Type::getInt32Ty(FC);
    case 8:
      return Type::getInt64Ty(FC);
    default:
      if (!allowFail) {
        llvm_unreachable("Type not handled.");
      }
      return nullptr;
    };
  case tok::kw_real:
  case tok::real: {
    switch (size) {
    case 4:
    case -1:
      return Type::getRealTy(FC);
    case 8:
      return Type::getDoubleTy(FC);
    default:
      if (!allowFail) {
        llvm_unreachable("Type not handled.");
      }
      return nullptr;
    };
  }
  case tok::kw_double: {
    return Type::getDoubleTy(FC);
  }
  case tok::kw_logical:
  case tok::logical: {
    return Type::getLogicalTy(FC);
  }
  case tok::kw_character: {
    return Type::getCharacterTy(FC);
  }
  case tok::character:
    return Type::getCharacterTy(FC);

  case tok::kw_complex: {
    // Default complex type is 4 + 4 bytes.
    return ComplexType::get(FC, (size == -1) ? 4 : size);
  }

  case tok::string: {
    switch (size) {
    case -1:
      return ArrayType::get(FC, Type::getStringCharTy(FC), 1);
    default:
      ArrayBounds bounds;
      bounds.first = 0;
      bounds.second = size;
      ArrBoundsList list;
      list.push_back(bounds);
      return ArrayType::get(FC, Type::getStringCharTy(FC), list);
    };
  }

  default:
    if (!allowFail) {
      llvm_unreachable("Type not handled.");
    }
    return nullptr;
  };
  };
  return nullptr;
}

// Consume the current token and get the next one.
void Parser::consumeToken(TokenKind kind) {

  assert((kind == tok::unknown || kind == currTokenKind) &&
         " consuming wrong token?");
  assert(!bufferedTokens.empty());
  bufferedTokens.pop_front();

  if (!L.eofSeen) {
    bufferedTokens.push_back(L.getNextToken());
  }
  currTokenKind = bufferedTokens[0].Kind;
}

// Expect next token to be of Kind.
// current one is consumed by default.
bool Parser::expect(TokenKind kind) {
  consumeToken();
  return is(kind);
}

// Expect the next token to be of kind.
// If not print dignostics
bool Parser::expect(TokenKind kind, diag::ErrorKind errorKind) {
  consumeToken();
  if (is(kind))
    return true;
  Diag.printError(getCurrLoc(), errorKind);
  return false;
}

bool Parser::expect(llvm::ArrayRef<TokenKind> tokens) {
  for (auto tok : tokens) {
    if (!expect(tok))
      return false;
  }
  return true;
}

TokenKind Parser::expectOneOf(llvm::ArrayRef<TokenKind> &tokens,
                              bool consumeCurrent) {
  if (consumeCurrent)
    consumeToken();
  for (auto token : tokens) {
    if (is(token)) {
      return token;
    }
  }
  return tok::unknown;
}

// look-ahead match without current token.
bool Parser::match(llvm::ArrayRef<TokenKind> tokens) {
  assert(!bufferedTokens.empty());
  assert(tokens.size() < bufferedTokens.size());
  unsigned I = 1;
  for (auto token : tokens) {
    if (bufferedTokens[I].Kind != token) {
      return false;
    }
    I++;
  }
  return true;
}

// consumes current token and checks if the next one
// is a Constant.
Type *Parser::expectConstant(bool consumeCurrent) {
  if (consumeCurrent)
    consumeToken();

  return getTypeFor(currTokenKind, getToken().constantTypeSize, true);
}

bool Parser::isOneOf(llvm::ArrayRef<TokenKind> arr) const {
  for (auto tok : arr) {
    if (currTokenKind == tok)
      return true;
  }
  return false;
}

// consumes current token and checks if the next one
// is a AttrSpecKind.
AttrSpecKind Parser::expectAttrSpecKind(bool consumeCurrent) {

  if (consumeCurrent)
    consumeToken();

  switch (currTokenKind) {
  case tok::kw_dimension:
    return AttrSpecKind::Dimension;
  case tok::kw_allocatable:
    return AttrSpecKind::Allocatable;
  case tok::kw_parameter:
    return AttrSpecKind::Parameter;
  case tok::kw_intent:
    return AttrSpecKind::Intent;
  case tok::kw_save:
    return AttrSpecKind::Save;
  case tok::kw_private:
    return AttrSpecKind::Private;
  case tok::kw_optional:
    return AttrSpecKind::Optional;
  case tok::kw_public:
    return AttrSpecKind::Public;
  case tok::kw_pointer:
    return AttrSpecKind::Pointer;
  case tok::kw_target:
    return AttrSpecKind::Target;
  default:
    llvm_unreachable("Unknown attr spec kind");
  };
}

// lex the next token and check if it
// is identifier.
llvm::StringRef Parser::expectIdentifier(bool consumeCurrent) {
  if (consumeCurrent)
    consumeToken();
  return checkIdentifier();
}

llvm::StringRef Parser::checkIdentifier() {
  // Get the identifier name.
  // Treat keywords which appear in the place of identifiers as identifers.
  if (isNot(tok::identifier) && !utils::isKeywordToken(currTokenKind)) {
    Diag.printError(getCurrLoc(), diag::ident_err1);
    return llvm::StringRef();
  }

  return getToken().getRef();
}

Symbol *Parser::getSymbolFor(llvm::StringRef name, SourceLoc loc) {
  if (!context.currSymTable->isSymbolDeclared(name)) {
    SymbolAttributes attr;
    return context.currSymTable->addNewSymbol(name, Type::getUndeclaredTy(FC),
                                              loc, attr);
  }

  return context.currSymTable->getSymbol(name);
}
