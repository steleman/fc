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
#include "lex/Lexer.h"
#include "common/Debug.h"
#include "lex/Token.h"

using namespace fc;
using namespace tok;
using namespace utils;

namespace fc {
struct TokenInfo {
  bool hasDigits;
  bool hasAlphabet;
  bool hasUnderscore;
  bool hasDot;
  bool hasTypeHint;
  int typeSize;

  TokenInfo()
      : hasDigits(false), hasAlphabet(false), hasUnderscore(false),
        hasDot(false), hasTypeHint(false), typeSize(-1) {}
};
} // namespace fc

static void update(SourceLoc &loc, const char *currPtr,
                   bool isNewLine = false) {
  if (isNewLine) {
    loc.Line++;
    loc.currStartOfLine = currPtr;
    loc.Col = 1;
  } else if (loc.currStartOfLine) {
    loc.Col = (currPtr - loc.currStartOfLine);
    if (loc.Line == 1)
      loc.Col += 1;
  } else {
    loc.currStartOfLine = currPtr;
  }
}

// Try to parse alphanumeric values.
TokenInfo Lexer::getNextWord(unsigned &Size, bool ignoreLeadingSpace) {
  TokenInfo Info;
  if (ignoreLeadingSpace)
    removeHorizontalSpaces();

  while (1) {
    char C = getCurrChar(false);
    if (C == 0)
      return Info;
    if ((C >= 'A' && C <= 'Z') || (C >= 'a' && C <= 'z')) {
      Size++;
      Info.hasAlphabet = true;
    } else if (C >= '0' && C <= '9') {
      Size++;
      Info.hasDigits = true;
    } else if (C == '_') {
      Size++;
      Info.hasUnderscore = true;
    } else {
      return Info;
    }
    ++currPtr;
  }
}

TokenInfo Lexer::getNumericalConstant(unsigned &Size) {
  TokenInfo Info;
  char C = getCurrChar(false);

  // Check if numerical constant has sign ahead.
  if ((C == '+' || C == '-')) {
    if (!std::isdigit(currPtr[1]))
      return Info;
    Size++;
    ++currPtr;
  }

  while (1) {
    C = getCurrChar(false);
    if (std::isdigit(C)) {
      Size++;
      Info.hasDigits = true;
    } else if (C == '.') {
      Size++;
      Info.hasDot = true;
    } else if ((C == 'D' || C == 'd') &&
               (currPtr[1] == '+' || currPtr[1] == '-')) {
      currPtr += 2;
      Size += 2;
      continue;
    } else if (C == 'd' && std::isdigit(currPtr[1])) {
      // Should be double.
      currPtr += 2;
      Size += 2;
      continue;
      Info.typeSize = 8;
    } else if ((C == 'E' || C == 'e') &&
               (currPtr[1] == '-' || currPtr[1] == '+' ||
                isdigit(currPtr[1]))) {

      currPtr += 2;
      Size += 2;
      while (isdigit(currPtr[0])) {
        currPtr++;
        Size++;
      }
      continue;
    } else {
      if (C == '_') {
        if (currPtr[1] == '1' && currPtr[2] == '6') {
          Info.hasTypeHint = true;
          Info.hasUnderscore = true;
          Info.typeSize = 16;
          currPtr += 3;
        } else if (currPtr[1] == '2' || currPtr[1] == '4' ||
                   currPtr[1] == '8') {
          Info.hasTypeHint = true;
          Info.hasUnderscore = true;
          Info.typeSize = currPtr[1] - '0';
          currPtr += 2;
        } else if (isalpha(currPtr[1])) {
          Info.hasTypeHint = true;
          Info.hasUnderscore = true;
          currPtr++;
          while (currPtr[0] == '_' || isalpha(currPtr[0]) ||
                 isdigit(currPtr[0]))
            currPtr++;
        }
      }
      return Info;
    }
    ++currPtr;
  }
}

static bool isValidLHS(Token &Tok) {
  if (Tok.Kind == tok::identifier)
    return true;
  // TODO: temporary fix for - 1 vs -1 issue.
  if (Tok.Kind == tok::r_paren)
    return true;
  if (isConstant(Tok.Kind))
    return true;
  return false;
}

Token Lexer::getNextToken() {
  Token Tok;
  removeHorizontalSpaces();

  unsigned Size = 0; // Size of current identifier.
  const char *Base = currPtr;
  update(loc, currPtr, false);

  TokenInfo Info;
  auto Curr = getCurrChar(true);

  // Probably numerical constant. Identifiers
  // cannot start with digit.
  if (std::isdigit(Curr) ||
      ((Curr == '+' || Curr == '-') && !isValidLHS(lastToken))) {
    Info = getNumericalConstant(Size);
  } else {
    Info = getNextWord(Size, true);
  }

  if (Size > 0) {

    TokenKind kind;
    int typeSize = -1;

    if (!Info.hasAlphabet && Info.hasDigits) {
      // Numerical constant.
      if (Info.hasDot) {
        kind = tok::real;
        if (Info.typeSize != -1)
          typeSize = Info.typeSize;
      } else
        kind = tok::integer;
      if (Info.hasUnderscore && Info.hasTypeHint) {
        // assert(Info.typeSize != -1);
        typeSize = Info.typeSize;
      }
    } else if (Info.hasAlphabet && Info.hasDigits) {
      // indentifier.
      kind = tok::identifier;
    } else if (Info.hasAlphabet && !Info.hasDigits) {
      // can be a keyword.
      llvm::StringRef Ident{Base, Size};
      // Fortran handles it as lower case letters.
      Ident = llvm::StringRef(Ident.lower());
      kind = isKeyword(Ident);
    } else {
      llvm_unreachable("unknown token");
    }

    Tok.Kind = (kind);
    Tok.value = (Base);
    if (typeSize != -1) {
      Tok.constantTypeSize = (typeSize);
    }
    Tok.size = (Size);
    lastToken = Tok;
    Tok.loc = loc;
    return Tok;
  }

  // Look for other symbols.
  char C = getCurrChar(true);

  // Check if they are logical literal constants.
  if (Curr == '.') {
    // TODO: optimize
    llvm::StringRef trueStr{currPtr, 6};
    if (trueStr.equals_lower(".true.")) {
      Tok.Kind = (tok::logical);
      Tok.value = (currPtr);
      Tok.size = (6);
      currPtr += 6;
      update(loc, currPtr, false);
      lastToken = Tok;
      Tok.loc = loc;
      return Tok;
    }

    llvm::StringRef falseStr{currPtr, 7};
    if (falseStr.equals_lower(".false.")) {
      Tok.Kind = (tok::logical);
      Tok.value = (currPtr);
      Tok.size = (7);
      currPtr += 7;
      update(loc, currPtr, false);
      lastToken = Tok;
      Tok.loc = loc;
      return Tok;
    }

    llvm::StringRef logicalOp{currPtr, 5};
    TokenKind kind = llvm::StringSwitch<TokenKind>(logicalOp.lower())
                         .Case(".and.", tok::l_and)
                         .Case(".not.", tok::l_not)
                         .Case(".eqv.", tok::l_eqv)
                         .Default(tok::undefined);

    if (kind != tok::undefined) {
      Tok.Kind = (kind);
      Tok.value = (currPtr);
      Tok.size = (5);
      currPtr += 5;
      update(loc, currPtr, false);
      lastToken = Tok;
      Tok.loc = loc;
      return Tok;
    }

    llvm::StringRef orOp{currPtr, 4};
    if (orOp.equals_lower(".or.")) {
      Tok.Kind = (tok::l_or);
      Tok.value = (currPtr);
      Tok.size = (4);
      currPtr += 4;
      update(loc, currPtr, false);
      lastToken = Tok;
      Tok.loc = loc;
      return Tok;
    }

    llvm::StringRef neqOp{currPtr, 6};
    if (neqOp.equals_lower(".neqv.")) {
      Tok.Kind = (tok::l_neqv);
      Tok.value = (currPtr);
      Tok.size = (6);
      currPtr += 6;
      update(loc, currPtr, false);
      lastToken = Tok;
      Tok.loc = loc;
      return Tok;
    }
  }

  switch (C) {
  case 0:
    Tok.Kind = (tok::eof);
    eofSeen = true;
    ++currPtr;
    break;
  case '\n':
  case '\r': {

    if (std == f77) {
      // consume the new line.
      currPtr++;
      update(loc, currPtr, true);
      if (currPtr[0] == 'c' || currPtr[0] == 'C' || currPtr[0] == '!') {
        // Ignore the characters till next new line.
        while (currPtr != end && currPtr[0] != '\n')
          currPtr++;
        return getNextToken();
      }

      removeHorizontalSpaces();
      update(loc, currPtr, false);

      if ((currPtr[0] == '*' || std::isdigit(currPtr[0]) ||
           currPtr[0] == '$') &&
          loc.Col == 5) {
        // consume the next character.
        currPtr++;
        update(loc, currPtr, false);
        return getNextToken();
      }

      if (currPtr[0] == '\n') {
        return getNextToken();
      }

      Tok.Kind = (tok::eol);
      return Tok;
    }

    while (currPtr != end && currPtr[0] == '\n') {
      update(loc, currPtr, true);
      currPtr++;
      removeHorizontalSpaces();
    }
    Tok.Kind = (tok::eol);
    break;
  }
  case ',':
    Tok.Kind = (tok::comma);
    update(loc, currPtr);
    ++currPtr;
    break;
  case ':':
    Tok.Kind = (tok::colon);
    update(loc, currPtr);
    ++currPtr;
    break;
  case ';':
    Tok.Kind = tok::semicolon;
    ++currPtr;
    update(loc, currPtr);
    break;
  case '=':
    if (*(currPtr + 1) == '=') {
      Tok.Kind = (tok::eq);
      update(loc, currPtr);
      currPtr += 2;
      break;

    } else if (*(currPtr + 1) == '>') {
      Tok.Kind = (tok::ptr_assign);
      update(loc, currPtr);
      currPtr += 2;
      break;
    }

    Tok.Kind = (tok::equals);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '(':
    if (currPtr[1] == '/') {
      Tok.Kind = tok::arr_start;
      update(loc, currPtr);
      currPtr += 2;
      break;
    }
    Tok.Kind = (tok::l_paren);
    update(loc, currPtr);
    ++currPtr;
    break;
  case ')':
    Tok.Kind = (tok::r_paren);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '+':
    Tok.Kind = (tok::plus);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '-':
    Tok.Kind = (tok::minus);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '%':
    Tok.Kind = (tok::percent);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '/':
    if (currPtr[1] == ')') {
      Tok.Kind = tok::arr_end;
      update(loc, currPtr);
      currPtr += 2;
      break;
    }
    if (*(currPtr + 1) == '=') {
      Tok.Kind = (tok::ne);
      update(loc, currPtr);
      currPtr += 2;
      break;
    }

    if (currPtr[1] == '/') {
      Tok.Kind = tok::concat;
      update(loc, currPtr);
      currPtr += 2;
      break;
    }
    Tok.Kind = (tok::division);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '*':
    if (*(currPtr + 1) == '*') {
      Tok.Kind = (tok::power);
      update(loc, currPtr);
      currPtr += 2;
      break;
    }
    Tok.Kind = (tok::star);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '<': // TODO Generalise the repeated code
    if (*(currPtr + 1) == '=') {
      Tok.Kind = (tok::le);
      update(loc, currPtr);
      currPtr += 2;
      break;
    }
    Tok.Kind = (tok::lt);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '>':
    if (*(currPtr + 1) == '=') {
      Tok.Kind = (tok::ge);
      update(loc, currPtr);
      currPtr += 2;
      break;
    }
    Tok.Kind = (tok::gt);
    update(loc, currPtr);
    ++currPtr;
    break;
  case '.':
    // TODO : Need a more generic way to ignore case. Parse ignore case?
    if (strncmp(currPtr, ".eq.", 4) == 0 || strncmp(currPtr, ".EQ.", 4) == 0) {
      Tok.Kind = (tok::eq);
    } else if (strncmp(currPtr, ".ne.", 4) == 0 ||
               strncmp(currPtr, ".NE.", 4) == 0) {
      Tok.Kind = (tok::ne);
    } else if (strncmp(currPtr, ".lt.", 4) == 0 ||
               strncmp(currPtr, ".LT.", 4) == 0) {
      Tok.Kind = (tok::lt);
    } else if (strncmp(currPtr, ".le.", 4) == 0 ||
               strncmp(currPtr, ".LE.", 4) == 0) {
      Tok.Kind = (tok::le);
    } else if (strncmp(currPtr, ".gt.", 4) == 0 ||
               strncmp(currPtr, ".GT.", 4) == 0) {
      Tok.Kind = (tok::gt);
    } else if (strncmp(currPtr, ".ge.", 4) == 0 ||
               strncmp(currPtr, ".GE.", 4) == 0) {
      Tok.Kind = (tok::ge);
    } else {
      error() << "\nFound character: \"" << *(currPtr + 1) << "\" \n";
      llvm_unreachable("unhandled token with . ");
    }
    update(loc, currPtr);
    currPtr += 4;
    break;
  case '\'':
  case '"': {
    int j = 1;
    while (true) {
      // Escape character is same as the character specifier.
      if (*(currPtr + j) == C && *(currPtr + j + 1) != C)
        break;
      j++;
    }
    j--;
    // character token
    if (j == 1) {
      Tok.Kind = (tok::character);
      update(loc, currPtr);
      Tok.value = (currPtr + 1);
      Tok.size = (j);
      currPtr += 3;
      break;
    }
    Tok.Kind = (tok::string);
    Tok.value = (currPtr + 1);
    Tok.size = (j);
    currPtr += j + 2;
    update(loc, currPtr);
    break;
  }

  // Handle comments
  case '!': {

    bool isNewLine = false;
    if (lastToken.Kind == tok::eol)
      isNewLine = true;
    while (currPtr != end && currPtr[0] != '\n' && currPtr[0] != '\r') {
      ++currPtr;
    }
    if (isNewLine) {
      update(loc, currPtr, true);
      ++currPtr;
    } else {
      update(loc, currPtr, false);
    }
    Tok = getNextToken();
    if (!isNewLine) {
      lastToken = Tok;
      Tok.loc = loc;
      return Tok;
    }
    while (Tok.Kind == tok::eol) {
      Tok = getNextToken();
    }
    lastToken = Tok;
    Tok.loc = loc;
    return Tok;
  }
  case '&': {

    // ignore the white spaces
    while (currPtr[0] != '\n' && currPtr[0] != '\r') {
      ++currPtr;
    }

    // ignore new line
    update(loc, currPtr, true);
    ++currPtr;
    lastToken.Kind = tok::eol;
    Tok = getNextToken();
    lastToken = Tok;
    Tok.loc = loc;
    return Tok;
  }

  default:
    error() << "\nFound character: \"" << C << "\" \n";
    llvm_unreachable("some other token");
  }
  lastToken = Tok;
  Tok.loc = loc;
  return Tok;
}
