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
#ifndef FC_LEXER_H
#define FC_LEXER_H

#include "lex/Token.h"
#include <string>

namespace fc {
struct TokenInfo;
class Lexer {
private:
  SourceLoc loc;
  std::string filename;
  // const char *start;
  const char *end;
  const char *currPtr;
  Token lastToken;
  Standard std;

public:
  bool eofSeen;
  explicit Lexer(std::string filename, Standard std, SourceLoc Loc,
                 const char *startBuffer, const char *endBuffer)
      : loc(Loc), filename(filename), end(endBuffer), currPtr(startBuffer),
        std(std), eofSeen(false) {}

  Lexer(Lexer &) = delete;
  Lexer(const Lexer &) = delete;

  Token getNextToken();

  inline void removeHorizontalSpaces() {
    while (currPtr != end && ((currPtr[0] == ' ') || (currPtr[0] == '\t')))
      ++currPtr;
  }
  inline char getCurrChar(bool ignoreHorizontalSpace = false) {

    if (ignoreHorizontalSpace)
      removeHorizontalSpaces();

    if (currPtr != end)
      return currPtr[0];

    return 0;
  }

  TokenInfo getNumericalConstant(unsigned &Size);

  // Try to parse alphanumeric values.
  TokenInfo getNextWord(unsigned &Size, bool ignoreLeadingSpace = false);
};
} // namespace fc

#endif
