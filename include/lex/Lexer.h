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
