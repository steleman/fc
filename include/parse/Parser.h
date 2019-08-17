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
#ifndef FC_PARSER_H
#define FC_PARSER_H

#include "ParserUtils.h"

#include "AST/ParseTreeBuilder.h"
#include "AST/ParserTreeCommon.h"
#include "AST/SymbolTable.h"
#include "common/Debug.h"
#include "common/Diagnostics.h"
#include "common/Source.h"
#include "lex/Lexer.h"
#include <deque>
#include <memory>

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/raw_ostream.h"

#include <stack>

namespace fc {

class Lexer;
class Diagnostics;
class ASTContext;

using namespace ast;

namespace parser {

class Parser {
  Lexer &L;
  Diagnostics &Diag;
  ASTContext &FC;
  std::unique_ptr<ParseTree> parseTree;
  std::deque<Token> bufferedTokens;
  ParseTreeBuilder builder;
  TokenKind currTokenKind;
  // This structure indicates the current context in
  // which the statement is being parsed.
  struct Context {
    llvm::StringRef currPU;
    SymbolTable *currSymTable;
    SymbolTable *globalSymTable;
    SymbolTableList usedSymTableList;
    StmtList *currStmtList{nullptr};

    // Used iff the symtab for the symbol being parsed is unknown
    // (eg. in case of a part-ref of a struct-comp whose DTD is not in
    // current-PU)
    SymbolTable *anonSymTab{nullptr};

    SpecificationPart *currSpecPart;

    bool isCurrUnitNested() const {
      auto parent = currSymTable->getParent();
      assert(parent);
      return parent->isFunctionScope() || parent->isSubroutineScope() ||
             parent->isMainProgramScope();
    }
  };
  Context context;

public:
  explicit Parser(ASTContext &FC, Lexer &L, Diagnostics &Diag);

  Parser(Parser &) = delete;
  Parser(const Parser &) = delete;

private:
  void consumeToken(TokenKind kind = tok::unknown);

  // returns the nth token in the look-ahead buffer. *Note* that n = 0 returns
  // the 0th look-ahead token which is the token immediately after what
  // Parser::Tok is pointing to.
  Token &lookAhead(unsigned n) {
    assert(n < bufferedTokens.size());
    return bufferedTokens[n];
  }

  bool isKeyWordIdentifer(Token Tok);

  bool isKeyWordIntrinsic(Token Tok);

  bool expect(TokenKind kind);

  bool expect(TokenKind kind, diag::ErrorKind errorKind);

  bool expect(llvm::ArrayRef<TokenKind> tokens);

  bool isOneOf(llvm::ArrayRef<TokenKind> arr) const;

  // look ahead match.
  bool match(llvm::ArrayRef<TokenKind> tokens);

  Module *parseModule();

  Function *parseFunction(Type *returnType = nullptr);

  // Specification part.
  SpecificationPart *parseSpecification();

  void parseTypeDeclStmt(StmtList &list);

  ArraySpec *parseArraySpec();

  AttrSpec *parseAttributeSpec(SymbolAttributes &attributes);

  EntityDecl *parseEntityDecl(DeclarationTypeSpec *decl, ArraySpec *arraySpec,
                              llvm::SmallVector<AttrSpec *, 2> &attrSpecList,
                              bool hasDoubleColon, SymbolAttributes &attributes,
                              ArraySpec *charArraySpec);
  DeclarationTypeSpec *parseDeclTypeSpec();

  void parseParameterStmt();

  ExecutionPart *parseExecutionPart();

  bool parseExecutableStmtList(StmtList &stmtList);

  Stmt *parseAssignmentStmt();

  CycleStmt *parseCycleStmt();

  ExitStmt *parseExitStmt();

  ReturnStmt *parseReturnStmt();

  std::tuple<Expr *, Block *> parseIfStmt(bool &hasThen, bool isElseIf = false);

  IfElseStmt *parseIfElseConstruct();

  Format *parseFormat(TokenKind kind);

  WhereStmt *parseWhereStmt(Expr *expr, SourceLoc loc, bool &isSingleStmt);

  WhereConstruct *parseWhereConstruct();

  Block *parseBlock();

  DoWhileStmt *parseDoWhileStmt();

  DoStmt *parseDoStmt(llvm::StringRef name, SourceLoc &loc);

  ForAllStmt *parseForAllStmt(llvm::StringRef name);

  Expr *parseExpr(bool consumeCurrent = true);

  bool parseKeywordActionStmt(StmtList &stmtList, llvm::StringRef name);

  bool parseActionStmt(StmtList &stmtList, bool consumeNext = true);

  StopStmt *parseStopStmt();

  PrintStmt *parsePrintStmt();

  ReadStmt *parseReadStmt();

  WriteStmt *parseWriteStmt();

  OpenStmt *parseOpenStmt();

  CloseStmt *parseCloseStmt();

  UseStmt *parseUseStmt();

  CallStmt *parseCallStmt();

  NullifyStmt *parseNullifyStmt();

  AllocateStmt *parseAllocateStmt();

  DeAllocateStmt *parseDeAllocateStmt();

  Expr *parseRangeExpr(llvm::ArrayRef<TokenKind> endToks, bool &hasRange);

  ExprList parseExprList(TokenKind endTok, bool &listHasRange,
                         bool consume = true);

  QuadExpr *parseACImpliedDo();
  QuadExpr *parseImpliedDo();

  ArrayConstructor *parseArrayConstructor();

  SelectCaseStmt *parseSelectCaseStmt();

  CaseStmt *parseCaseStmt();

  ArgsList parseArgsList(TokenKind endTok); // Parser helpers

  Expr *checkAndBuildExpr(Expr *lhs, Expr *rhs, TokenKind tokKind, Type *type);

  Symbol *getSymbolFor(llvm::StringRef name, SourceLoc loc);

  const Token &getToken() { return bufferedTokens[0]; }

  constexpr bool is(TokenKind kind) const { return currTokenKind == kind; }

  constexpr bool isNot(TokenKind kind) const { return currTokenKind != kind; }

  SourceLoc getCurrLoc() { return bufferedTokens[0].loc; }

  // Check if the current token is identifier.
  llvm::StringRef checkIdentifier();

  // Lex the current token and check if identifier.
  llvm::StringRef expectIdentifier(bool consumeCurrent = true);

  TokenKind expectOneOf(llvm::ArrayRef<TokenKind> &tokens,
                        bool consumeCurrent = true);
  Type *expectConstant(bool consumeCurrent = true);

  AttrSpecKind expectAttrSpecKind(bool consumeCurrent = true);

  ProgramUnit *parseInternalSubprogram();

  // allowFail is allow to fail on different token.
  Type *getTypeFor(TokenKind kind, int size, bool allowFail = false);

  ArraySpec *updateArraySpec(ArraySpec *arraySpec, ArraySpec *charSpec);

  void pushOperation(std::stack<Expr *> &valueStack,
                     std::stack<TokenKind> &opsStack, SourceLoc loc);

  Expr *parseExprOperand();

  Expr *_parseExprOperand(SymbolTable *DTSymTable = nullptr);

  void parseTypeAttributeSpec();

  DerivedTypeDef *parseDerivedTypeDef();

public:
  std::unique_ptr<ParseTree> &getTree() { return parseTree; }

  std::string dumpParseTree(llvm::raw_ostream &OS);

  bool parseProgram();
};
} // namespace parser
} // namespace fc
#endif
