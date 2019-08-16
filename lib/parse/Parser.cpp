#include "parse/Parser.h"
#include "AST/ASTContext.h"
#include "AST/ProgramUnit.h"

using namespace fc;
using namespace parser;

#define MAX_LOOKAHEAD 3

std::string Parser::dumpParseTree(llvm::raw_ostream &OS) {
  return parseTree->dump(OS);
}

Parser::Parser(ASTContext &FC, Lexer &L, Diagnostics &Diag)
    : L(L), Diag(Diag), FC(FC), builder(FC) {
  //  Tokens lexed are always MAX_LOOKAHEAD ahead of current token being parsed
  //  except during EOF.
  context.anonSymTab =
      SymbolTable::get(FC, ScopeKind::GlobalScope, "anon@3.14159", nullptr);
  context.anonSymTab->setAnon(true);

  for (unsigned i = 0; i < MAX_LOOKAHEAD + 1; i++) {
    bufferedTokens.push_back(L.getNextToken());
    if (L.eofSeen) {
      break;
    }
  }
  currTokenKind = bufferedTokens[0].Kind;
}

ArgsList Parser::parseArgsList(TokenKind endTok) {
  ArgsList exprList;

  // consume current.
  consumeToken();

  while (isNot(endTok)) {

    llvm::StringRef name = getToken().getRef();
    exprList.push_back(name);
    consumeToken();
    if (is(tok::comma)) {
      consumeToken();
      continue;
    }

    if (is(endTok)) {
      break;
    }
    assert(false && "unknown token encountered!");
  }
  return exprList;
}

bool Parser::parseExecutableStmtList(StmtList &stmtList) {
  while (!utils::isBlockEnd(currTokenKind)) {
    if (parseActionStmt(stmtList))
      continue;
    llvm_unreachable("Unhandled executable construct");
  }
  return true;
}

ExecutionPart *Parser::parseExecutionPart() {
  if (is(tok::kw_end) || is(tok::kw_contains))
    return nullptr;

  StmtList stmtList;
  auto parentList = context.currStmtList;
  context.currStmtList = &stmtList;
  if (!parseExecutableStmtList(stmtList)) {
    return nullptr;
  }

  auto block = builder.buildBlock(stmtList, getCurrLoc());
  context.currStmtList = parentList;
  return builder.buildExecutionPart(block);
}

Module *Parser::parseModule() {
  assert(is(tok::kw_module));

  // Parse the begin token.
  // syntax :  program program-name
  auto name = expectIdentifier();
  if (name.empty()) {
    return nullptr;
  }
  SymbolAttributes attr;
  context.globalSymTable->addNewSymbol(name, Type::getModueTy(FC), getCurrLoc(),
                                       attr);
  SymbolTable *newSymTable = builder.buildSymbolTable(
      FC, ScopeKind::ModuleScope, name, context.currSymTable);
  context.currSymTable = newSymTable;
  context.currPU = name;
  context.currStmtList = nullptr;

  // Expect EOl after statement.
  assert(expect(tok::eol));

  consumeToken();

  SpecificationPart *specPart = nullptr;
  specPart = parseSpecification();

  if (Diag.hasError())
    return nullptr;

  ProgramUnitList PUList;

  // Ignore private and public memebers for now!
  while (is(tok::kw_private) || is(tok::kw_public)) {
    consumeToken();
    while (isNot(tok::eol)) {
      consumeToken();
    }
    consumeToken(tok::eol);
  }

  assert(is(tok::kw_end) || is(tok::kw_contains));

  if (is(tok::kw_contains)) {
    consumeToken(); // contains

    while (isNot(tok::kw_end)) {
      context.currSymTable = newSymTable;
      context.currPU = name;
      auto PU = parseInternalSubprogram();
      if (!PU) {
        return nullptr;
      }
      PUList.push_back(PU);

      assert(is(tok::eol));
      consumeToken();
    }
  }

  // "end module" statement.
  if (!expect(tok::kw_module)) {
    Diag.printError(getCurrLoc(), diag::mp_end_stmt_err1);
    return nullptr;
  }

  auto endName = expectIdentifier();
  if (endName.empty()) {
    return nullptr;
  }

  // verify if the program name is as previous one.
  if (name != endName) {
    Diag.printError(getCurrLoc(), diag::mp_end_stmt_err2);
  }

  // Expect EOL after statement.
  assert(expect(tok::eol));

  if (Diag.hasError()) {
    return nullptr;
  }

  return builder.buildModule(name, newSymTable, specPart, PUList);
}

Function *Parser::parseFunction(Type *returnType) {

  assert(isOneOf({tok::kw_function, tok::kw_subroutine, kw_program}));

  auto currFuncTok = getToken().Kind;

  auto currPUKind = ProgramUnitKind::FunctionKind;
  auto currScope = ScopeKind::FunctionScope;
  switch (currFuncTok) {
  case tok::kw_program:
    currPUKind = ProgramUnitKind::MainProgramKind;
    currScope = ScopeKind::MainProgramScope;
    break;
  case tok::kw_function:
    currPUKind = ProgramUnitKind::FunctionKind;
    currScope = ScopeKind::FunctionScope;
    break;
  case tok::kw_subroutine:
    currPUKind = ProgramUnitKind::SubroutineKind;
    currScope = ScopeKind::SubroutineScope;
    break;
  default:
    llvm_unreachable("Unreachable program unit kind.");
  }

  // Parse the begin token.
  // syntax :  program program-name
  auto name = expectIdentifier();
  if (name.empty()) {
    return nullptr;
  }

  SourceLoc loc = getCurrLoc();

  SymbolTable *newSymTable =
      builder.buildSymbolTable(FC, currScope, name, context.currSymTable);
  context.currSymTable = newSymTable;
  context.currPU = name;
  context.currStmtList = nullptr;

  // consume the identifier
  consumeToken();
  ArgsList argList;
  if (is(tok::l_paren)) {
    // Parse the argument list.
    argList = parseArgsList(tok::r_paren);
    assert(is(tok::r_paren));
    consumeToken();
  }
  assert(is(tok::eol));

  // consume Eol.
  consumeToken();

  // Add the ArgsList parsed to the symbol table
  // as DummyArgsID.
  // Will be processed later with the specification.
  SymbolAttributes attr1;
  llvm::SmallVector<Type *, 2> typeList;
  for (auto arg : argList) {
    auto Ty = Type::getDummyArgTy(FC);
    context.currSymTable->addNewSymbol(arg, Ty, loc, attr1);
    typeList.push_back(Ty);
  }

  SymbolAttributes attr;
  if (currScope == SubroutineScope) {
    attr.isSubroutine = true;
  }
  auto parent = context.currSymTable->getParent();
  auto returnTy = returnType ? returnType : Type::getUndeclaredTy(FC);
  auto FuncTy = FunctionType::get(FC, returnTy, typeList);
  auto sym = parent->getSymbol(name);
  // If symbol is already declared.
  if (sym) {
    auto Ty = sym->getType();
    auto &attr = sym->getAttr();
    if (!Ty->isUndeclaredFnTy() ||
        (currScope == SubroutineScope && !attr.isSubroutine)) {
      Diag.printError(loc, diag::symbol_not_function);
      Diag.printError(sym->getSourceLoc(), diag::already_delcared_here);
      return nullptr;
    }
    sym->setType(FuncTy);
  } else {
    parent->addNewSymbol(name, FuncTy, loc, attr);
  }

  SpecificationPart *specPart = nullptr;
  ExecutionPart *execPart = nullptr;
  specPart = parseSpecification();
  context.currSpecPart = specPart;

  // Internal interface
  ProgramUnitList PUList;
  if (is(tok::kw_interface)) {
    consumeToken(); // interface
    while (isNot(tok::kw_end)) {
      context.currSymTable = newSymTable;
      context.currPU = name;
      auto PU = parseInternalSubprogram();
      if (!PU) {
        return nullptr;
      }
      PUList.push_back(PU);

      assert(is(tok::eol));
      consumeToken();
    }
    assert(expect({tok::kw_interface, tok::eol}));
    consumeToken(); // Consume eol
  }

  execPart = parseExecutionPart();

  if (Diag.hasError())
    return nullptr;

  assert(is(tok::kw_end) || is(tok::kw_contains));

  if (is(tok::kw_contains)) {
    consumeToken(); // contains

    while (isNot(tok::kw_end)) {
      context.currSymTable = newSymTable;
      context.currPU = name;
      auto PU = parseInternalSubprogram();
      if (!PU) {
        return nullptr;
      }
      PUList.push_back(PU);

      assert(is(tok::eol));
      consumeToken();
    }
  }

  consumeToken(tok::kw_end);

  if (isNot(tok::eol)) {

    // "end subroutine" statement.
    if (isNot(currFuncTok)) {
      Diag.printError(getCurrLoc(), diag::mp_end_stmt_err1);
      return nullptr;
    }

    consumeToken(currFuncTok);

    if (isNot(tok::eof) && isNot(tok::eol)) {

      assert(is(tok::identifier));
      auto endName = getToken().getRef();
      if (endName.empty()) {
        return nullptr;
      }

      // verify if the program name is as previous one.
      if (name.lower() != endName.lower()) {
        Diag.printError(getCurrLoc(), diag::mp_end_stmt_err2);
      }

      // Expect EOL after statement.
      assert(expect(tok::eol));
    }
  }

  if (Diag.hasError()) {
    return nullptr;
  }

  return builder.buildFunction(currPUKind, name, argList, newSymTable, specPart,
                               execPart, PUList);
}

ProgramUnit *Parser::parseInternalSubprogram() {
  Type *returnType = nullptr;
  while (isNot(tok::kw_end)) {
    switch (currTokenKind) {
    case tok::kw_subroutine: {
      return parseFunction(Type::getVoidTy(FC));
    }
    case tok::kw_function: {
      return parseFunction(returnType);
    }
    // consume empty line.
    case tok::eol:
      consumeToken();
      continue;
    // Not handled for now.
    case tok::kw_recursive:
      consumeToken();
      continue;
    default:
      // might be the return type. Parse it.
      // TODO: parsing scalar return types for now.
      if (utils::isKeywordToken(currTokenKind)) {
        returnType = getTypeFor(currTokenKind, -1, false);
        consumeToken();
        continue;
      }
      llvm_unreachable("Token not hanlded yet.");
    };
  }
  return nullptr;
}

// This routine parses the whole Program and returns the AST.

// A program may consist of following:
// 1. main program (one)
// 2. 0+ extern subprogram
// 3. 0+ module
// 4. 0+ submodule
// 5. 0+ block data
bool Parser::parseProgram() {

  auto globalSymbolTable =
      builder.buildSymbolTable(FC, ScopeKind::GlobalScope, "Global", nullptr);
  context.globalSymTable = globalSymbolTable;
  context.currSymTable = context.globalSymTable;

  ProgramUnitList programList;

  // For functions.
  Type *returnType = nullptr;

  while (isNot(tok::eof)) {
    ProgramUnit *PU = nullptr;
    context.currSymTable = context.globalSymTable;
    switch (currTokenKind) {
    // 1. parse main program.
    case tok::kw_program: {
      PU = parseFunction(Type::getInt32Ty(FC));
      break;
    }
    case tok::kw_subroutine: {
      PU = parseFunction(Type::getVoidTy(FC));
      break;
    }
    case tok::kw_function: {
      PU = parseFunction(returnType);
      break;
    }
    case tok::kw_module: {
      PU = parseModule();
      break;
    }
    // consume empty line.
    case tok::eol:
      consumeToken();
      continue;
    case tok::eof:
      break;
    default:
      // might be the return type. Parse it.
      // TODO: parsing scalar return types for now.
      if (utils::isKeywordToken(currTokenKind)) {
        returnType = getTypeFor(currTokenKind, -1, false);
        consumeToken();
        continue;
      }
      llvm_unreachable("\n ERROR: Token not handled yet.\n");
    }
    if (!PU && isNot(tok::eof)) {
      return false;
    }
    if (PU)
      programList.push_back(PU);
  }

  parseTree = std::make_unique<ParseTree>(FC.inputFileName, FC,
                                          globalSymbolTable, programList);

  return true;
}
