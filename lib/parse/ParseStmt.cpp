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
#include "AST/Statements.h"
#include "parse/Parser.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace parser;

// Parse (pointer) assignment statements
Stmt *Parser::parseAssignmentStmt() {
  SourceLoc loc = getCurrLoc();
  auto lhs = parseExprOperand();
  assert(lhs);

  // if pointer assignment
  if (is(tok::ptr_assign)) {
    auto rhs = parseExpr();
    auto pointerAssignmentStmt =
        builder.buildPointerAssignmentStmt(lhs, rhs, loc);
    if (is(tok::semicolon)) {
      consumeToken();
    }
    return pointerAssignmentStmt;
  }

  // Mostly a function call without LHS
  if (isNot(tok::equals)) {
    auto funcRef = llvm::dyn_cast<FunctionReference>(lhs);
    assert(funcRef);
    Symbol *sym = funcRef->getSymbol();
    ExprList args = funcRef->getArgsList();
    ExprList newArgs;
    newArgs.append(args.begin(), args.end());
    auto call = builder.buildCallStmt(sym, newArgs, funcRef->getSourceLoc());
    // delete(funcRef);
    return call;
  }
  auto rhs = parseExpr();
  auto assignmentStmt = builder.buildAssignmentStmt(lhs, rhs, loc);
  if (is(tok::semicolon)) {
    consumeToken();
  }
  return assignmentStmt;
}

// Parse PRINT *, <expr-list>
PrintStmt *Parser::parsePrintStmt() {

  SourceLoc loc = getCurrLoc();
  assert(is(tok::kw_print));
  if (!expect({tok::star, tok::comma})) {
    Diag.printError(getCurrLoc(), diag::err_printstmt);
    return nullptr;
  }
  bool hasRangeExpr;
  auto exprList = parseExprList(tok::eol, hasRangeExpr);
  if (exprList.empty()) {
    Diag.printError(getCurrLoc(), diag::err_printstmt);
    return nullptr;
  }
  return builder.buildPrintStmt(exprList, loc);
}

// Parse READ *, <input-item> |
//       READ(1, *)  <input-item>
ReadStmt *Parser::parseReadStmt() {
  SourceLoc loc = getCurrLoc();
  assert(is(tok::kw_read));

  // Next can be * or (
  consumeToken();

  Expr *unit = nullptr;
  Format *format = nullptr;
  Expr *iostat = nullptr;
  if (is(tok::l_paren)) {

    // Consume l_paren
    consumeToken();

    if (is(tok::star))
      consumeToken();
    else
      unit = parseExprOperand();

    if (is(tok::comma)) {
      // TODO: parse read format.
      consumeToken(tok::comma);
      if (isNot(tok::star)) {
        format = parseFormat(tok::r_paren);
      } else {
        consumeToken(); // consumen *,
      }
    }

    while (is(tok::comma)) {
      consumeToken();
      if (is(tok::kw_iostat)) {
        if (!expect(tok::equals)) {
          Diag.printError(getCurrLoc(), diag::err_readstmt);
          return nullptr;
        }
        iostat = parseExpr();
        continue;
      }
      llvm_unreachable("Unhandled argument in read statement");
    }

  } else {
    if (isNot(tok::star)) {
      Diag.printError(getCurrLoc(), diag::err_readstmt);
      return nullptr;
    }

    if (!expect(tok::comma, diag::err_readstmt))
      return nullptr;
  }
  bool hasRangeExpr;
  auto exprList = parseExprList(tok::eol, hasRangeExpr);
  if (exprList.empty()) {
    Diag.printError(getCurrLoc(), diag::err_readstmt);
    return nullptr;
  }
  return builder.buildReadStmt(exprList, loc, unit, format, iostat);
}

WriteStmt *Parser::parseWriteStmt() {
  SourceLoc loc = getCurrLoc();
  assert(is(tok::kw_write));

  if (!expect(tok::l_paren, diag::err_writestmt)) {
    return nullptr;
  }

  Expr *unit = nullptr;
  consumeToken();
  if (is(tok::star)) {
    unit = nullptr;
    // Consume ","
    consumeToken();
  } else {
    unit = parseExpr(false);
  }

  // Format can be present or absent or can be *

  Format *format = nullptr;
  ConstantVal *advance = nullptr;
  Expr *iostat = nullptr;
  if (is(tok::comma)) {
    consumeToken();

    while (isNot(tok::r_paren)) {
      if (is(tok::comma)) {
        consumeToken(); // ","
        continue;
      } else if (is(tok::star)) {
        if (!expect(tok::r_paren)) {
          Diag.printError(getCurrLoc(), diag::err_writestmt);
          return nullptr;
        }
        break;
      } else if (is(tok::string) || is(tok::kw_fmt) || is(tok::identifier)) {

        if (is(tok::kw_fmt)) {
          consumeToken(); // "= "
          consumeToken();
        }
        format = parseFormat(tok::r_paren);
      } else if (is(tok::kw_advance)) {

        if (!expect(tok::equals, diag::err_writestmt))
          return nullptr;

        // Consume equals
        consumeToken();

        advance = builder.buildConstantVal(
            getToken().getRef(),
            getTypeFor(currTokenKind, getToken().constantTypeSize, true),
            getCurrLoc());
        consumeToken(); // consumen the next token
      } else if (is(tok::kw_iostat)) {
        consumeToken(); // "= "
        iostat = parseExpr();
      } else {
        llvm_unreachable("How to handle this format");
      }
    }
  }

  consumeToken(); // consume r_paren
  bool hasRangeExpr;
  ExprList exprList;

  // This is io implied DO
  if (is(tok::l_paren)) {
    unsigned dimension = 0;
    while (is(tok::l_paren)) {
      dimension++;
      consumeToken();
    }
    ExprList exprs;
    while (true) {
      exprs.push_back(parseExpr(false));

      // Break when range expression
      if (match({tok::identifier, tok::equals}))
        break;
      assert(is(tok::comma));
      consumeToken(); // Consume next identifier
    }
    ExprList ioDos;
    while (dimension--) {
      consumeToken(); // consume identifier
      ioDos.push_back(parseImpliedDo());
    }

    if (isNot(tok::eol) && isNot(tok::semicolon)) {
      assert(is(tok::comma));
      consumeToken(); // ","
    }

    exprList.push_back(
        builder.buildIOImpliedDo(exprs, ioDos, exprs[0]->getType(), loc));
  }
  if (isNot(tok::eol) && isNot(tok::semicolon)) {
    auto newExprList = parseExprList(tok::eol, hasRangeExpr, false);
    exprList.insert(exprList.end(), newExprList.begin(), newExprList.end());
  }

  if (is(tok::semicolon)) {
    consumeToken(); // consume semicolon
  }

  return builder.buildWriteStmt(exprList, loc, unit, format, advance, iostat);
}

// Parse Open stmt
OpenStmt *Parser::parseOpenStmt() {
  SourceLoc loc = getCurrLoc();
  assert(is(tok::kw_open));
  if (!expect(tok::l_paren, diag::err_openstmt)) {
    return nullptr;
  }

  // Step 1. Consume the unit,
  // unit -> [unit] scalar-int-expr
  // Cosume unit/expression
  consumeToken();
  if (is(tok::kw_unit)) {
    // Consume =
    if (!expect(tok::equals, diag::err_openstmt)) {
      return nullptr;
    }

    // consume the expression
    consumeToken();
  }

  auto unit = parseExpr(false);

  Expr *status = nullptr;
  Expr *file = nullptr;
  Expr *form = nullptr;
  Expr *iostat = nullptr;
  ExprList specList;
  while (isNot(tok::r_paren)) {
    // consume comma
    consumeToken();
    switch (currTokenKind) {
    case kw_file:
      if (!expect(tok::equals, diag::err_openstmt))
        return nullptr;
      file = parseExpr();
      break;
    case kw_status:
      if (!expect(tok::equals, diag::err_openstmt))
        return nullptr;
      status = parseExpr();
      break;
    case kw_form:
      // TODO: form not handled.
      if (!expect(tok::equals, diag::err_openstmt))
        return nullptr;
      form = parseExpr();
      break;
    case kw_iostat:
      if (!expect(tok::equals, diag::err_openstmt))
        return nullptr;
      iostat = parseExpr();
      break;
    default:
      specList.push_back(parseExpr(false));
      break;
    }
  }

  StatusKind statusKind = StatusKind::undefined;
  if (status) {
    auto ArrayTy = llvm::dyn_cast<ArrayType>(status->getType());
    assert(ArrayTy && ArrayTy->getElementTy()->isStringCharTy());
    auto statusString = llvm::dyn_cast<ConstantVal>(status);
    assert(statusString);
    std::string value = statusString->getValueRef().lower();

    statusKind = llvm::StringSwitch<StatusKind>(value)
                     .Case("old", StatusKind::OLD)
                     .Case("new", StatusKind::NEW)
                     .Case("scatch", StatusKind::SCRATCH)
                     .Case("replace", StatusKind::REPLACE)
                     .Case("unknown", StatusKind::UNKNOWN)
                     .Default(StatusKind::undefined);
    assert(statusKind != StatusKind::undefined);
  }

  // cosume the r_paren
  consumeToken();

  return builder.buildOpenStmt(loc, unit, file, statusKind, iostat, specList);
}

// Parse Close stmt
CloseStmt *Parser::parseCloseStmt() {
  SourceLoc loc = getCurrLoc();
  assert(is(tok::kw_close));

  if (!expect(tok::l_paren, diag::err_closestmt)) {
    return nullptr;
  }

  // Step 1. Consume the unit,
  // unit -> [unit] scalar-int-expr
  // Cosume unit/expression
  consumeToken();
  if (is(tok::kw_unit)) {
    // consume =
    if (!expect(tok::equals, diag::err_openstmt)) {
      return nullptr;
    }

    // consume the expression
    consumeToken();
  }

  auto unit = parseExpr(false);
  Expr *iostat = nullptr;

  if (is(tok::comma)) {
    while (is(tok::comma)) {
      consumeToken();
      if (is(tok::kw_iostat)) {
        if (!expect(tok::equals)) {
          Diag.printError(getCurrLoc(), diag::err_readstmt);
          return nullptr;
        }
        iostat = parseExpr();
        continue;
      }
      llvm_unreachable("Unhandled argument in read statement");
    }
  }

  // cosume the r_paren
  consumeToken();
  return builder.buildCloseStmt(loc, unit, iostat);
}

DeAllocateStmt *Parser::parseDeAllocateStmt() {
  assert(is(tok::kw_deallocate));
  SourceLoc loc = getCurrLoc();
  if (!expect(tok::l_paren, diag::err_allocstmt))
    return nullptr;

  SymbolList deAllocateObjectList;
  Expr *stat = nullptr;
  while (isNot(tok::r_paren)) {
    auto objectName = expectIdentifier();
    auto objectSym = context.currSymTable->getOrInsertSymbol(
        objectName, Type::getUndeclaredFnTy(FC), loc);

    consumeToken(); // consume the identifier

    deAllocateObjectList.push_back(objectSym);
    if (!isOneOf({tok::comma, tok::r_paren})) {
      Diag.printError(getCurrLoc(), diag::err_allocstmt);
      return nullptr;
    }

    if (match({tok::kw_stat, tok::equals})) {
      consumeToken(); // Consume stat
      consumeToken(); // consume =
      stat = parseExpr();
    }
  }

  // consume eol
  consumeToken();
  return builder.buildDeAllocateStmt(deAllocateObjectList, loc, stat);
}

// allocatae-stmt
AllocateStmt *Parser::parseAllocateStmt() {
  assert(is(tok::kw_allocate));
  SourceLoc loc = getCurrLoc();
  if (!expect(tok::l_paren, diag::err_allocstmt))
    return nullptr;

  SymbolList allocateObjectList;
  ArraySpecList allocateShapeList;
  Expr *stat = nullptr;
  while (isNot(tok::r_paren)) {
    auto objectName = expectIdentifier();
    auto objectSym = context.currSymTable->getOrInsertSymbol(
        objectName, Type::getUndeclaredFnTy(FC), loc);
    // consumen l_paren
    consumeToken();
    auto arraySpec = parseArraySpec();

    allocateObjectList.push_back(objectSym);
    allocateShapeList.push_back(arraySpec);

    if (!isOneOf({tok::comma, tok::r_paren})) {
      Diag.printError(getCurrLoc(), diag::err_allocstmt);
      return nullptr;
    }

    if (match({tok::kw_stat, tok::equals})) {
      consumeToken(); // Consume stat
      consumeToken(); // consume =
      stat = parseExpr();
    }
  }

  // consume eol
  consumeToken();

  return builder.buildAllocateStmt(allocateObjectList, allocateShapeList, loc,
                                   stat);
}

NullifyStmt *Parser::parseNullifyStmt() {
  bool hasRange;
  SourceLoc loc = getCurrLoc();

  consumeToken(); // nullify
  ExprList list = parseExprList(tok::r_paren, hasRange,
                                /* conumeCurrent = */ true);

  if (list.empty()) {
    Diag.printError(getCurrLoc(), diag::err_nullifystmt);
    return nullptr;
  }

  consumeToken(); // r_paren
  return builder.buildNullifyStmt(list, loc);
}

// stop-stmt -> STOP [const-int-expr]
StopStmt *Parser::parseStopStmt() {
  SourceLoc loc = getCurrLoc();

  consumeToken(tok::kw_stop);

  Expr *stopCode = nullptr;
  if (isNot(tok::eol)) {
    stopCode = parseExpr(false);
  }
  assert(is(tok::eol));
  // assert(stopCode->getType()->isIntegralTy() &&
  //      "Expecting const-int expression");
  return builder.buildStopStmt(stopCode, loc);
}

DoWhileStmt *Parser::parseDoWhileStmt() {

  SourceLoc loc = getCurrLoc();
  if (isNot(tok::kw_while)) {
    Diag.printError(getCurrLoc(), diag::exp_while);
    return nullptr;
  }
  auto logicalExpr = parseExpr();

  if (isNot(tok::eol)) {
    Diag.printError(getCurrLoc(), diag::exp_eol);
    return nullptr;
  }

  // consume eol.
  consumeToken();

  // Parse List of action stmt.
  Block *block = parseBlock();
  if (!block) {
    return nullptr;
  }
  if (isNot(kw_enddo)) {
    assert(is(kw_end));
    // Check if we have end if.
    if (!expect(tok::kw_do)) {
      Diag.printError(getCurrLoc(), diag::exp_end_do);
      return nullptr;
    }
  }
  // consume enddo.
  consumeToken();

  assert(is(tok::eol));
  return builder.buildDoWhileStmt(logicalExpr, block, loc);
}

DoStmt *Parser::parseDoStmt(llvm::StringRef name, SourceLoc &loc) {
  QuadExpr *expr = nullptr;

  if (isNot(tok::eol)) {
    Expr *operands[4];

    // Expect Do variable.
    operands[0] = parseExpr(false);
    if (!is(tok::equals)) {
      Diag.printError(getCurrLoc(), diag::exp_equals);
      return nullptr;
    }

    // Expect Do init.
    operands[1] = parseExpr();
    if (!is(tok::comma)) {
      Diag.printError(getCurrLoc(), diag::exp_equals);
      return nullptr;
    }

    // Expect Do end.
    operands[2] = parseExpr();

    if (is(tok::comma)) {
      operands[3] = parseExpr();
    } else {
      operands[3] = builder.buildConstantVal("1",
                                             getTypeFor(tok::integer,
                                                        /*default=*/4),
                                             getCurrLoc());
    }
    expr = builder.buildQuadExpr(operands, operands[0]->getType(), loc);
    context.currStmtList->push_back(expr);
  }
  // consume eol
  consumeToken(tok::eol);

  auto block = parseBlock();
  if (!block) {
    return nullptr;
  }

  if (isNot(kw_enddo)) {
    assert(is(kw_end));
    // Check if we have end if.
    if (!expect(tok::kw_do)) {
      Diag.printError(getCurrLoc(), diag::exp_end_do);
      return nullptr;
    }
  }
  // consume enddo.
  consumeToken();

  if (!name.empty()) {
    assert(is(tok::identifier));
    auto endName = getToken().getRef();
    assert(endName == name);
    consumeToken(tok::identifier);
  }

  assert(is(tok::eol));
  return builder.buildDoStmt(expr, block, name, loc);
}

ForAllStmt *Parser::parseForAllStmt(llvm::StringRef name) {
  SourceLoc loc = getCurrLoc();

  consumeToken(tok::kw_forall);

  assert(is(tok::l_paren));

  bool hasRange = false;
  ExprList list = parseExprList(tok::r_paren, hasRange);

  consumeToken(tok::r_paren);

  auto assignmentStmt = parseAssignmentStmt();
  StmtList stmtList;
  stmtList.push_back(assignmentStmt);
  auto block = builder.buildBlock(stmtList, loc);

  return builder.buildForAllStmt(list, block, name, loc);
}

WhereStmt *Parser::parseWhereStmt(Expr *expr, SourceLoc loc,
                                  bool &isSingleStmt) {
  if (isNot(tok::eol) && isNot(tok::r_paren)) {
    isSingleStmt = true;
    auto assignmentStmt = parseAssignmentStmt();
    StmtList stmtList;
    stmtList.push_back(assignmentStmt);
    auto block = builder.buildBlock(stmtList, loc);
    return builder.buildWhereStmt(expr, block, loc);
  }

  // Consume left out r_paren if any
  if (is(tok::r_paren))
    consumeToken();

  // Consume eol
  consumeToken();
  auto whereBlock = parseBlock();
  return builder.buildWhereStmt(expr, whereBlock, loc);
}

WhereConstruct *Parser::parseWhereConstruct() {

  WhereStmtList list;
  WhereStmtKindList kindList;
  bool isSingleStmt = false;
  SourceLoc loc = getCurrLoc();
  assert(is(kw_where));
  auto maskExpr = parseExpr();

  list.push_back(parseWhereStmt(maskExpr, loc, isSingleStmt));
  kindList.push_back(WhereElseConstructKind::WhereKind);

  if (isSingleStmt) {
    return builder.buildWhereConstruct(list, kindList, loc);
  }

  while (is(tok::kw_elsewhere)) {
    // consume elsewhere
    SourceLoc whereLoc = getCurrLoc();
    consumeToken();

    if (is(tok::eol)) {
      list.push_back(parseWhereStmt(nullptr, whereLoc, isSingleStmt));
      kindList.push_back(WhereElseConstructKind::ElseWhereKind);
      break;
    }

    maskExpr = parseExpr();
    list.push_back(parseWhereStmt(maskExpr, whereLoc, isSingleStmt));
    kindList.push_back(WhereElseConstructKind::ElseWhereWhereKind);
  }

  assert(is(tok::kw_end));
  if (!expect({tok::kw_where, tok::eol}))
    llvm_unreachable("Expecting endwhere");
  return builder.buildWhereConstruct(list, kindList, loc);

  /*
  auto whereBlock = parseBlock();
  if (isNot(kw_end)) {
    assert(is(kw_elsewhere));

    // Consume elsewhere
    consumeToken();

    // Consume eol for now
    consumeToken();

    auto elseBlock = parseBlock();
    assert(is(tok::kw_end));
    if (!expect({tok::kw_where, tok::eol}))
      llvm_unreachable("Expecting endwhere");

    return builder.buildWhereConstruct(maskExpr, whereBlock, elseBlock, loc);
  }

  assert(is(tok::kw_end));
  if (!expect({tok::kw_where, tok::eol}))
    llvm_unreachable("Expecting endwhere");

  return builder.buildWhereConstruct(maskExpr, whereBlock, nullptr, loc);
  */
}

CallStmt *Parser::parseCallStmt() {

  assert(is(tok::kw_call));
  // consume Call
  consumeToken();
  SourceLoc loc = getCurrLoc();

  if (!is(tok::identifier)) {
    Diag.printError(getCurrLoc(), diag::ident_err1);
    return nullptr;
  }

  llvm::StringRef subName = getToken().getRef();

  auto sym = context.currSymTable->getSymbol(subName);
  if (!sym) {
    SymbolAttributes attributes;
    attributes.isSubroutine = true;
    sym = context.currSymTable->addNewSymbol(
        subName, Type::getUndeclaredFnTy(FC), getCurrLoc(), attributes);
  }

  ExprList exprList;
  consumeToken(tok::identifier);
  // Subroutine without arguments can ignore the parenthesis.
  if (isNot(tok::eol)) {
    if (isNot(tok::l_paren)) {
      Diag.printError(getCurrLoc(), diag::exp_l_paren);
      return nullptr;
    }

    bool hasRangeExpr;
    exprList = parseExprList(tok::r_paren, hasRangeExpr);

    // consume )
    consumeToken();
  }

  assert(is(tok::eol));
  return builder.buildCallStmt(sym, exprList, loc);
}

CaseStmt *Parser::parseCaseStmt() {
  auto loc = getCurrLoc();

  // Consume case, Next can be l_paren or kw_default
  consumeToken(tok::kw_case);
  ExprList exprList;

  if (is(tok::kw_default)) {
    // consume default
    consumeToken(tok::kw_default);
    // consume eol
    consumeToken(tok::eol);
    auto caseBlock = parseBlock();
    return builder.buildCaseStmt(exprList, caseBlock, loc);
  }

  assert(is(tok::l_paren));
  bool hasRangeExpr;
  exprList = parseExprList(tok::r_paren, hasRangeExpr);

  // consume r_paren
  consumeToken(tok::r_paren);
  // consume eol
  consumeToken(tok::eol);

  auto caseBlock = parseBlock();
  return builder.buildCaseStmt(exprList, caseBlock, loc);
}

SelectCaseStmt *Parser::parseSelectCaseStmt() {
  assert(is(tok::kw_select));

  SourceLoc loc = getCurrLoc();
  if (!expect(tok::kw_case)) {
    Diag.printError(getCurrLoc(), diag::expect_case);
    return nullptr;
  }
  consumeToken(); // case token.
  Expr *caseExpr = nullptr;
  if (is(tok::l_paren)) {
    caseExpr = parseExpr();
  }

  CaseStmtList list;
  assert(caseExpr);
  assert(is(r_paren));
  if (!expect(tok::eol)) {
    Diag.printError(getCurrLoc(), diag::expect_eol);
    return nullptr;
  }

  consumeToken(); // eol

  while (is(tok::kw_case)) {
    list.push_back(parseCaseStmt());
  }

  // end select statement.
  assert(is(tok::kw_end));
  assert(expect({tok::kw_select, tok::eol}));
  assert(is(tok::eol));
  return builder.buildSelectCaseStmt(caseExpr, list, loc);
}

ExitStmt *Parser::parseExitStmt() {
  auto loc = getCurrLoc();
  consumeToken(tok::kw_exit);

  llvm::StringRef name;
  if (is(tok::identifier)) {
    name = getToken().getRef();
    consumeToken();
  }
  assert(is(tok::eol));
  return builder.buildExitStmt(name, loc);
}

CycleStmt *Parser::parseCycleStmt() {
  auto loc = getCurrLoc();
  consumeToken(tok::kw_cycle);

  llvm::StringRef name;
  if (is(tok::identifier)) {
    name = getToken().getRef();
    consumeToken();
  }
  assert(is(tok::eol));
  return builder.buildCycleStmt(name, loc);
}

ReturnStmt *Parser::parseReturnStmt() {
  auto loc = getCurrLoc();
  consumeToken(tok::kw_return);

  Expr *expr = nullptr;
  if (isNot(tok::eol)) {
    expr = parseExpr(false);
  }

  assert(is(tok::eol));
  return builder.buildReturnStmt(expr, loc);
}

bool Parser::parseKeywordActionStmt(StmtList &stmtList, llvm::StringRef name) {
  switch (currTokenKind) {
  case tok::kw_stop:
    stmtList.push_back(parseStopStmt());
    return true;
  case tok::kw_print:
    stmtList.push_back(parsePrintStmt());
    return true;
  case tok::kw_read:
    stmtList.push_back(parseReadStmt());
    return true;
  case tok::kw_write:
    stmtList.push_back(parseWriteStmt());
    return true;
  case tok::kw_open:
    stmtList.push_back(parseOpenStmt());
    return true;
  case tok::kw_close:
    stmtList.push_back(parseCloseStmt());
    return true;
  case tok::kw_if:
    stmtList.push_back(parseIfElseConstruct());
    return true;
  case tok::kw_where:
    stmtList.push_back(parseWhereConstruct());
    return true;
  case tok::kw_select:
    stmtList.push_back(parseSelectCaseStmt());
    return true;
  case tok::kw_cycle:
    stmtList.push_back(parseCycleStmt());
    return true;
  case tok::kw_exit:
    stmtList.push_back(parseExitStmt());
    return true;
  case tok::kw_return:
    stmtList.push_back(parseReturnStmt());
    return true;
  case tok::kw_do: {
    // consume DO
    auto loc = getCurrLoc();
    consumeToken();
    if (is(tok::kw_while)) {
      stmtList.push_back(parseDoWhileStmt());
      return true;
    }
    if (!utils::isKeywordToken(currTokenKind)) {
      stmtList.push_back(parseDoStmt(name, loc));
      return true;
    }
    llvm_unreachable("unknown DO statement");
  }
  case tok::kw_forall: {
    stmtList.push_back(parseForAllStmt(name));
    return true;
  }
  case tok::kw_call:
    stmtList.push_back(parseCallStmt());
    return true;
  case tok::kw_allocate:
    stmtList.push_back(parseAllocateStmt());
    return true;
  case tok::kw_deallocate:
    stmtList.push_back(parseDeAllocateStmt());
    return true;
  case tok::kw_nullify:
    stmtList.push_back(parseNullifyStmt());
    return true;
  default:
    llvm_unreachable("Unhandled keyword actionstmt");
  };
  return false;
}

bool Parser::parseActionStmt(StmtList &stmtList, bool consume) {

  llvm::StringRef name;
  if (is(tok::identifier)) {
    // This is statement of form <identifier>: <stmt>
    if (match({tok::colon})) {
      name = getToken().getRef();
      consumeToken(tok::identifier);
      consumeToken(tok::colon);
    } else {
      auto assignment = parseAssignmentStmt();
      stmtList.push_back(assignment);
      if (consume && is(tok::eol)) {
        consumeToken();
      }
      return true;
    }
  }

  // An assingment statement where LHS is identifier which is also key word
  // Symbol has to be in the symbol table and should follow with
  // equals or l_paren in case of array element and shouldn't be intrinsic.
  auto sym = context.currSymTable->getSymbol(getToken().getRef());
  if ((utils::isKeywordToken(currTokenKind) && match(tok::equals)) ||
      (sym &&
       (match(tok::equals) ||
        (match(tok::l_paren) && !intrin::isIntrinsic(getToken().getRef()))))) {

    auto assignment = parseAssignmentStmt();
    stmtList.push_back(assignment);
    if (consume && is(tok::eol)) {
      consumeToken();
    }
    return true;
  }

  if (!parseKeywordActionStmt(stmtList, name)) {
    return false;
  }

  // Consume the next token
  if (consume && is(tok::eol))
    consumeToken();
  return true;
}

Block *Parser::parseBlock() {
  StmtList stmtList;
  auto parentList = context.currStmtList;
  context.currStmtList = &stmtList;
  // Parse List of action stmt.
  if (!parseExecutableStmtList(stmtList)) {
    return nullptr;
  }
  auto block = builder.buildBlock(stmtList, getCurrLoc());
  assert(parentList);
  context.currStmtList = parentList;
  return block;
}

std::tuple<Expr *, Block *> Parser::parseIfStmt(bool &hasThen, bool isElseIf) {
  Expr *logicalExpr = parseExpr();
  Block *block = nullptr;

  auto nullTuple = std::make_tuple<Expr *, Block *>(nullptr, nullptr);
  hasThen = false;
  // Check if we have THEN

  if (is(kw_then)) {
    hasThen = true;

    if (!expect(tok::eol)) {
      Diag.printError(getCurrLoc(), diag::exp_eol);
      return nullTuple;
    }

    // consume eol
    consumeToken();

    block = parseBlock();
    if (!block) {
      return nullTuple;
    }
  }

  // Get only one action stmt.
  else {
    if (isElseIf) {
      Diag.printError(getCurrLoc(), diag::missing_then);
      return nullTuple;
    }
    StmtList stmtList;
    if (!parseActionStmt(stmtList, false)) {
      return nullTuple;
    }
    assert(stmtList.size() == 1 && "Expecting only one action");
    auto actionStmt = llvm::dyn_cast<Stmt>(*stmtList.begin());
    assert(actionStmt && "Expecting action statement");
    block = builder.buildBlock(stmtList, getCurrLoc());
  }

  return std::tuple<Expr *, Block *>(logicalExpr, block);
}

IfElseStmt *Parser::parseIfElseConstruct() {
  assert(is(tok::kw_if));
  SourceLoc loc = getCurrLoc();
  Expr *expr = nullptr;
  Block *block = nullptr;
  IfStmtList list;
  IfConstructKindList kindList;
  bool hasThen = false;
  std::tie(expr, block) = parseIfStmt(hasThen);
  if (!expr || !block)
    return nullptr;
  list.push_back(builder.buildIfStmt(expr, block, loc));
  kindList.push_back(IfConstructKind::IfThenKind);

  if (!hasThen) {
    assert(is(tok::eol));
    kindList[0] = IfConstructKind::IfKind;
    return builder.buildIfElseStmt(list, kindList, loc);
  }

  while (is(tok::kw_else) || is(tok::kw_elseif)) {

    if (isNot(tok::kw_elseif)) {
      // consume else
      consumeToken(tok::kw_else);
    }

    if (is(tok::eol)) {
      consumeToken();
    } // parse else-if statement.
    else if (is(tok::kw_elseif) || is(tok::kw_if)) {
      auto loc = getCurrLoc();
      std::tie(expr, block) = parseIfStmt(hasThen, true);
      if (!expr || !block)
        return nullptr;
      kindList.push_back(IfConstructKind::ElseIfKind);
      list.push_back(builder.buildIfStmt(expr, block, loc));
      continue;
    }

    // parse plain else.
    auto loc = getCurrLoc();
    block = parseBlock();
    if (!block)
      return nullptr;
    kindList.push_back(IfConstructKind::ElseKind);
    list.push_back(builder.buildIfStmt(nullptr, block, loc));
  }

  assert(isOneOf({kw_end, kw_endif}));
  // Check if we have end if.
  if (is(tok::kw_end) && !expect(tok::kw_if)) {
    Diag.printError(getCurrLoc(), diag::exp_end_if);
    return nullptr;
  }
  // consume endif.
  consumeToken();

  assert(is(tok::eol));
  return builder.buildIfElseStmt(list, kindList, loc);
}
