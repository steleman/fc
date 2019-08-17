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
#include "AST/Expressions.h"
#include "AST/ProgramUnit.h"
#include "parse/Parser.h"

using namespace fc;
using namespace parser;

static BinaryOpKind getBinaryOpKind(TokenKind tokenKind) {
  switch (tokenKind) {
  case tok::plus:
    return BinaryOpKind::Addition;
  case tok::minus:
  case tok::unary_minus:
    return BinaryOpKind::Subtraction;
  case tok::star:
    return BinaryOpKind::Multiplication;
  case tok::division:
    return BinaryOpKind::Division;
  case tok::power:
    return BinaryOpKind::Power;
  case tok::concat:
    return BinaryOpKind::Concat;
  default:
    llvm_unreachable("Unhandled binary operator");
  }
}

static RelationalOpKind getRelationalOpKind(TokenKind kind) {
  switch (kind) {
  case tok::eq:
    return RelationalOpKind::EQ;
  case tok::ne:
    return RelationalOpKind::NE;
  case tok::gt:
    return RelationalOpKind::GT;
  case tok::ge:
    return RelationalOpKind::GE;
  case tok::lt:
    return RelationalOpKind::LT;
  case tok::le:
    return RelationalOpKind::LE;
  default:
    llvm_unreachable("Unhandled relational operator");
  }
}

static LogicalOpKind getLogicalOpKind(TokenKind kind) {
  switch (kind) {
  case tok::l_and:
    return LogicalOpKind::AND;
  case tok::l_or:
    return LogicalOpKind::OR;
  case tok::l_eqv:
    return LogicalOpKind::EQV;
  case tok::l_neqv:
    return LogicalOpKind::NEQV;
  case tok::l_not:
    return LogicalOpKind::NOT;
  default:
    llvm_unreachable("Unhandled logical operator!");
  }
}

static int getOpPrecedence(TokenKind tokKind) {
  switch (tokKind) {
  case tok::plus:
  case tok::minus:
    return 10;
  case tok::star:
  case tok::division:
    return 20;
  case tok::power:
    return 30;
  case tok::unary_minus:
    return 40;
  case tok::eq:
  case tok::ne:
  case tok::gt:
  case tok::ge:
  case tok::lt:
  case tok::le:
    return 9;
  case tok::l_not:
    return 8;
  case tok::l_and:
    return 7;
  case tok::l_or:
    return 6;
  case tok::l_eqv:
  case tok::l_neqv:
    return 5;
  default:
    return 0;
    llvm_unreachable("Undefined operator");
  }
}

// Parse Implied do
QuadExpr *Parser::parseImpliedDo() {

  assert(is(tok::identifier));
  auto sym = getSymbolFor(getToken().getRef(), getCurrLoc());

  consumeToken(); // consume identifier
  consumeToken(); // consume equals

  auto initExpr = parseExpr(false);
  assert(initExpr);

  consumeToken(tok::comma);

  auto endExpr = parseExpr(false);

  Expr *incrExpr = nullptr;
  if (currTokenKind == tok::comma) {
    incrExpr = parseExpr();
  } else {
    assert(is(tok::r_paren));
    incrExpr =
        builder.buildConstantVal("1", Type::getInt32Ty(FC), getCurrLoc());
  }
  consumeToken(tok::r_paren);

  auto objName = builder.buildObjectName(sym, sym->getSourceLoc());
  Expr *ops[4] = {objName, initExpr, endExpr, incrExpr};
  return builder.buildQuadExpr(ops, initExpr->getType(), sym->getSourceLoc());
}

// The current expression to be parsed might be a range expression. If it is,
// construct a range argument and return. Else, return simple expression.
Expr *Parser::parseRangeExpr(llvm::ArrayRef<TokenKind> endToks,
                             bool &hasRange) {

  llvm::StringRef identName;
  bool isAssignExpr = false;

  // Expressions of the form <symbol>("a=10",..)
  if (is(tok::identifier) && match(tok::equals)) {
    bool hasEquals = false;
    for (auto tok : endToks) {
      if (tok == tok::equals) {
        hasEquals = true;
        break;
      }
    }

    if (!hasEquals) {
      identName = getToken().getRef();
      consumeToken(tok::identifier);
      consumeToken(tok::equals);
      isAssignExpr = true;
    }
  }

  // For lb,ub,stride.
  Expr *exprs[3] = {nullptr, nullptr, nullptr};
  auto loc = getCurrLoc();
  unsigned I = 0;
  while (!isOneOf(endToks)) {
    if (is(tok::colon)) {
      consumeToken(tok::colon);
      I++;
      continue;
    }
    exprs[I] = parseExpr(false);
  }

  // This is a simple expression without Range.
  if (exprs[0] && !exprs[1] && !exprs[2] && I == 0) {
    hasRange = false;
    if (isAssignExpr) {
      return builder.buildAssignmentExpr(identName, exprs[0], loc);
    }
    return exprs[0];
  }

  // :<expr>:<expr> is invalid.
  // :<expr> is valid in case statements alone. We
  // will check this in sema.
  if (!exprs[0] && exprs[1] && exprs[2]) {
    Diag.printError(loc, diag::invalid_range_expr);
    return nullptr;
  }
  hasRange = true;
  auto rangeExpr = builder.buildRangeExpr(loc, exprs[0], exprs[1], exprs[2]);
  if (!isAssignExpr) {
    return rangeExpr;
  }

  return builder.buildAssignmentExpr(identName, rangeExpr, loc);
}

ExprList Parser::parseExprList(TokenKind endTok, bool &listHasRange,
                               bool consumeCurrent) {
  ExprList exprList;

  // consume current.

  if (consumeCurrent)
    consumeToken();
  auto hasRange = false;

  while (isNot(endTok)) {
    auto expr = parseRangeExpr({tok::comma, endTok}, hasRange);
    listHasRange |= hasRange;
    if (Diag.hasError()) {
      exprList.clear();
      return exprList;
    }
    assert(expr);
    exprList.push_back(expr);
    if (is(tok::comma)) {
      consumeToken();
      continue;
    }

    if (is(endTok)) {
      break;
    }
    llvm_unreachable("unknown token encountered!");
  }
  return exprList;
}

// Build appropriate expression based on \p op.
Expr *Parser::checkAndBuildExpr(Expr *lhs, Expr *rhs, TokenKind opKind,
                                Type *type) {

  if (utils::isBinaryOp(opKind)) {

    // If one of the operands is real/ double , expr type should be real/double.
    if (lhs) {
      if (lhs->getType()->isFloatingTy() &&
          (rhs->getType()->isIntegralTy() || rhs->getType()->isFloatingTy())) {
        type = lhs->getType();
      }

      auto type1 = lhs->getType();
      auto type2 = rhs->getType();
      if (((type1->isIntegralTy() && type2->isIntegralTy()) ||
           (type1->isFloatingTy() && type2->isFloatingTy()))) {
        type = type1->getSizeInBits() > type2->getSizeInBits() ? type1 : type2;
      }
    }

    auto binOpKind = getBinaryOpKind(opKind);
    auto binaryExpr =
        builder.buildExpr(lhs, rhs, binOpKind, type, getCurrLoc());
    context.currStmtList->push_back(binaryExpr);
    return binaryExpr;
  }

  if (utils::isRelationalOp(opKind)) {
    // TODO Assert that lhs and rhs can not be relational expression
    auto relOpKind = getRelationalOpKind(opKind);
    auto relationalExpr =
        builder.buildRelationalExpr(lhs, rhs, relOpKind, getCurrLoc());
    context.currStmtList->push_back(relationalExpr);
    return relationalExpr;
  }

  if (utils::isLogicalOp(opKind)) {
    auto logicalOpKind = getLogicalOpKind(opKind);
    auto logical =
        builder.buildLogicalExpr(lhs, rhs, logicalOpKind, getCurrLoc());
    context.currStmtList->push_back(logical);
    return logical;
  }

  llvm_unreachable("Undefined Expression type to build!");
}

// We have already done a lookahead search
// to determine it is a implied do statement.
// syntax (ident,ident=init-val,end-val,incr)
QuadExpr *Parser::parseACImpliedDo() {
  consumeToken(tok::l_paren);

  auto identName = getToken().getRef();
  auto sym = getSymbolFor(identName, getCurrLoc());
  consumeToken(tok::identifier);
  consumeToken(tok::comma);

  if (getToken().getRef() != identName) {
    Diag.printError(getCurrLoc(), diag::error_in_implied_do);
    return nullptr;
  }

  consumeToken(tok::identifier);
  consumeToken(tok::equals);

  auto initExpr = parseExpr(false);
  assert(initExpr);

  consumeToken(tok::comma);

  auto endExpr = parseExpr(false);

  Expr *incrExpr = nullptr;
  if (currTokenKind == tok::comma) {
    incrExpr = parseExpr();
  } else {
    assert(is(tok::r_paren));
    incrExpr =
        builder.buildConstantVal("1", Type::getInt32Ty(FC), getCurrLoc());
  }
  consumeToken(tok::r_paren);

  auto objName = builder.buildObjectName(sym, sym->getSourceLoc());
  Expr *ops[4] = {objName, initExpr, endExpr, incrExpr};
  return builder.buildQuadExpr(ops, initExpr->getType(), sym->getSourceLoc());
}

ArrayConstructor *Parser::parseArrayConstructor() {
  // Array init.
  // (/ expr1,expr2,....    /)
  assert(is(tok::arr_start));
  auto loc = getCurrLoc();
  // consume current.
  consumeToken();

  ExprList list;
  auto endTok = tok::arr_end;

  while (isNot(endTok)) {

    Expr *expr = nullptr;
    // Check if it is the array init
    // syntax like (ident,ident=init-val,end-val,incr)
    if (is(tok::l_paren) && match({tok::identifier, tok::comma})) {
      expr = parseACImpliedDo();
    } else {
      expr = parseExpr(false);
    }
    if (Diag.hasError()) {
      list.clear();
      return nullptr;
    }
    assert(expr);
    list.push_back(expr);
    if (is(tok::comma)) {
      consumeToken();
      continue;
    }

    if (is(endTok)) {
      break;
    }
    llvm_unreachable("unknown token encountered!");
  }

  consumeToken(tok::arr_end);
  auto type = list.empty() ? nullptr : list[0]->getType();
  auto acSpec = builder.buildACSpec(type, list, loc);
  return builder.buildArrayConstructor(acSpec, loc);
}

// The internal expr-operand parser which treats each part-ref of a
// structure-component as an expr-operand. Set \p DTSymTable if the parser is
// pointing to a "non-first part-ref" of a structure-component.
Expr *Parser::_parseExprOperand(SymbolTable *DTSymTable) {

  // Parse constant.
  if (utils::isConstant(currTokenKind)) {
    auto constType = expectConstant(false);
    assert(constType);
    if (constType->isArrayTy()) {
      ArrayBounds bounds;
      bounds.first = 0;
      bounds.second = getToken().size;
      ArrBoundsList list;
      list.push_back(bounds);
      constType = ArrayType::get(FC, Type::getStringCharTy(FC), list);
    }
    auto Const =
        builder.buildConstantVal(getToken().getRef(), constType, getCurrLoc());
    consumeToken();
    return Const;
  }

  // Array init.
  // (/ expr1,expr2,....    /)
  if (currTokenKind == tok::arr_start) {
    return parseArrayConstructor();
  }

  SymbolTable *origSymTable = context.currSymTable;
  if (DTSymTable) {
    if (!DTSymTable->isAnon())
      assert(DTSymTable->getParent());
    context.currSymTable = DTSymTable;
  }

  assert(currTokenKind == tok::identifier || isKeyWordIdentifer(getToken()) ||
         isKeyWordIntrinsic(getToken()));
  auto loc = getCurrLoc();
  auto sym = getSymbolFor(getToken().getRef(), loc);
  auto type = sym->getType();
  consumeToken(); // ident.

  // If we're parsing DT-var%arr(n)%... the symbol 'n' should belong to the
  // original sym-table of context, not in DT-var's sym-table
  context.currSymTable = origSymTable;

  // Parse objectName
  if (isNot(tok::l_paren)) {

    // If the type is array
    if (type->isArrayTy()) {
      auto arrTy = static_cast<ArrayType *>(type);
      assert(!arrTy->getElementTy()->isDerivedTy());
      auto base = builder.buildObjectName(sym, loc);
      return builder.buildArraySection(sym, base, loc);
    }

    // If it is undeclared type or the scalar type itself,
    // handle it here.
    return builder.buildObjectName(sym, loc);
  }

  // At this point it should be either array / function reference
  // or array type.
  assert(type->isArrayTy() || type->isUndeclared() || type->isCharacterTy());
  bool hasRangeExpr = false;
  ExprList list = parseExprList(tok::r_paren, hasRangeExpr);

  consumeToken(tok::r_paren);

  // TODO: can list be empty if it is not an undeclared type or
  // array type.
  if (!type->isUndeclared())
    assert(!list.empty() && "Can this be empty?");

  // This should be a ArraySection. Building one.
  if (hasRangeExpr && type->isArrayTy()) {
    auto arrTy = static_cast<ArrayType *>(type);
    assert(!arrTy->getElementTy()->isDerivedTy());
    auto base = builder.buildObjectName(sym, loc);
    return builder.buildArraySection(sym, base, list, loc);
  }

  // This can be either an unresolved array reference or function call.
  // Safetly putting it as unresolved function call for now. It should be
  // resolved in sema, else throw error.
  if (type->isUndeclared()) {
    sym->setType(Type::getUndeclaredFnTy(FC));
    sym->getAttr().isSubroutine = false;
    auto funcReference =
        builder.buildFunctionReference(sym, list, sym->getType(), loc);
    context.currStmtList->push_back(funcReference);
    return funcReference;
  }

  assert(type->isArrayTy() || type->isCharacterTy());
  // auto arrTy = static_cast<ArrayType *>(type);
  // assert(!arrTy->getElementTy()->isDerivedTy());
  auto base = builder.buildObjectName(sym, loc);
  auto arrEle = builder.buildArrayElement(sym, base, list, loc);
  return arrEle;
}

Expr *Parser::parseExprOperand() {
  Expr *expr = _parseExprOperand();

  if (!is(tok::percent))
    return expr; // not a struct-comp

  ExprList partRefs;
  Expr *partRef;

  partRef = expr;
  while (is(tok::percent)) {
    partRefs.push_back(partRef);
    consumeToken(); // tok::percent

    // The symbols for each part-ref will be resolved at semantics.
    partRef = _parseExprOperand(context.anonSymTab);
  }
  partRefs.push_back(partRef);

  return builder.buildStructureComponent(partRefs, partRefs.back()->getType(),
                                         getCurrLoc());
}

// Build the expression and push it to the stack.
void Parser::pushOperation(std::stack<Expr *> &valueStack,
                           std::stack<TokenKind> &opsStack, SourceLoc loc) {
  auto op = opsStack.top();
  opsStack.pop();
  if (valueStack.empty()) {
    Diag.printError(loc, diag::unmatched_paren);
    return;
  }

  auto val2 = valueStack.top();
  valueStack.pop();
  Expr *val1 = nullptr;
  if (op != tok::l_not && op != tok::unary_minus) {
    if (valueStack.empty()) {
      Diag.printError(loc, diag::unmatched_paren);
      return;
    }
    val1 = valueStack.top();
    valueStack.pop();
  }

  auto expr = checkAndBuildExpr(val1, val2, op, val2->getType());
  valueStack.push(expr);
}

Expr *Parser::parseExpr(bool consumeCurrent) {
  if (consumeCurrent) {
    consumeToken();
  }

  std::stack<Expr *> valueStack;
  std::stack<TokenKind> opsStack;
  int paranBalanced = 0;

  SourceLoc loc = getCurrLoc();
  auto prevToken = tok::unknown;
  while (isNot(tok::eol) && isNot(tok::comma)) {
    auto currTok = currTokenKind;

    // Process (.
    if (currTok == tok::l_paren) {
      opsStack.push(currTok);
      consumeToken(); // '('
      prevToken = currTok;
      paranBalanced++;
      continue;
    }

    // Process all the operands.
    if (utils::isConstant(currTok) || currTok == tok::identifier ||
        currTok == tok::arr_start || isKeyWordIdentifer(getToken()) ||
        isKeyWordIntrinsic(getToken())) {
      auto expr = parseExprOperand();
      valueStack.push(expr);
      prevToken = currTok;
      continue;
    }

    // Process ).
    if (currTok == tok::r_paren) {
      while (!opsStack.empty() && opsStack.top() != tok::l_paren) {
        pushOperation(valueStack, opsStack, getCurrLoc());
      }

      // Consume l_paren.
      if (opsStack.empty()) {
        if (paranBalanced != 0) {
          Diag.printError(getCurrLoc(), diag::unmatched_paren);
          return nullptr;
        }
        break;
      }
      // should be l_paren now.
      assert(opsStack.top() == tok::l_paren);
      opsStack.pop();
      consumeToken(); // ')'

      // Look if the next token is operator, else we are done with processing.
      if (opsStack.empty()) {
        if (!utils::isOp(getToken().Kind)) {
          break;
        }
      }
      // continue to parse the expression.
      paranBalanced--;
      prevToken = currTok;
      continue;
    }

    // Process all the operators.
    if (utils::isOp(currTok)) {

      // Handle unary minus.
      if (currTok == tok::minus &&
          (utils::isOp(prevToken) || prevToken == tok::l_paren ||
           prevToken == tok::unknown)) {
        currTok = tok::unary_minus;
      }

      if ((currTok != tok::l_not && currTok != tok::unary_minus) &&
          prevToken == tok::l_paren) {
        Diag.printError(getCurrLoc(), diag::error_in_expr);
        return nullptr;
      }

      auto currPrecedence = getOpPrecedence(currTok);
      while (!opsStack.empty()) {

        auto stackPrecedence = getOpPrecedence(opsStack.top());
        if (stackPrecedence == currPrecedence) {
          // right to left associativity.
          if (currTok == tok::power || currTok == tok::l_and)
            break;
        } else if (stackPrecedence < currPrecedence) {
          break;
        }
        pushOperation(valueStack, opsStack, loc);
      }

      // Push the current token to ops.
      // Handle unary minus.
      opsStack.push(currTok);
      consumeToken(); // op
      prevToken = currTok;
      continue;
    }
    // Any other operator, just break;
    break;
  }

  // Build all the parsed expressions.
  while (!opsStack.empty()) {
    pushOperation(valueStack, opsStack, loc);
  }

  // Exit, on error.
  if (Diag.hasError()) {
    return nullptr;
  }
  assert(valueStack.size() == 1);
  assert(opsStack.empty());
  return valueStack.top();
}

Format *Parser::parseFormat(TokenKind eolKind) {
  SourceLoc loc = getCurrLoc();
  ExprList stringList;

  if (isNot(tok::identifier)) {
    while (is(tok::string)) {
      auto formatString = builder.buildConstantVal(
          getToken().getRef(),
          getTypeFor(currTokenKind, getToken().constantTypeSize, true),
          getCurrLoc());
      stringList.push_back(formatString);

      consumeToken();

      if (is(tok::comma)) {
        consumeToken();
        continue;
      }

      if (is(eolKind))
        break;

      llvm_unreachable("unrecognised token");
    }
  } else {
    stringList.push_back(parseExpr(false));
  }
  return builder.buildFormat(stringList, Type::getUndeclaredFnTy(FC), loc);
}
