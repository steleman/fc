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
#include "AST/ASTContext.h"
#include "AST/ASTPass.h"

#include "AST/ParseTreeBuilder.h"
#include "AST/StmtVisitor.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Diagnostics.h"
#include "sema/ExpansionUtils.h"
#include "sema/Intrinsics.h"

using namespace fc;
using namespace ast;
namespace fc {

static void getStringRef(const char *data, llvm::StringRef &msg) {
  char delim = data[0];
  data++;
  unsigned i = 0;
  while (data[i] != delim) {
    i++;
  }
  msg = llvm::StringRef(data, i);
}

class FormatHandler : public StmtVisitor<FormatHandler, bool> {
  ParseTreeBuilder builder;
  Block *currBlock{nullptr};
  SymbolTable *currSymTable{nullptr};
  Stmt *currStmt{nullptr};
  ASTContext &C;

public:
  FormatHandler(ASTContext &C) : builder(C), C(C) {}

  void setBlock(Block *b) { this->currBlock = b; }

  void setSymTable(SymbolTable *sym) { this->currSymTable = sym; }

  void setCurrStmt(Stmt *curr) { currStmt = curr; }

  ConstantVal *getConstantVal(const char *data, unsigned &size, SourceLoc loc) {
    llvm::StringRef msg;
    getStringRef(data, msg);
    size = msg.size() + 2;
    ArrayBounds bounds;
    bounds.first = 1;
    bounds.second = msg.size();
    ArrBoundsList boundsList;
    boundsList.push_back(bounds);
    auto constType = ArrayType::get(C, Type::getStringCharTy(C), boundsList);
    return builder.buildConstantVal(msg, constType, loc);
  }

  bool postWriteStmt(WriteStmt *stmt) override {
    if (!stmt->getFormat())
      return true;

    ExprList writeList = stmt->getExprList();
    unsigned k = 0;
    Format *format = stmt->getFormat();
    SourceLoc loc = stmt->getSourceLoc();
    ExprList list = format->getExprList();
    assert(list.size() == 1);
    auto expr = list[0];
    auto constantVal = llvm::dyn_cast<ConstantVal>(expr);
    assert(constantVal && "format should for string constant");
    llvm::StringRef string = constantVal->getValueRef();

    ExprList newList;
    llvm::SmallVector<int, 2> spaceList;
    const char *data = string.data();
    // unsigned length = 0;
    Block *innerMostBlock = nullptr;
    llvm::SmallVector<DoStmt *, 2> loopNest;
    while (data[0] != '\0') {
      if (data[0] == '(' || data[0] == '/' || data[0] == ',' ||
          data[0] == ')') {
        data++;
        continue;
      }

      if (data[0] == '"' || data[0] == '\'') {
        unsigned charConsumed;
        newList.push_back(getConstantVal(data, charConsumed, loc));
        spaceList.push_back(1);
        data += charConsumed;
        continue;
      }

      if (isdigit(data[0])) {
        unsigned p = 0;
        char dimCharArray[20];
        while (isdigit(data[p])) {
          dimCharArray[p] = data[p];
          p++;
        }
        dimCharArray[p] = '\0';
        data += p;
        int dimsToExpand = std::stoi(dimCharArray);

        // Only handling a specific case
        if (dimsToExpand == 81) {
          data++;
          auto impliedDo = llvm::dyn_cast<IOImpliedDo>(writeList[k]);
          k++;
          assert(impliedDo);
          ExprList exprs = impliedDo->getExprList();
          assert(exprs.size() == 1);
          Expr *operand = exprs[0];
          auto arrayElement = llvm::dyn_cast<ArrayElement>(operand);
          auto arraySym = arrayElement->getSymbol();

          auto baseExpr = builder.buildObjectName(arraySym, loc);
          auto arraySec = builder.buildArraySection(arraySym, baseExpr, loc);
          TypeList typeList;
          typeList.push_back(arraySym->getType());

          auto FuncTy = FunctionType::get(C, arraySym->getType(), typeList);
          auto transpose =
              currSymTable->getOrInsertSymbol("transpose", FuncTy, loc);
          auto result = currSymTable->getTempSymbol(arraySym->getType(), loc);
          ExprList argsList{arraySec};
          auto funcReference = builder.buildFunctionReference(
              transpose, argsList, arraySym->getType(), loc);
          auto tempBaseExpr = builder.buildObjectName(result, loc);
          auto tempArraySec =
              builder.buildArraySection(result, tempBaseExpr, loc);
          auto assignment =
              builder.buildAssignmentStmt(tempArraySec, funcReference, loc);
          currBlock->insertStmtBefore(assignment, stmt);

          newList.push_back(tempArraySec->clone());
          spaceList.push_back(0);

          unsigned i = k;
          for (; i < writeList.size(); ++i) {
            newList.push_back(writeList[k]);
            if (llvm::isa<ArraySection>(writeList[k]) ||
                llvm::isa<ArrayElement>(writeList[k])) {
              spaceList.push_back(0);
            } else {
              spaceList.push_back(1);
            }
            k++;
          }

          while (data[0] != ')')
            data++;
          continue;
        }

        // Repeat next format till dimsToExpandTimes
        if (data[0] == '(') {
          data++;
          ExprList multiExpr;
          llvm::SmallVector<int, 2> tempSpaceList;
          while (data[0] != ')') {
            if (data[0] == ',' || data[0] == ':' || data[0] == ' ') {
              data++;
              continue;
            }

            if (data[0] == '"' || data[0] == '\'') {
              unsigned charConsumed;
              multiExpr.push_back(getConstantVal(data, charConsumed, loc));
              tempSpaceList.push_back(1);
              data += charConsumed;
              continue;
            }

            // TODO Should be a recursive function
            if (isdigit(data[0])) {
              unsigned p1 = 0;
              char dimCharArray[20];
              while (isdigit(data[p1])) {
                dimCharArray[p1] = data[p1];
                p1++;
              }
              dimCharArray[p1] = '\0';
              data += p1;
              int dimsToExpand1 = std::stoi(dimCharArray);
              if (data[0] == 'i') {
                data++;
                int numSpaces;
                if (isdigit(data[0])) {
                  numSpaces = data[0] - '0';
                  data++;
                }
                for (unsigned i = 0; i < dimsToExpand1; ++i) {
                  multiExpr.push_back(nullptr);
                  tempSpaceList.push_back(numSpaces);
                }

                while (data[0] != ',')
                  data++;
                continue;
              }
            }
            if (data[0] == 'i') {
              data++;
              int numSpaces;
              if (isdigit(data[0])) {
                numSpaces = data[0] - '0';
                data++;
              }
              multiExpr.push_back(nullptr);
              tempSpaceList.push_back(numSpaces);
              while (data[0] != ',')
                data++;
              continue;
            }
            // TODO: Should be character now. Not handled!!
            data++;
          }

          assert(multiExpr.size() == tempSpaceList.size());
          for (int i = 0; i < dimsToExpand; ++i) {
            for (int j = 0; j < multiExpr.size(); ++j) {
              if (multiExpr[j]) {
                newList.push_back(multiExpr[j]);
              } else {
                newList.push_back(writeList[k++]);
              }
              spaceList.push_back(tempSpaceList[j]);
            }
          }
          data++;
          continue;
        }

        // Mostly implied do
        auto impliedDo = llvm::dyn_cast<IOImpliedDo>(writeList[k]);
        if (impliedDo && writeList.size() == 1) {
          k++;

          // consume till ',' or ')'

          data++;
          if (data[0] == 'i')
            data++;
          int numSpaces = 1;
          if (isdigit(data[0])) {
            numSpaces = data[0] - '0';
            data++;
          }
          while (data[0] != ',' && data[0] != ')')
            data++;

          // Temporary fix!
          if (impliedDo->getExprList().size() != 1)
            return true;

          Expr *operand = impliedDo->getExprList()[0];
          auto arrayElement = llvm::dyn_cast<ArrayElement>(operand);
          ExprList exprList = impliedDo->getImpliedDos();
          assert(exprList.size() == 2 && "Only handling 2d");
          auto quadExpr1 = llvm::dyn_cast<QuadExpr>(exprList[0]);
          auto quadExpr2 = llvm::dyn_cast<QuadExpr>(exprList[1]);
          assert(quadExpr1 && quadExpr2);
          auto stride1Const =
              llvm::dyn_cast<ConstantVal>(quadExpr1->getOperand(3));

          auto lb1Const = llvm::dyn_cast<ConstantVal>(quadExpr1->getOperand(1));

          auto ub1Const = llvm::dyn_cast<ConstantVal>(quadExpr1->getOperand(2));
          auto objName1 = llvm::dyn_cast<ObjectName>(quadExpr1->getOperand(0));

          assert(lb1Const && ub1Const && stride1Const && objName1);

          auto firstSymName = objName1->getSymbol()->getName();
          auto lb1 = lb1Const->getInt();
          auto ub1 = ub1Const->getInt();
          auto stride1 = stride1Const->getInt();

          ExprList subsList = arrayElement->getSubscriptList();
          auto int32 = Type::getInt32Ty(C);
          assert(ub1 - lb1 + 1 == dimsToExpand && stride1 == 1);
          for (int i = lb1; i <= ub1; i += stride1) {

            ExprList newSubscripts;
            for (unsigned j = 0; j < subsList.size(); ++j) {
              auto subObject = llvm::dyn_cast<ObjectName>(subsList[j]);
              assert(subObject);
              if (subObject->getName() == firstSymName) {
                newSubscripts.push_back(
                    builder.buildConstantVal(std::to_string(i), int32, loc));
              }

              else {
                newSubscripts.push_back(subObject->clone());
              }
            }

            auto newArrayEleBase =
                builder.buildObjectName(arrayElement->getSymbol(), loc);
            newList.push_back(
                builder.buildArrayElement(arrayElement->getSymbol(),
                                          newArrayEleBase, newSubscripts, loc));
            spaceList.push_back(numSpaces);
          }

          if (innerMostBlock)
            continue;

          DynArrBoundsList boundsList;
          ExprList strideList;
          SymbolList indVarList;

          for (int i = 1; i < exprList.size(); ++i) {
            auto quadExpr = llvm::dyn_cast<QuadExpr>(exprList[i]);
            assert(quadExpr);
            strideList.push_back(quadExpr->getOperand(3));
            boundsList.push_back(std::make_pair(quadExpr->getOperand(1),
                                                quadExpr->getOperand(2)));
            auto objName = llvm::dyn_cast<ObjectName>(quadExpr->getOperand(0));
            indVarList.push_back(objName->getSymbol());
          }
          loopNest =
              builder.buildLoopNestFor(indVarList, boundsList, strideList, loc);

          innerMostBlock = loopNest[loopNest.size() - 1]->getBlock();
        }
        continue;
      }

      // llvm::errs() << "unhanled " << data[0];
      data++;
    }

    for (unsigned j = k; j < writeList.size(); ++j) {
      newList.push_back(writeList[j]);
      spaceList.push_back(1);
    }

    assert(newList.size() == spaceList.size());

    auto newWriteStmt =
        builder.buildWriteStmt(newList, loc, stmt->getUnit(), nullptr,
                               stmt->getAdvance(), stmt->getIostat());

    if (!innerMostBlock) {
      currBlock->replaceWith(newWriteStmt, stmt);
    } else {
      innerMostBlock->addStmt(newWriteStmt);
      currBlock->replaceWith(loopNest[0], stmt);
    }

    newWriteStmt->setSpaceList(spaceList);
    return true;
  }
};

class FormatHandlerPass : public ASTBlockPass {
  FormatHandler expander;

public:
  FormatHandlerPass(ASTContext &C)
      : ASTBlockPass(C, "Format handling pass"), expander(C) {}

  bool runOnBlock(Block *block) override {
    expander.setBlock(block);
    expander.setSymTable(currPU->getSymbolTable());

    auto &stmtList = block->getStmtList();

    for (auto stmt : stmtList) {
      expander.setCurrStmt(stmt);
      if (!expander.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
};

ASTPass *createFormatHandlerPass(ASTContext &C) {
  return new FormatHandlerPass(C);
}
} // namespace fc
