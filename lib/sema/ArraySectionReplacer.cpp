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

class ArraySectionReplacer : public StmtVisitor<ArraySectionReplacer, bool> {
  ParseTreeBuilder builder;
  Block *currBlock{nullptr};
  SymbolTable *currSymTable{nullptr};
  Stmt *currStmt{nullptr};
  ASTContext &C;

public:
  ArraySectionReplacer(ASTContext &C) : builder(C), C(C) {}

  void setBlock(Block *b) { this->currBlock = b; }

  void setSymTable(SymbolTable *sym) { this->currSymTable = sym; }

  void setCurrStmt(Stmt *curr) { currStmt = curr; }

  // Create extra array symbol for arraysections in function call
  bool postFunctionReference(FunctionReference *ref) override {
    auto loc = ref->getSourceLoc();
    auto sym = ref->getSymbol()->getOrigSymbol();
    std::map<Expr *, ArraySection *> argSectionMap;

    // Intrinsic expander should take care of this
    if (intrin::isIntrinsic(sym->getName()))
      return true;

    auto functionType = llvm::dyn_cast<FunctionType>(sym->getType());

    // If the functin type is not known can't replace
    if (!functionType)
      return true;

    auto args = ref->getArgsList();
    ArraySectionHelper helper(builder, currSymTable, C);
    auto I32 = Type::getInt32Ty(C);
    auto one = builder.getConstantOne(loc, I32);

    class ArrSecVisitor : public StmtVisitor<ArrSecVisitor, bool> {
    public:
      llvm::SmallVector<ArraySection *, 2> arraySectionList;
      bool postArraySection(ArraySection *section) {
        arraySectionList.push_back(section);
        return true;
      }
    };

    SymbolList symList;
    ArraySpecList specList;
    StmtVecList stmtList;
    ArrSecVisitor visitor;

    for (unsigned i = 0; i < args.size(); ++i) {
      visitor.arraySectionList.clear();
      visitor.visit(args[i]);
      for (auto arraySection : visitor.arraySectionList) {
        if (arraySection->isFullRange())
          continue;

        auto argType = functionType->getArgType(i);
        auto arrayType = llvm::dyn_cast<ArrayType>(argType);

        if (!arrayType)
          continue;

        RangeInfoList infoList;
        helper.computeArrSecRangeInfo(arraySection, infoList, false);
        DynArrBoundsList resultBounds;
        for (auto rangeInfo : infoList) {
          if (rangeInfo.isRange()) {
            resultBounds.push_back(rangeInfo.bounds);
          } else {
            resultBounds.push_back(std::make_pair(one, one));
          }
        }
        auto resultTy =
            ArrayType::get(C, arrayType->getElementTy(), resultBounds.size());
        auto resultSym = currSymTable->getTempSymbol(resultTy, loc);

        auto arraySpec =
            builder.buildArraySpec(resultBounds, resultBounds.size(), loc);
        symList.push_back(resultSym);
        specList.push_back(arraySpec);

        auto resultObj = builder.buildObjectName(resultSym, loc);
        auto resultSection =
            builder.buildArraySection(resultSym, resultObj, loc);

        if (argSectionMap.find(args[i]) == argSectionMap.end())
          argSectionMap[args[i]] = resultSection;

        arraySection->replaceWith(resultSection);
        stmtList.push_back(builder.buildAssignmentStmt(resultSection->clone(),
                                                       arraySection, loc));
      }
    }

    if (symList.size() != 0) {
      auto allocateStmt = builder.buildAllocateStmt(symList, specList, loc);
      stmtList.insert(stmtList.begin(), allocateStmt);
      for (auto stmt : stmtList) {
        currBlock->insertStmtBefore(stmt, currStmt);
      }
    }

    // Now check if any argument is expression with array section in it
    // if so replace it. Because ArraySectionExpander doesn't
    // expand such array sections
    for (unsigned i = 0; i < args.size(); ++i) {
      if (argSectionMap.find(args[i]) == argSectionMap.end())
        continue;

      if (llvm::isa<ArraySection>(args[i]))
        continue;

      auto arraySection = argSectionMap[args[i]];
      assert(arraySection->isFullRange() &&
             "All partial sections are replaced earlier");
      args[i]->replaceWith(arraySection->clone());
      auto assignmentStmt =
          builder.buildAssignmentStmt(arraySection->clone(), args[i], loc);
      currBlock->insertStmtBefore(assignmentStmt, currStmt);
    }

    auto returnArrayTy =
        llvm::dyn_cast<ArrayType>(functionType->getReturnType());
    if (!returnArrayTy)
      return true;

    if (!returnArrayTy->isDynArrayTy())
      return true;

    auto returnSym = currSymTable->getTempSymbol(returnArrayTy, loc);
    auto returnObj = builder.buildObjectName(returnSym, loc);
    auto returnSection = builder.buildArraySection(returnSym, returnObj, loc);
    ref->replaceWith(returnSection);
    auto assign = builder.buildAssignmentStmt(returnObj, ref, loc);
    currBlock->insertStmtBefore(assign, currStmt);

    return true;
  }
};

class ArraySectionReplacerPass : public ASTBlockPass {
  ArraySectionReplacer replacer;

public:
  ArraySectionReplacerPass(ASTContext &C)
      : ASTBlockPass(C, "ArraySection Replacer"), replacer(C) {}

  bool runOnBlock(Block *block) override {
    replacer.setBlock(block);
    replacer.setSymTable(currPU->getSymbolTable());

    auto &stmtList = block->getStmtList();

    for (auto stmt : stmtList) {
      replacer.setCurrStmt(stmt);
      if (!replacer.visit(stmt)) {
        return false;
      }
    }
    return true;
  }
};

ASTPass *createArraySectionReplacerPass(ASTContext &C) {
  return new ArraySectionReplacerPass(C);
}
} // namespace fc
