#include <AST/ASTContext.h>
#include <AST/ASTPass.h>
#include <AST/Declaration.h>
#include <AST/ParseTreeBuilder.h>
#include <AST/ProgramUnit.h>
#include <AST/Statements.h>

using namespace fc;
using namespace ast;

namespace fc {
class DeclEliminatorPass : public ASTPUPass {
public:
  ASTContext &context;
  ParseTreeBuilder builder;
  ProgramUnit *currPU;

  DeclEliminatorPass(ASTContext &C)
      : ASTPUPass(C, "Declaration eliminator pass"), context(C), builder(C) {}

  // Creates a allocate call when the arraySpec value is dynamic for local
  // arrays.
  void createDynamicArrays(StmtVecList &newStmts, EntityDecl *entityDecl) {
    auto sym = entityDecl->getSymbol();
    if (sym->getParentSymbol() || sym->hasIntent() ||
        !sym->getType()->isArrayTy())
      return;

    auto arrTy = llvm::cast<ArrayType>(sym->getType());
    if (!arrTy->boundsEmpty()) {
      return;
    }
    auto arrSpec = entityDecl->getArraySpec();
    auto boundsList = arrSpec->getBoundsList();
    if (boundsList.empty()) {
      return;
    }

    SymbolList symList{sym};
    ArraySpecList arrSpecList{arrSpec};

    auto allocate = builder.buildAllocateStmt(symList, arrSpecList,
                                              entityDecl->getSourceLoc());
    newStmts.push_back(allocate);
  }

  // Check if array spec has all the dimensions.
  bool isFullArraySpec(ArraySpec *spec) {
    if (!spec) {
      return false;
    }
    auto boundsList = spec->getBoundsList();
    if (boundsList.empty())
      return false;
    for (auto bounds : boundsList) {
      if (bounds.first == nullptr || bounds.second == nullptr) {
        return false;
      }
    }
    return true;
  }

  void splitDecls(EntityDeclList *entityDecls) {
    StmtVecList newStmts;

    ExecutionPart *currExecPart = currPU->getExecPart();
    // TODO: How to handle empty-exec-part fortran programs ? We can build an
    // ExecutionPart here, but we usually add ExecutionPart to ProgramUnit by
    // it's ctor.
    if (!currExecPart) {
      return;
    }

    for (EntityDecl *entityDecl : *entityDecls) {
      Expr *initExpr = entityDecl->getInit();
      if (initExpr) {
        SourceLoc currLoc = entityDecl->getSourceLoc();
        ObjectName *lhs =
            builder.buildObjectName(entityDecl->getSymbol(), currLoc);

        AssignmentStmt *newAssignmentStmt =
            builder.buildAssignmentStmt(lhs, initExpr, currLoc);
        newStmts.push_back(newAssignmentStmt);
        entityDecl->setInit(nullptr);
      }

      createDynamicArrays(newStmts, entityDecl);

      // Keep the array spec in specification if it is not empty.
      // We will use to it emit the bounds.
      auto arrSpec = entityDecl->getArraySpec();
      auto sym = entityDecl->getSymbol();
      if (arrSpec && sym->getType()->isDynArrayTy() &&
          isFullArraySpec(arrSpec)) {
        currPU->getSpec()->setarrSpecMap(sym, arrSpec);
      }
    }

    Stmt *insertPoint = currExecPart->getBlock()->getStmtList().front();
    for (auto newStmt : newStmts) {
      currExecPart->getBlock()->insertStmtBefore(newStmt, insertPoint);
    }
  }

  void eliminateDecls(EntityDeclList *entityDecls) {
    for (EntityDecl *entityDecl : *entityDecls) {
      currPU->getSpec()->getBlock()->removeStmt(entityDecl);
    }
  }

  bool hasGlobalVars(EntityDeclList *entityDecls) {
    for (EntityDecl *entityDecl : *entityDecls) {
      if (entityDecl->getSymbol()->isStaticGlobalAlloc())
        return true;
    }
    return false;
  }

  bool runOnProgramUnit(ProgramUnit *PU) {
    SpecificationPart *specPart = PU->getSpec();
    EntityDeclList entityDecls;
    currPU = PU;

    for (Stmt *stmt : specPart->getBlock()->getStmtList()) {
      if (auto entityDecl = llvm::dyn_cast<EntityDecl>(stmt)) {
        entityDecls.push_back(entityDecl);
      }
    }

    if (entityDecls.size() == 0)
      return true;

    if (hasGlobalVars(&entityDecls))
      return true;

    splitDecls(&entityDecls);
    eliminateDecls(&entityDecls);
    return true;
  }
};

ASTPass *createDeclEliminatorPass(ASTContext &C) {
  return new DeclEliminatorPass(C);
}

} // end namespace fc
