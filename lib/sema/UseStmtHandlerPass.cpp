#include "AST/ASTContext.h"
#include "AST/ASTPass.h"
#include "AST/Declaration.h"
#include "AST/ParseTreeBuilder.h"
#include "AST/ProgramUnit.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Debug.h"
#include "common/Diagnostics.h"
#include "sema/ModFileHandler.h"

#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/StringRef.h"

#include <algorithm>

using namespace fc;
using namespace ast;
namespace fc {

class UseStmtHandler : public ASTProgramPass {
  SymbolTableSet set;
  llvm::SmallPtrSet<Module *, 2> moduleSet;
  ParseTree *parseTree;

  bool isUseHandled(SymbolTable *symTable) {
    // Use already processed.
    return (set.count(symTable) != 0);
  }

  Module *getModuleFor(llvm::StringRef name) {
    for (auto PU : parseTree->getProgramUnitList()) {
      if (name != PU->getName()) {
        continue;
      }
      auto module = llvm::dyn_cast<Module>(PU);
      assert(module);
      return module;
    }
    return nullptr;
  }

  bool importAllSymbolsFromUsedModules(Module *module) {
    auto currModuleTable = module->getSymbolTable();
    auto spec = module->getSpec();
    if (!spec)
      return true;
    auto UseStmts = spec->getUseStmts();
    if (UseStmts.empty())
      return true;

    for (auto useStmt : UseStmts) {

      auto modSymTable = useStmt->getSymbolTable();
      if (!isUseHandled(modSymTable)) {
        Context.Diag.printError(useStmt->getSourceLoc(),
                                diag::mutual_module_use);
        return false;
      }

      // Import each symbol from use stmt module to the current module symbol
      // table.
      for (auto sym : modSymTable->getSymbolList()) {
        auto symName = sym->getName();
        auto currSym = currModuleTable->getSymbol(symName);

        if (currSym && currSym->getType()->isUndeclaredTy()) {
          currSym->setParentSymbol(sym);
          currSym->setType(sym->getType());
          auto modName = sym->getOriginalModName();
          currSym->setOrigModName(modName);
          auto attrs = sym->getAttr();
          currSym->setAttributes(attrs);
          continue;
        }

        if (currSym &&
            sym->getOriginalModName() == currSym->getOriginalModName()) {
          // Symbol already include via use of actual module and current module
          // also uses actual parent module of this symbol! Hence symbol is part
          // of both modules.
          continue;
        }

        if (currSym != nullptr) {
          llvm::errs() << "Symbol " << currSym->getName() << "\n";
          Context.Diag.printError(currSym->getSourceLoc(), diag::dup_variable);
          Context.Diag.printError(useStmt->getSourceLoc(),
                                  diag::var_decl_in_use_stmt);
          return false;
        }

        auto newSym =
            currModuleTable->addNewSymbol(sym->getName(), sym->getType(),
                                          sym->getSourceLoc(), sym->getAttr());
        newSym->setParentSymbol(sym);
        auto modName = sym->getOriginalModName();
        newSym->setOrigModName(modName);
      }
    }
    return true;
  }

  bool processModule(Module *module) {
    // Handle the current module.
    if (!handleProgramUnit(module)) {
      return false;
    }
    // Now that the current module is handled.
    // Import all the types from used modules
    // and keep it.
    if (!importAllSymbolsFromUsedModules(module)) {
      return false;
    }

    // Now dump the module.
    if (!ModFileHandler::dumpModFile(module)) {
      return false;
    }
    moduleSet.insert(module);
    return true;
  }

  // Handle \p useStmt in \p PU. TODO. merge this function with the
  // handleUseStmt() function.
  bool handleModule(UseStmt *useStmt, ProgramUnit *PU) {

    SymbolTable *symTable = useStmt->getSymbolTable();
    // If module is found in current program unit,
    // first dump the mod file.
    auto module = getModuleFor(symTable->getName());
    if (module) {
      if (!processModule(module))
        return false;
    }

    // Do a check before reading if module is already handled
    // as part of other program units.
    if (isUseHandled(symTable))
      return true;

    // FIXME This assert is added to demonstrate that we're creating a symtab
    // without PU when we're importing from .mod file. Are we good with this ?
    // Some passes might fetch a Symbol's PU by
    // sym->getSymboltable()->getProgramUnit() which would be nullptr!, which
    // doesn't seem natural
    /* assert(symTable->getProgramUnit()); */

    // Read from the module file.
    std::string InputFile = symTable->getName() + ".mod";
    std::transform(InputFile.begin(), InputFile.end(), InputFile.begin(),
                   ::tolower);
    bool success = ModFileHandler::readModFile(InputFile, useStmt, PU, Context);
    if (!success) {
      error() << "\n error reading mod file " << InputFile << "\n";
      Context.Diag.printError(useStmt->getSourceLoc(), diag::mod_file_error);
      return false;
    }
    symTable->setLoadedFromModFile(true);
    return true;
  }

  bool handleUseStmt(UseStmt *useStmt, ProgramUnit *PU) {
    auto symTable = useStmt->getSymbolTable();
    if (isUseHandled(symTable)) {
      return true;
    }

    if (!handleModule(useStmt, PU))
      return false;

    set.insert(symTable);
    return true;
  }

  bool handleIntrinsicModule(UseStmt *useStmt, SymbolTable *symTable) {
    assert(useStmt->getSymbolTable()->getName() == "iso_fortran_env");
    if (isUseHandled(useStmt->getSymbolTable()))
      return true;

    set.insert(useStmt->getSymbolTable());

    bool isLocal = true;

    SourceLoc loc;
    // This has to be decided on only list
    auto errorUnit = symTable->getOrInsertSymbol(
        "error_unit", Type::getInt32Ty(Context), loc);

    if (symTable->isModuleScope() || symTable->isGlobalScope())
      isLocal = false;

    errorUnit->setType(Type::getInt32Ty(Context));
    errorUnit->setConstantAttribute();
    errorUnit->setAllocatableAttribute();

    AllocationKind kind;
    if (isLocal)
      kind = AllocationKind::StaticLocal;
    else
      kind = AllocationKind::StaticGlobal;

    errorUnit->setAllocKind(kind);

    ParseTreeBuilder builder(Context);

    auto zero = builder.buildConstantVal("0", Type::getInt32Ty(Context), loc);
    errorUnit->setInitConstant(zero->getConstant());
    return true;
  }

  bool handleProgramUnit(ProgramUnit *PU) {
    auto spec = PU->getSpec();
    if (!spec)
      return true;
    auto UseStmts = spec->getUseStmts();
    if (UseStmts.empty())
      return true;

    for (auto useStmt : UseStmts) {
      if (useStmt->isIntrinsic()) {
        if (!handleIntrinsicModule(useStmt, PU->getSymbolTable()))
          return false;
        continue;
      }

      auto symTable = useStmt->getSymbolTable();
      if (PU->hasUsedSymbolTable(symTable)) {
        continue;
      }
      if (!handleUseStmt(useStmt, PU))
        return false;

      PU->addUsedSymTable(symTable);

      DerivedTypeDefList useStmtDTDs = useStmt->getDTDs();
      Module *useStmtMod = getModuleFor(useStmt->getModuleName());

      if (!useStmtMod->getDTDs().empty()) {
        for (DerivedTypeDef *dtd : useStmtMod->getDTDs()) {
          PU->addDTD(dtd);
        }

      } else {
        for (DerivedTypeDef *dtd : useStmtDTDs) {
          PU->addDTD(dtd);
        }
      }
    }
    return true;
  }

public:
  bool handleProgramUnitRecurse(ProgramUnit *PU) {

    if (!handleProgramUnit(PU)) {
      return false;
    }

    assert((!PU->isModule() ||
            PU->getParent()->getKind() == ProgramUnitKind::ProgramKind) &&
           "modules as subprogram are not handled");

    for (auto subPU : PU->getProgramUnitList()) {
      if (!handleProgramUnitRecurse(subPU)) {
        return false;
      }
    }
    return true;
  }

  virtual bool runOnProgram(ParseTree *parseTree) override {
    this->parseTree = parseTree;
    // Walk over all the Program units and see if it has an uses,
    // A. If it has use,
    // For every use stmt do the following.
    // 1. If the module referred is in current program unit. emit module
    // file
    // 2. If not in current program, try to read the file.
    // B. If the the current program unit is module, import all the used module
    // types in current.
    // C. Resolve the undeclared symbols in the current program unit.

    for (auto PU : parseTree->getProgramUnitList()) {
      if (!handleProgramUnitRecurse(PU)) {
        return false;
      }
    }

    // Dump the remaining modules which are not procesed yet.
    for (auto PU : parseTree->getProgramUnitList()) {
      if (!PU->isModule())
        continue;

      auto module = static_cast<Module *>(PU);
      if (moduleSet.count(module) != 0)
        continue;

      if (!processModule(module)) {
        return false;
      }
    }
    return true;
  }
  explicit UseStmtHandler(ASTContext &C)
      : ASTProgramPass(C, "Use Stmt Handler Pass") {}
};

ASTPass *createUseStmtHandlerPass(ASTContext &C) {
  return new UseStmtHandler(C);
}

} // namespace fc
