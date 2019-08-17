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
#include "AST/ASTPass.h"
#include "common/Debug.h"

#include "AST/ParseTreeBuilder.h"
#include "AST/ParserTreeCommon.h"
#include "AST/ProgramUnit.h"
#include "AST/Statements.h"
#include "llvm/Support/CommandLine.h"

llvm::cl::opt<bool>
    PrintAfterAll("fc-print-after-all",
                  llvm::cl::desc("Print the AST after all the passes"),
                  llvm::cl::init(false));

using namespace fc;
using namespace ast;

ASTPassManager::ASTPassManager(std::unique_ptr<ParseTree> &parseTree,
                               llvm::StringRef name)
    : parseTree(parseTree), name(name) {
  builder = std::make_unique<ParseTreeBuilder>(parseTree->getContext());
}

static bool handleProgramUnitRecurse(ASTPUPass *pass, ProgramUnit *PU) {

  if (!pass->runOnProgramUnit(PU)) {
    return false;
  }

  for (auto subPU : PU->getProgramUnitList()) {
    if (!handleProgramUnitRecurse(pass, subPU)) {
      return false;
    }
  }
  return true;
}

static bool runOnBlockRecurse(ASTBlockPass *pass, Block *block);

// Add all the possible instructions which has
// blocks in it.
static bool runSubBlocks(ASTBlockPass *pass, Block *block) {
  for (auto stmt : block->getStmtList()) {
    auto stmtKind = stmt->getStmtType();
    switch (stmtKind) {
    case StmtType::DoWhileStmtKind: {
      auto whileStmt = static_cast<DoWhileStmt *>(stmt);
      if (!runOnBlockRecurse(pass, whileStmt->getBlock())) {
        return false;
      }
      break;
    }
    case StmtType::DoStmtKind: {
      auto doStmt = static_cast<DoStmt *>(stmt);
      if (!runOnBlockRecurse(pass, doStmt->getBlock())) {
        return false;
      }
      break;
    }

    case StmtType::IfElseStmtKind: {
      auto ifElseStmt = static_cast<IfElseStmt *>(stmt);
      for (unsigned I = 0; I < ifElseStmt->getNumOperands(); ++I) {
        auto ifStmt = ifElseStmt->getIfStmt(I);
        if (!runOnBlockRecurse(pass, ifStmt->getBlock())) {
          return false;
        }
      }
      break;
    }

    case StmtType::SelectCaseStmtKind: {
      auto selectStmt = static_cast<fc::ast::SelectCaseStmt *>(stmt);
      auto caseStmtList = selectStmt->getCaseStmtList();
      for (auto caseStmt : caseStmtList) {
        if (!runOnBlockRecurse(pass, caseStmt->getBlock())) {
          return false;
        }
      }
      break;
    }

    case StmtType::WhereConstructKind: {
      auto whereConstruct = static_cast<WhereConstruct *>(stmt);
      for (unsigned i = 0; i < whereConstruct->getNumOperands(); ++i) {
        auto whereStmt = whereConstruct->getWhereStmt(i);
        if (!runOnBlockRecurse(pass, whereStmt->getBlock()))
          return false;
      }
      break;
    }

    case StmtType::ForAllStmtKind: {
      auto forAllConstruct = static_cast<ForAllStmt *>(stmt);
      if (!runOnBlockRecurse(pass, forAllConstruct->getBlock()))
        return false;
      break;
    }

    case StmtType::BlockKind: {
      auto subBlock = static_cast<Block *>(stmt);
      if (!runOnBlockRecurse(pass, subBlock)) {
        return false;
      }
      break;
    }
    default:
      break;
    };
  }
  return true;
}

static bool runOnBlockRecurse(ASTBlockPass *pass, Block *block) {
  if (!block)
    return true;

  if (!pass->runOnBlock(block)) {
    return false;
  }

  if (!runSubBlocks(pass, block)) {
    return false;
  }
  return true;
}

static bool handleProgramUnitRecurse(ASTBlockPass *pass, ProgramUnit *PU) {

  pass->setProgramUnit(PU);

  if (PU->getSpec()) {
    auto block = PU->getSpec()->getBlock();
    if (!runOnBlockRecurse(pass, block)) {
      return false;
    }
  }

  if (PU->getExecPart()) {
    auto block = PU->getExecPart()->getBlock();
    if (!runOnBlockRecurse(pass, block)) {
      return false;
    }
  }

  for (auto subPU : PU->getProgramUnitList()) {
    if (!handleProgramUnitRecurse(pass, subPU)) {
      return false;
    }
  }
  return true;
}

// First traverse the current program unit and then
// recursively traverse all the sub programs.
bool ASTPassManager::runOnProgramUnit(ASTPUPass *pass) {
  for (auto PU : parseTree->getProgramUnitList()) {
    if (!handleProgramUnitRecurse(pass, PU)) {
      return false;
    }
  }

  return true;
}

bool ASTPassManager::runOnProgramUnit(ASTBlockPass *pass) {
  for (auto PU : parseTree->getProgramUnitList()) {
    if (!handleProgramUnitRecurse(pass, PU)) {
      return false;
    }
  }
  return true;
}

bool ASTPassManager::runOnModule(ASTModulePass *pass) {
  for (auto PU : parseTree->getProgramUnitList()) {
    if (!PU->isModule())
      continue;
    if (!pass->runOnModule(static_cast<Module *>(PU)))
      return false;
  }
  return true;
}

void ASTPassManager::addPass(ASTPass *pass) { passList.push_back(pass); }

ASTContext &ASTPassManager::getContext() { return parseTree->getContext(); }

bool ASTPassManager::run() {
  FC_DEBUG((debug() << "Running " << getName() << " Pass Manager"
                    << "\n\n"));
  for (auto pass : passList) {
    FC_DEBUG((debug() << "*** Running " << pass->getName() << " ***\n\n"));
    switch (pass->getPassKind()) {
    case ASTPass::ProgramPassKind: {
      auto programPass = static_cast<ASTProgramPass *>(pass);
      if (!programPass->runOnProgram(parseTree.get()))
        return false;
      break;
    }

    case ASTPass::ProgramUnitPassKind: {
      auto programUnitPass = static_cast<ASTPUPass *>(pass);
      if (!runOnProgramUnit(programUnitPass))
        return false;
      break;
    }

    case ASTPass::ModulePassKind: {
      auto programUnitPass = static_cast<ASTModulePass *>(pass);
      if (!runOnModule(programUnitPass))
        return false;
      break;
    }

    case ASTPass::BlockPassKind: {
      auto blockPass = static_cast<ASTBlockPass *>(pass);
      if (!runOnProgramUnit(blockPass)) {
        return false;
      }
      break;
    }
    default:
      llvm_unreachable("Unknown pass kind");
    };

    if (PrintAfterAll) {
      FC_DEBUG(debug() << "*** Dumping AST after pass " << pass->getName()
                       << "\n");
      FC_DEBUG(parseTree->dump(debug(), 0));
    }
  }

  return true;
}

ASTPassManager::~ASTPassManager() {
  for (auto pass : passList) {
    delete pass;
  }
}
