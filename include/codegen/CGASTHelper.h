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
#ifndef FC_CG_AST_HELPER_H
#define FC_CG_AST_HELPER_H
#include "AST/ParserTreeCommon.h"
#include "common/Source.h"
#include "llvm/IR/GlobalValue.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"

#include <map>

namespace fc {

class ArrayType;

using namespace ast;

// Helper calss for conversoin from AST types/ names to LLVM IR.
class CGASTHelper {
private:
  ast::ParseTree *parseTree;
  llvm::Module *TheModule;
  ASTContext &C;
  llvm::LLVMContext &LLC;
  llvm::IRBuilder<> *IRB;
  std::map<fc::ArrayType *, llvm::StructType *> dynArrMap;

  std::map<fc::StructType *, llvm::StructType *> structTypeMap;

  // Map to track functions and their return global value
  std::map<std::string, llvm::Constant *> FuncRetMap;

  // Map to keep track of orinal PU name emitted PU name
  std::map<std::string, std::string> PUNameMap;

  // This structure is to help emission of
  // nested subroutines.
  struct SubPUHelper {
    // structure type of the frame argument.
    llvm::StructType *frameTy;
    // Whether there is any frame arg.
    bool hasFrameArg;
    // What are the symbols to be passes to the
    // nested routines.
    SymbolSet set;

    SubPUHelper() : frameTy(nullptr), hasFrameArg(false) {}

    SubPUHelper(const SubPUHelper &other) {
      this->frameTy = other.frameTy;
      this->hasFrameArg = other.hasFrameArg;
      this->set = other.set;
    }
  };

  std::map<ProgramUnit *, SubPUHelper> subPUHelperMap;

  // Fortran standard
  Standard std;

public:
  explicit CGASTHelper(ast::ParseTree *tree, llvm::Module *module,
                       llvm::IRBuilder<> *IRB, Standard std);
  llvm::Type *getLLVMTypeFor(fc::Type *type);

  llvm::Type *getLLVMTypeFor(Symbol *symbol);

  llvm::Type *getLLVMTypeFor(ProgramUnit *PU, bool &hasFrameArg);

  std::string getNameForProgramUnit(ProgramUnit *PU);

  llvm::GlobalValue::LinkageTypes getLinkageTypeFor(ProgramUnit *PU);

  llvm::Function *emitDeclarationFor(ProgramUnit *sub);

  std::string getFunctionNameForSymbol(Symbol *symbol);

  std::string getGlobalSymbolName(Symbol *symbol);

  SymbolList getUsedSymbolsInChildren();

  llvm::StructType *getLLVMStructTypeFor(llvm::StringRef name, SymbolSet &set);

  SubPUHelper *getSubPUHelper(ProgramUnit *PU);

  ProgramUnit *getCalledProgramUnit(Symbol *symbol);

  llvm::Type *getLLVMTypeForDynArray(fc::ArrayType *arrTy);

  std::string getEmittedNameForPU(std::string name);

  llvm::Value *getReturnValueFor(std::string fun);

  llvm::Function *getMallocFunction();

  llvm::Function *getFreeFunction();

  unsigned getSizeForType(Type *Ty);

  llvm::Value *getSExt(llvm::Value *V);
};
} // namespace fc

#endif
