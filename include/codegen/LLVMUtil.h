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
#ifndef FC_CG_LLVM_UTILS_H
#define FC_CG_LLVM_UTILS_H

#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"

using namespace llvm;

class LLVMUtil {
public:
  static llvm::Type *getLLVMArrayBaseType(llvm::Type *type) {
    if (type->isPointerTy())
      type = type->getPointerElementType();

    if (!type->isArrayTy()) {
      assert(false);
      return nullptr;
    }
    while (type->isArrayTy()) {
      type = type->getArrayElementType();
    }
    return type;
  }

  // returns nullptr, if empty.
  static llvm::Instruction *getFirstNonAllocaInst(llvm::BasicBlock *BB) {
    assert(BB);
    if (BB->empty())
      return nullptr;
    for (auto &I : *BB) {
      if (!llvm::isa<llvm::AllocaInst>(&I))
        return &I;
    }
    return nullptr;
  }

  static llvm::StructType *getStructTypeForAlloc(llvm::Value *Val) {
    auto Ty = getTypeForAlloc(Val);
    if (!Ty->isStructTy())
      return nullptr;
    return cast<StructType>(Ty);
  }

  static llvm::Type *getTypeForAlloc(llvm::Value *Val) {
    assert(Val);
    llvm::Type *Ty = nullptr;
    if (auto AI = llvm::dyn_cast<llvm::AllocaInst>(Val)) {
      Ty = AI->getAllocatedType();
    } else if (auto globalVar = llvm::dyn_cast<GlobalVariable>(Val)) {
      Ty = globalVar->getValueType();
    } else if (auto arg = llvm::dyn_cast<llvm::Argument>(Val)) {
      Ty = arg->getType();
      if (Ty->isPointerTy()) {
        Ty = Ty->getPointerElementType();
      }
    } else if (auto arg = llvm::dyn_cast<llvm::LoadInst>(Val)) {
      Ty = Val->getType();
      if (Ty->isPointerTy()) {
        Ty = Ty->getPointerElementType();
      }
    } else {
      errs() << *Val;
      assert(false && "unknown llvm value type.");
    }

    return Ty;
  }
};

#endif