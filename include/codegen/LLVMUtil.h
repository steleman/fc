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