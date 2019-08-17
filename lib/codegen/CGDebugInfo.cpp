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
#include "codegen/CGDebugInfo.h"
#include "AST/SymbolTable.h"
#include "common/Source.h"

using namespace fc;
using namespace llvm;

llvm::DIType *CGDebugInfo::getDIType(fc::Type *type) {
  return getDIType(cgHelper->getLLVMTypeFor(type));
}

llvm::DebugLoc CGDebugInfo::getLoc(SourceLoc loc, DIScope *scope) {

  if (!scope) {
    scope = ScopeStack.back();
  }
  return llvm::DebugLoc::get(loc.Line, loc.Col, scope);
}

llvm::DIType *CGDebugInfo::getDIType(llvm::Type *type) {
  switch (type->getTypeID()) {
  case llvm::Type::IntegerTyID: {
    return builder->createBasicType("integer", type->getScalarSizeInBits(),
                                    dwarf::DW_ATE_signed);
  }
  case llvm::Type::VoidTyID: {
    return builder->createBasicType("void", 0, dwarf::DW_ATE_UCS);
  }

  case llvm::Type::FloatTyID: {
    return builder->createBasicType("real", type->getScalarSizeInBits(),
                                    dwarf::DW_ATE_float);
  }
  case llvm::Type::PointerTyID: {
    auto ptrTy = static_cast<llvm::PointerType *>(type);
    auto baseTy = getDIType(ptrTy->getElementType());
    return builder->createPointerType(baseTy, ptrTy->getScalarSizeInBits());
  }
  case llvm::Type::ArrayTyID: {
    auto arrTy = static_cast<llvm::ArrayType *>(type);
    auto baseTy = getDIType(arrTy->getElementType());

    auto *eleTy = type;
    llvm::SmallVector<llvm::Metadata *, 2> Subs;
    while (auto tempTy = llvm::dyn_cast<llvm::ArrayType>(eleTy)) {
      Subs.push_back(builder->getOrCreateSubrange(0, tempTy->getNumElements()));
      eleTy = tempTy->getArrayElementType();
    }

    llvm::DINodeArray subs = builder->getOrCreateArray(Subs);
    return builder->createArrayType(
        arrTy->getNumElements(),
        arrTy->getArrayElementType()->getScalarSizeInBits(), baseTy, subs);
  }
  case llvm::Type::DoubleTyID: {
    return builder->createBasicType(
        "double precision", type->getScalarSizeInBits(), dwarf::DW_ATE_float);
  }
  case llvm::Type::StructTyID: {

    llvm::SmallVector<llvm::Metadata *, 2> diTypes;
    auto funcTy = static_cast<llvm::StructType *>(type);

    for (unsigned I = 0; I < funcTy->getNumElements(); ++I) {

      auto scope = getCurrCU();
      auto name = funcTy->hasName() ? funcTy->getName() : "";
      auto file = getCurrCU()->getFile();
      auto lineNumber = -1;
      auto baseTy = funcTy->getStructElementType(I);
      auto size = baseTy->getScalarSizeInBits();
      auto align = size;
      llvm::DINode::DIFlags flags = llvm::DINode::DIFlags::FlagPublic;
      auto member =
          builder->createMemberType(scope, name, file, lineNumber, size, align,
                                    10, flags, getDIType(baseTy));
      diTypes.push_back(member);
    }

    auto scope = getCurrCU();
    auto name = funcTy->hasName() ? funcTy->getName() : "";
    auto file = getCurrCU()->getFile();
    auto lineNumber = -1;
    auto size = funcTy->getScalarSizeInBits();
    auto align = size;
    llvm::DINode::DIFlags flags = llvm::DINode::DIFlags::FlagPublic;
    auto derivedFrom = nullptr;
    auto nodeArray = builder->getOrCreateArray(diTypes);
    return builder->createStructType(scope, name, file, lineNumber, size, align,
                                     flags, derivedFrom, nodeArray);
  }
  case llvm::Type::FunctionTyID: {

    llvm::SmallVector<llvm::Metadata *, 2> diTypes;
    auto funcTy = static_cast<llvm::FunctionType *>(type);

    auto returnType = getDIType(funcTy->getReturnType());

    diTypes.push_back(returnType);

    for (unsigned I = 0; I < funcTy->getNumParams(); ++I) {
      diTypes.push_back(getDIType(funcTy->getParamType(I)));
    }

    llvm::DITypeRefArray EltTypeArray = builder->getOrCreateTypeArray(diTypes);
    return builder->createSubroutineType(EltTypeArray);
  }
  default:
    errs() << "\n Unknown type is: " << *type << "\n";
    llvm_unreachable("Unknown DI Type conversion");
  };
}

llvm::DISubprogram *CGDebugInfo::getSubProgram(llvm::Function *func,
                                               SourceLoc loc) {
  auto fnType = func->getFunctionType();
  auto diType = llvm::cast<llvm::DISubroutineType>(getDIType(fnType));
  auto subProg = builder->createFunction(
      ScopeStack.back(), func->getName(), func->getName(), currCU->getFile(),
      loc.Line, diType,
      func->getLinkage() == llvm::GlobalValue::InternalLinkage, true, 0);
  return subProg;
}

llvm::DILocalVariable *CGDebugInfo::getLocalVariable(llvm::Value *arg,
                                                     fc::Symbol *sym,
                                                     int ArgNum,
                                                     llvm::BasicBlock *BB) {
  assert(sym);
  auto ty = getDIType(sym->getType());

  DILocalVariable *argVal = nullptr;
  if (ArgNum == -1) {
    argVal = builder->createAutoVariable(getCurrScope(), sym->getName(),
                                         currCU->getFile(),
                                         sym->getSourceLoc().Line, ty, true);
  } else {
    argVal = builder->createParameterVariable(
        ScopeStack.back(), sym->getName(), ArgNum, currCU->getFile(),
        sym->getSourceLoc().Line, ty, true);
  }
  auto DL = getLoc(sym->getSourceLoc(), ScopeStack.back());
  assert(BB);
  builder->insertDeclare(arg, argVal, builder->createExpression(), DL, BB);
  return argVal;
}
