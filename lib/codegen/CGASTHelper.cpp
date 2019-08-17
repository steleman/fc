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
#include "codegen/CGASTHelper.h"

#include "AST/ProgramUnit.h"
#include "AST/SymbolTable.h"
#include "AST/Type.h"
#include "common/Debug.h"

using namespace fc;
using namespace ast;

CGASTHelper::CGASTHelper(ast::ParseTree *tree, llvm::Module *module,
                         llvm::IRBuilder<> *IRB, Standard std)
    : parseTree(tree), TheModule(module), C(parseTree->getContext()),
      LLC(TheModule->getContext()), IRB(IRB), std(std) {}

CGASTHelper::SubPUHelper *CGASTHelper::getSubPUHelper(ProgramUnit *PU) {
  auto val = subPUHelperMap.find(PU);
  if (val == subPUHelperMap.end())
    return nullptr;
  return &val->second;
}

llvm::Type *CGASTHelper::getLLVMTypeForDynArray(fc::ArrayType *arrTy) {
  assert(arrTy->getBoundsList().empty());

  // See if the type is already constructed.
  auto structTypeVal = this->dynArrMap.find(arrTy);
  if (structTypeVal != this->dynArrMap.end()) {
    return structTypeVal->second;
  }

  auto BaseTy = getLLVMTypeFor(arrTy->getElementTy());
  auto BasePtrTy = BaseTy->getPointerTo();
  auto LB = IRB->getInt64Ty();
  auto UB = LB;
  auto Size = LB; // UB - LB + 1
  auto isAllocated = IRB->getInt1Ty();
  auto dimsTy = llvm::StructType::create("struct.dims", LB, UB, Size);
  auto dimsArrTy = llvm::ArrayType::get(dimsTy, arrTy->getNumDims());
  auto StructTy =
      llvm::StructType::create("struct.arr", BasePtrTy, dimsArrTy, isAllocated);

  // record the type.
  this->dynArrMap[arrTy] = StructTy;

  return StructTy;
}

llvm::Type *CGASTHelper::getLLVMTypeFor(fc::Type *type) {
  switch (type->getTypeID()) {
  case fc::Type::VoidID:
    return IRB->getVoidTy();
  case fc::Type::LogicalID:
    return IRB->getInt1Ty();
  case fc::Type::CharacterID:
  case fc::Type::StringCharID:
    return IRB->getInt8Ty();
  case fc::Type::Int16ID:
    return IRB->getInt16Ty();
  case fc::Type::Int32ID:
    return IRB->getInt32Ty();
  case fc::Type::Int64ID:
    return IRB->getInt64Ty();
  case fc::Type::Int128ID:
    return IRB->getInt128Ty();
  case fc::Type::RealID:
    return IRB->getFloatTy();
  case fc::Type::DoubleID:
    return IRB->getDoubleTy();
  case fc::Type::DummyArgID:
  case fc::Type::UndeclaredID:
  case fc::Type::UndeclaredFnID:
  case fc::Type::VarArgID:
    llvm_unreachable("Found unhandled type in codegen");

  case fc::Type::PointerID: {
    auto ptrTy = static_cast<fc::PointerType *>(type);
    auto baseTy = getLLVMTypeFor(ptrTy->getElementType());
    return llvm::PointerType::get(baseTy, 0);
  }
  // Array type:
  case fc::Type::ArrayID: {
    auto arrTy = static_cast<ArrayType *>(type);

    auto LLBaseTy = getLLVMTypeFor(arrTy->getElementTy());

    // Get type for dynamic arrays.
    if (arrTy->getBoundsList().empty()) {
      // In f77, bounds are
      if (std == Standard::f77) {
        // Create zero sized array
        auto ty = llvm::ArrayType::get(LLBaseTy, 0);
        assert(ty);
        return LLBaseTy;
      }
      return getLLVMTypeForDynArray(arrTy);
    }

    // Get the base element type.
    auto dims = arrTy->getDimSize();
    bool returnMultiDimArray = false;
    if (returnMultiDimArray) {

      auto LLArrTy = LLBaseTy;
      // Construct the single dim array.
      for (auto dim : dims) {
        LLArrTy = llvm::ArrayType::get(LLBaseTy, dim);
        LLBaseTy = LLArrTy;
      }

      return LLArrTy;
    } else {
      // Emit single dimension array.
      auto totalSize = 1;
      for (auto dim : dims) {
        totalSize *= dim;
      }
      return llvm::ArrayType::get(LLBaseTy, totalSize);
    }
  }
  case fc::Type::FunctionID: {
    llvm::SmallVector<llvm::Type *, 2> Tys;
    auto fcFuncTy = static_cast<FunctionType *>(type);
    bool varArgSeen = false;
    for (auto fcType : fcFuncTy->getArgList()) {
      if (fcType->isVarArgTy()) {
        // Should be the last argument.
        varArgSeen = true;
        break;
      }
      auto LLTy = getLLVMTypeFor(fcType);
      assert(LLTy);
      Tys.push_back(LLTy->getPointerTo());
    }
    auto retTy = getLLVMTypeFor(fcFuncTy->getReturnType());
    return llvm::FunctionType::get(retTy, Tys, varArgSeen);
  }

  case fc::Type::StructID: {
    auto structType = static_cast<StructType *>(type);

    // If we already created it
    if (structTypeMap.find(structType) != structTypeMap.end()) {
      return structTypeMap[structType];
    }

    // We create an empty(opaque) structure always since there can be
    // self-referential pointers that needs to point to this (incomplete then)
    // type.
    llvm::StructType *llStructType =
        llvm::StructType::create(LLC, structType->getName());
    structTypeMap[structType] = llStructType;

    llvm::SmallVector<llvm::Type *, 2> llTypes;
    for (auto fcType : structType->getTypeList()) {
      llTypes.push_back(getLLVMTypeFor(fcType));
    }

    llStructType->setBody(llTypes);
    return llStructType;
  }

    /*   // For complex type, emit the structure access.
      case fc::Type::ComplexID: {
        auto ty = static_cast<ComplexType *>(type);
        llvm::Type *llTy = nullptr;
        if (ty->getKind() == 4)
          llTy = llvm::Type::getFloatTy(LLC);
        else if (ty->getKind() == 8) {
          llTy = llvm::Type::getDoubleTy(LLC);
        } else {
          llvm::errs() << "\n" << ty->getKind();
          llvm_unreachable("Unhandled complex type kind");
        }
        return llvm::StructType::create(LLC, {llTy, llTy},
                                        "complex_" +
      std::to_string(ty->getKind()));
      }
      */
  default:
    llvm_unreachable("type not handled yet.");
  };
}

llvm::Type *CGASTHelper::getLLVMTypeFor(Symbol *symbol) {
  auto parenSym = symbol->getParentSymbol();
  if (parenSym)
    symbol = parenSym;

  fc::Type *symType = symbol->getType(); // This will be OrigType

  if (auto symPtrType = llvm::dyn_cast<fc::PointerType>(symType)) {
    if (symPtrType->getElementType()->isArrayTy())
      assert(symPtrType->getElementType()->isDynArrayTy());

    return getLLVMTypeFor(symPtrType);
  }

  return getLLVMTypeFor(symType);
}

llvm::Type *CGASTHelper::getLLVMTypeFor(ProgramUnit *PU, bool &hasFrameArg) {
  hasFrameArg = false;
  auto parent = PU->getParent();
  if (parent->getKind() == ProgramUnitKind::ProgramKind ||
      parent->getKind() == ProgramUnitKind::ModuleKind) {
    return getLLVMTypeFor(PU->getType());
  }

  assert(PU->isSubroutine() || PU->isFunction());

  auto helperVal = subPUHelperMap.find(PU->getParent());
  llvm::StructType *StructTy = nullptr;
  if (helperVal == subPUHelperMap.end()) {

    // This is a nested subroutine. Get the used parent symbol list.
    SymbolSet set;
    // TODO: optimize.
    PU->getParent()->getUsedSymbolsInChildren(set);

    if (set.empty()) {
      return getLLVMTypeFor(PU->getType());
    }

    hasFrameArg = true;
    StructTy = getLLVMStructTypeFor(PU->getName(), set);

    SubPUHelper helper;
    helper.frameTy = StructTy;
    helper.hasFrameArg = true;
    helper.set = set;

    subPUHelperMap[PU->getParent()] = helper;
  } else {
    hasFrameArg = true;
    StructTy = helperVal->second.frameTy;
  }

  auto fcFuncType = static_cast<fc::FunctionType *>(PU->getType());
  auto argsTypeList = fcFuncType->getArgList();
  auto llFuncTy = static_cast<llvm::FunctionType *>(getLLVMTypeFor(fcFuncType));

  llvm::SmallVector<llvm::Type *, 2> typeList;
  typeList.push_back(StructTy->getPointerTo());

  for (auto param : llFuncTy->params()) {
    typeList.push_back(param);
  }

  auto returnTy = getLLVMTypeFor(fcFuncType->getReturnType());
  return llvm::FunctionType::get(returnTy, typeList, false);
}

llvm::StructType *CGASTHelper::getLLVMStructTypeFor(llvm::StringRef name,
                                                    SymbolSet &set) {
  llvm::SmallVector<llvm::Type *, 4> typeList;

  for (auto sym : set) {

    auto LLTy = getLLVMTypeFor(sym);
    if (sym->hasIntent()) {
      LLTy = LLTy->getPointerTo();
    }
    typeList.push_back(LLTy);
  }
  auto StructTy =
      llvm::StructType::create(LLC, typeList, "FUNC." + name.str(), false);

  return StructTy;
}

std::string CGASTHelper::getNameForProgramUnit(ProgramUnit *PU) {
  assert(PU->isSubroutine() || PU->isFunction() || PU->isMainProgram());
  // auto Sub = static_cast<Subroutine *>(PU);

  auto parent = PU->getParent();

  if (parent->isProgram() || PU->isMainProgram())
    return PU->getName();

  switch (parent->getKind()) {
  case ProgramUnitKind::MainProgramKind:
    return parent->getName().str() + "." + PU->getName().str();
  case ProgramUnitKind::FunctionKind:
  case ProgramUnitKind::SubroutineKind:
    return getNameForProgramUnit(parent) + "." + PU->getName().str();
  case ProgramUnitKind::ModuleKind:
    return parent->getName().str() + "_" + PU->getName().str();

  default:
    llvm_unreachable("Unknown program kind");
  }
  return "";
}

llvm::GlobalValue::LinkageTypes
CGASTHelper::getLinkageTypeFor(ProgramUnit *PU) {
  auto Linkage = llvm::GlobalValue::InternalLinkage;
  if (PU->inGlobalScope() || PU->getParent()->isModule())
    Linkage = llvm::GlobalValue::ExternalLinkage;
  return Linkage;
}

llvm::Function *CGASTHelper::emitDeclarationFor(ProgramUnit *PU) {

  assert(llvm::isa<fc::Function>(PU));
  ArgsList argsList;

  auto name = getNameForProgramUnit(PU);

  auto fn = static_cast<Function *>(PU);
  argsList = fn->getArgsList();

  bool hasFrameArg = false;
  auto FnTy = getLLVMTypeFor(PU, hasFrameArg);

  PUNameMap[PU->getName().data()] = name;
  auto Linkage = getLinkageTypeFor(PU);
  auto Fn = llvm::Function::Create(static_cast<llvm::FunctionType *>(FnTy),
                                   Linkage, name, TheModule);
  unsigned I = 0;
  for (auto &LLArg : Fn->args()) {
    // Adding no alias attributes for all the arguments.
    LLArg.addAttr(llvm::Attribute::NoAlias);

    if (I == 0 && hasFrameArg) {
      LLArg.setName("frameArg." + PU->getName());
      hasFrameArg = false;
      continue;
    }
    auto currSym = PU->getSymbolTable()->getSymbol(argsList[I]);
    assert(currSym);
    if (currSym->getAttr().intentKind == In) {
      LLArg.addAttr(llvm::Attribute::ReadOnly);
    } else if (currSym->getAttr().intentKind == Out) {
      LLArg.addAttr(llvm::Attribute::WriteOnly);
    }
    LLArg.setName(argsList[I].str());
    I++;
  }
  return Fn;
}

std::string CGASTHelper::getEmittedNameForPU(std::string name) {
  assert(PUNameMap.find(name) != PUNameMap.end());
  return PUNameMap[name];
}

std::string CGASTHelper::getFunctionNameForSymbol(Symbol *sym) {
  if (sym->getParentSymbol())
    sym = sym->getParentSymbol();

  if (sym->getType() == FunctionType::getUndeclaredFuncTy(C)) {
    return sym->getName();
  }

  auto symTable = sym->getSymTable();

  auto name = sym->getName().str();
  switch (symTable->getScopeKind()) {
  case GlobalScope:
    return sym->getName();
  case ModuleScope:
    return symTable->getName() + "_" + name;
  case FunctionScope:
  case SubroutineScope: {
    name = symTable->getName() + "." + name;
    auto parentTable = symTable->getParent();
    if (parentTable) {
      if (parentTable->isModuleScope()) {
        name = parentTable->getName() + "_" + name;
      } else {
        assert(parentTable->isGlobalScope());
      }
    }
    return name;
  }
  case MainProgramScope:
    return symTable->getName() + "." + name;
  default:
    llvm_unreachable("Unkown symbol kind");
  };
  return "";
}

std::string CGASTHelper::getGlobalSymbolName(Symbol *symbol) {

  if (symbol->getSymTable()->isModuleScope()) {
    return "_FCMOD_" + symbol->getOriginalModName() + "_" +
           symbol->getName().str();
  }

  auto name = symbol->getSymTable()->getName();
  std::string varName = name;
  if (!symbol->getSymTable()->isMainProgramScope())
    varName = getEmittedNameForPU(name);

  return varName + "_" + symbol->getName().str();
}

ProgramUnit *CGASTHelper::getCalledProgramUnit(Symbol *symbol) {

  symbol = symbol->getOrigSymbol();
  assert(symbol->getType()->isFunctionTy());

  auto symTable = symbol->getSymTable();
  if (symTable->isLoadedFromModFile())
    return nullptr;

  auto PU = symTable->getProgramUnit();
  assert(PU);

  // now search for the symbol in program unit and return.
  for (auto subPU : PU->getProgramUnitList()) {
    if (subPU->getName() == symbol->getName())
      return subPU;
  }
  return nullptr;
}

llvm::Value *CGASTHelper::getReturnValueFor(std::string func) {
  assert(FuncRetMap.find(func) != FuncRetMap.end());
  return FuncRetMap[func];
}

llvm::Function *CGASTHelper::getMallocFunction() {
  auto I8PtrTy = IRB->getInt8PtrTy();
  auto I64 = IRB->getInt64Ty();
  auto FTy = llvm::FunctionType::get(I8PtrTy, {I64}, false);
  auto func = TheModule->getOrInsertFunction("malloc", FTy);
  return static_cast<llvm::Function *>(func);
}

llvm::Function *CGASTHelper::getFreeFunction() {
  auto I8PtrTy = IRB->getInt8PtrTy();
  auto FTy = llvm::FunctionType::get(IRB->getVoidTy(), {I8PtrTy}, false);
  auto func = TheModule->getOrInsertFunction("free", FTy);
  return static_cast<llvm::Function *>(func);
}

unsigned CGASTHelper::getSizeForType(Type *Ty) {
  switch (Ty->getTypeID()) {
  case fc::Type::Int32ID:
    return 4;
  case fc::Type::Int64ID:
    return 8;
  case fc::Type::RealID:
    return 4;
  case fc::Type::DoubleID:
    return 8;
  case fc::Type::CharacterID:
  case fc::Type::StringCharID:
    return 1;
  default:
    llvm_unreachable("Unhandled type");
  }
  return 0;
}

llvm::Value *CGASTHelper::getSExt(llvm::Value *V) {
  assert(V->getType()->isIntegerTy());
  auto I64 = IRB->getInt64Ty();
  if (V->getType() != I64)
    return IRB->CreateSExt(V, I64);
  return V;
}
