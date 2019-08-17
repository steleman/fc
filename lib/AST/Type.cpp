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
#include "AST/Type.h"
#include "AST/ASTContext.h"
#include "AST/ASTContextImpl.h"
#include "llvm/Support/Casting.h"

using namespace fc;

bool Type::isStringArrTy() {
  if (isArrayTy()) {
    auto arrTy = static_cast<ArrayType *>(this);
    if (arrTy->getElementTy()->isStringCharTy())
      return true;
  }
  return false;
}

// Static functions to create each type
Type *Type::getVoidTy(ASTContext &FC) { return &FC.impl->voidTy; }

Type *Type::getInt16Ty(ASTContext &FC) { return &FC.impl->int16Ty; }

Type *Type::getInt32Ty(ASTContext &FC) { return &FC.impl->int32Ty; }

Type *Type::getInt64Ty(ASTContext &FC) { return &FC.impl->int64Ty; }

Type *Type::getInt128Ty(ASTContext &FC) { return &FC.impl->int128Ty; }

Type *Type::getRealTy(ASTContext &FC) { return &FC.impl->realTy; }

Type *Type::getDoubleTy(ASTContext &FC) { return &FC.impl->doubleTy; }

Type *Type::getCharacterTy(ASTContext &FC) { return &FC.impl->charTy; }

Type *Type::getLogicalTy(ASTContext &FC) { return &FC.impl->logicalTy; }

Type *Type::getStringCharTy(ASTContext &FC) { return &FC.impl->stringCharTy; }

Type *Type::getUndeclaredTy(ASTContext &FC) { return &FC.impl->undeclaredTy; }

Type *Type::getDummyArgTy(ASTContext &FC) { return &FC.impl->dummyArgTy; }

Type *Type::getModueTy(ASTContext &FC) { return &FC.impl->moduleTy; }

Type *Type::getVarArgTy(ASTContext &FC) { return &FC.impl->varArgTy; }

Type *Type::getUndeclaredFnTy(ASTContext &FC) {
  return &FC.impl->undeclaredFnTy;
}

Type *Type::getCoreElementType(Type *type) {
  if (auto ptrType = llvm::dyn_cast<PointerType>(type)) {
    return getCoreElementType(ptrType->getElementType());
  } else if (auto arrType = llvm::dyn_cast<ArrayType>(type)) {
    return getCoreElementType(arrType->getElementTy());
  } else {
    return type;
  }
}

// Return true if this is Array with dynamic bounds.
bool Type::isDynArrayTy() const {
  if (!isArrayTy())
    return false;

  auto arrayTy = static_cast<const ArrayType *>(this);
  return arrayTy->boundsEmpty();
}

llvm::SmallVector<int, 2> ArrayType::getDimSize() const {
  // First, check if all the bounds are constants.
  // If so, calculate the dimensions.
  llvm::SmallVector<int, 2> dimList;

  for (auto &bounds : boundsList) {
    long int lower = bounds.first;
    long int upper = bounds.second;
    assert(lower <= upper);
    dimList.push_back(upper - lower + 1);
  }
  return dimList;
}

ArrayType *ArrayType::get(ASTContext &FC, Type *elementTy,
                          ArrBoundsList &list) {
  return FC.impl->get(elementTy, list);
}

ArrayType *ArrayType ::get(ASTContext &FC, Type *elementTy, int numDims) {
  return FC.impl->get(elementTy, numDims);
}

FunctionType *FunctionType::get(ASTContext &FC, Type *returnTy,
                                llvm::ArrayRef<Type *> list) {

  llvm::SmallVector<Type *, 2> typeList;
  typeList.append(list.begin(), list.end());
  return FC.impl->get(returnTy, typeList);
}

FunctionType *FunctionType::get(ASTContext &FC, Type *returnTy,
                                llvm::SmallVector<Type *, 2> &list) {
  return FC.impl->get(returnTy, list);
}

StructType *StructType::get(ASTContext &FC, llvm::StringRef name,
                            llvm::SmallVector<Type *, 2> &list,
                            llvm::SmallVector<std::string, 2> &fieldNameList,
                            bool isDynArray) {
  assert(list.size() == fieldNameList.size() && !name.empty());
  StructType *structTy = FC.impl->get(name, list, fieldNameList);
  structTy->isDynArray = true;
  return structTy;
}

PointerType *PointerType::get(ASTContext &FC, Type *elementTy) {
  return FC.impl->get(elementTy);
}

ComplexType *ComplexType::get(ASTContext &FC, unsigned kind) {
  return FC.impl->get(kind);
}

bool FunctionType::classof(const Type *type) {
  auto typeID = type->getTypeID();
  return typeID == FunctionID;
}

NamedType *NamedType::get(ASTContext &FC, llvm::StringRef name) {
  return FC.impl->get(name);
}

FunctionType *FunctionType::getUndeclaredFuncTy(ASTContext &FC) {
  return FunctionType::get(FC, Type::getRealTy(FC), {Type::getVarArgTy(FC)});
}

FunctionType *FunctionType::getVoidUndeclaredFuncTy(ASTContext &FC) {
  return FunctionType::get(FC, Type::getVoidTy(FC), {Type::getVarArgTy(FC)});
}

bool StructType::classof(const Type *type) {
  auto typeID = type->getTypeID();
  return typeID == StructID;
}

bool PointerType::classof(const Type *type) {
  auto typeID = type->getTypeID();
  return typeID == PointerID;
}

Type *FunctionType::getArgType(unsigned I) {
  assert(I < typeList.size());
  return *(typeList.begin() + I);
}

Type *StructType::getContainedType(unsigned I) const {
  assert(I < typeList.size());
  return *(typeList.begin() + I);
}

void StructType::setContainedType(Type *type, unsigned I) {
  assert(I < typeList.size());
  typeList[I] = type;
}

bool Type::isIntegralTy() const {
  switch (ID) {
  case Int16ID:
  case Int32ID:
  case Int64ID:
  case Int128ID:
    return true;
  default:
    return false;
  };
}

bool Type::isFloatingTy() const {
  switch (ID) {
  case RealID:
  case DoubleID:
    return true;
  default:
    return false;
  };
}
