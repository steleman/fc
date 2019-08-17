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
#ifndef FC_CONTEXT_IMPL_H
#define FC_CONTEXT_IMPL_H

#include "AST/Type.h"
#include "SymbolTableCommon.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/StringRef.h"

namespace fc {

class ASTContext;
class ASTContextImpl {
  ASTContext &FC;
  llvm::StringMap<Type *> derivedTypes;
  llvm::StringMap<SymbolTable *> symbolTableMap;

protected:
  friend class Type;
  Type int16Ty, int32Ty, int64Ty, int128Ty, realTy, doubleTy, charTy, logicalTy,
      dummyArgTy, stringCharTy, undeclaredTy, moduleTy, voidTy, undeclaredFnTy,
      varArgTy;

  friend class ArrayType;
  ArrayType *get(Type *baseTy, ArrBoundsList &list);
  ArrayType *get(Type *baseTy, int numDims);

  friend class FunctionType;
  FunctionType *get(Type *returnTy, llvm::SmallVector<Type *, 2> &list);

  friend class SymbolTable;
  SymbolTable *get(llvm::StringRef name, ScopeKind kind, SymbolTable *parent);

  friend class ASTContext;
  explicit ASTContextImpl(ASTContext &FC);

  friend class StructType;
  StructType *get(llvm::StringRef name, llvm::SmallVector<Type *, 2> &list,
                  llvm::SmallVector<std::string, 2> &fieldNameList);

  friend class PointerType;
  PointerType *get(Type *elementType);

  friend class ComplexType;
  ComplexType *get(unsigned kind);

  friend class NamedType;
  NamedType *get(llvm::StringRef name);

  ~ASTContextImpl();
};
} // namespace fc
#endif
