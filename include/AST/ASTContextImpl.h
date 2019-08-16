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
