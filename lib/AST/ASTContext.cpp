#include "AST/ASTContext.h"
#include "AST/ASTContextImpl.h"
#include "AST/SymbolTable.h"

using namespace fc;

#include <bits/stdc++.h>

ASTContextImpl::ASTContextImpl(ASTContext &FC)
    : FC(FC), int16Ty(Type::Int16ID, FC, 16), int32Ty(Type::Int32ID, FC, 32),
      int64Ty(Type::Int64ID, FC, 64), int128Ty(Type::Int128ID, FC, 128),
      realTy(Type::RealID, FC, 32), doubleTy(Type::DoubleID, FC, 64),
      charTy(Type::CharacterID, FC, 8), logicalTy(Type::LogicalID, FC, 1),
      dummyArgTy(Type::DummyArgID, FC), stringCharTy(Type::StringCharID, FC),
      undeclaredTy(Type::UndeclaredID, FC), moduleTy(Type::ModuleID, FC),
      voidTy(Type::VoidID, FC), undeclaredFnTy(Type::UndeclaredFnID, FC),
      varArgTy(Type::VarArgID, FC) {}

ASTContext::ASTContext(Diagnostics &Diag, llvm::StringRef filename)
    : Diag(Diag), inputFileName(filename) {
  impl = new ASTContextImpl(*this);
}

ASTContext::~ASTContext() { delete impl; }

void *ASTContext::allocate(size_t bytes, size_t align) const {
  return allocator.Allocate(bytes, align);
}

void ASTContext::deallocate(void *ptr) const {
  // Automatic de-allocation will happen during destruction of context.
}

ASTContextImpl::~ASTContextImpl() {
  /*
  // Clear all symbol tables.
  for (auto &pair : symbolTableMap) {
    delete pair.second;
  }

  // clear all derived types.
   for (auto &pair : derivedTypes) {
    delete pair.second;
   }
  */
}

static void getKey(Type *type, std::stringstream &ss);

static void getPointerTypeKey(Type *eleTy, std::stringstream &ss) {
  ss << "pty_";
  getKey(eleTy, ss);
  ss << "_";
  return;
}

static void getNamedTypeKey(llvm::StringRef name, std::stringstream &ss) {
  ss << "namedty_" << name.str() << "_";
  return;
}

static void getComplexTypeKey(unsigned kind, std::stringstream &ss) {
  ss << "complex_" << kind;
  return;
}

static void getFunctionTypeKey(Type *returnTy,
                               const llvm::SmallVector<Type *, 2> &list,
                               std::stringstream &ss) {
  ss << "fty_";
  getKey(returnTy, ss);
  ss << "_";
  for (auto b : list) {
    getKey(b, ss);
    ss << "_";
  }
  return;
}

static void
getStructTypeKey(const llvm::StringRef name,
                 const llvm::SmallVector<Type *, 2> &list,
                 const llvm::SmallVector<std::string, 2> &fieldNameList,
                 std::stringstream &ss) {
  ss << "sty_" << name.str();
  ss << "_";
  for (unsigned I = 0; I < list.size(); ++I) {
    if (list[I]->isPointerTy())
      ss << "pty_";
    else
      getKey(list[I], ss);
    ss << "_" << fieldNameList[I] << "_";
  }
  return;
}

static void getArrayTypeKey(Type *baseTy, const ArrBoundsList &list,
                            std::stringstream &ss) {

  ss << "aty_";
  getKey(baseTy, ss);
  ss << "_";
  for (auto b : list) {
    ss << std::to_string(b.first) << "_";
    ss << std::to_string(b.second) << "_";
  }
  return;
}

// TODO: do faster method.
static void getKey(Type *type, std::stringstream &ss) {
  if (!type->isDerivedTy()) {
    ss << std::to_string((unsigned)(type->getTypeID()));
    return;
  }
  switch (type->getTypeID()) {
  case Type::ArrayID: {
    auto arrTy = static_cast<ArrayType *>(type);
    getArrayTypeKey(arrTy->getElementTy(), arrTy->getBoundsList(), ss);
    return;
  }
  case Type::FunctionID: {
    auto FuncTy = static_cast<FunctionType *>(type);
    getFunctionTypeKey(FuncTy->getReturnType(), FuncTy->getArgList(), ss);
    return;
  }
  case Type::PointerID: {
    auto ptrTy = static_cast<PointerType *>(type);
    getPointerTypeKey(ptrTy->getElementType(), ss);
    return;
  }
  case Type::StructID: {
    auto StructTy = static_cast<StructType *>(type);
    getStructTypeKey(StructTy->getName(), StructTy->getTypeList(),
                     StructTy->getNameList(), ss);
    return;
  }
  case Type::NamedID: {
    auto NamedTy = static_cast<NamedType *>(type);
    getNamedTypeKey(NamedTy->getName(), ss);
    return;
  }
  default:
    llvm_unreachable("not handled yet.");
  };
}

ArrayType *ASTContextImpl::get(Type *baseTy, int numDims) {
  std::stringstream ss;
  getKey(baseTy, ss);
  ss << "_" << std::to_string(numDims) << "_";
  auto key = ss.str();

  auto val = derivedTypes.find(key);

  if (val == derivedTypes.end()) {
    auto newVal = new (FC, alignof(ArrayType)) ArrayType(baseTy, numDims, FC);
    derivedTypes.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return static_cast<ArrayType *>(val->getValue());
}

ArrayType *ASTContextImpl::get(Type *baseTy, ArrBoundsList &list) {
  std::stringstream ss;
  getArrayTypeKey(baseTy, list, ss);
  auto key = ss.str();

  auto val = derivedTypes.find(key);
  if (val == derivedTypes.end()) {
    auto newVal = new (FC, alignof(ArrayType)) ArrayType(baseTy, list, FC);
    derivedTypes.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return static_cast<ArrayType *>(val->getValue());
}

FunctionType *ASTContextImpl::get(Type *returnTy,
                                  llvm::SmallVector<Type *, 2> &list) {
  std::stringstream ss;
  getFunctionTypeKey(returnTy, list, ss);
  auto key = ss.str();

  auto val = derivedTypes.find(key);
  if (val == derivedTypes.end()) {
    auto newVal =
        new (FC, alignof(FunctionType)) FunctionType(returnTy, list, FC);
    derivedTypes.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return static_cast<FunctionType *>(val->getValue());
}

static std::string getSymbolTableKey(llvm::StringRef name, ScopeKind kind,
                                     SymbolTable *parent) {
  return name.str() + "_" + std::to_string((unsigned int)(kind)) +
         (parent ? (parent->getName().c_str()) : "");
}

SymbolTable *ASTContextImpl::get(llvm::StringRef name, ScopeKind kind,
                                 SymbolTable *parent) {
  auto key = getSymbolTableKey(name, kind, parent);
  auto val = symbolTableMap.find(key);
  if (val == symbolTableMap.end()) {
    auto newVal =
        new (FC, alignof(SymbolTable)) SymbolTable(FC, kind, name, parent);
    symbolTableMap.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return val->getValue();
}

StructType *
ASTContextImpl::get(llvm::StringRef name, llvm::SmallVector<Type *, 2> &list,
                    llvm::SmallVector<std::string, 2> &fieldNameList) {
  std::stringstream ss;

  // FIXME: we can have self-referential pointers that will infinitely recurse
  // in key generation.
  getStructTypeKey(name, list, fieldNameList, ss);
  auto key = ss.str();

  auto val = derivedTypes.find(key);
  if (val == derivedTypes.end()) {
    auto newVal =
        new (FC, alignof(StructType)) StructType(name, list, fieldNameList, FC);
    derivedTypes.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return static_cast<StructType *>(val->getValue());
}

PointerType *ASTContextImpl::get(Type *elementTy) {
  // For the moment we are forcing new allocation due to the risk of recursing
  // infintely in the type-key generation of self-referential pointers in
  // struct-types.
  if (elementTy->isStructTy())
    return new (FC, alignof(PointerType)) PointerType(elementTy, FC);

  std::stringstream ss;
  getPointerTypeKey(elementTy, ss);
  auto key = ss.str();

  auto val = derivedTypes.find(key);
  if (val == derivedTypes.end()) {
    auto newVal = new (FC, alignof(PointerType)) PointerType(elementTy, FC);
    derivedTypes.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return static_cast<PointerType *>(val->getValue());
}

ComplexType *ASTContextImpl::get(unsigned kind) {

  std::stringstream ss;
  getComplexTypeKey(kind, ss);
  auto key = ss.str();

  auto val = derivedTypes.find(key);
  if (val == derivedTypes.end()) {
    auto newVal = new (FC, alignof(ComplexType)) ComplexType(kind, FC);
    derivedTypes.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return static_cast<ComplexType *>(val->getValue());
}

NamedType *ASTContextImpl::get(const llvm::StringRef name) {
  std::stringstream ss;
  getNamedTypeKey(name, ss);
  auto key = ss.str();

  auto val = derivedTypes.find(key);
  if (val == derivedTypes.end()) {
    auto newVal = new (FC, alignof(NamedType)) NamedType(name, FC);
    derivedTypes.insert(std::make_pair(key, newVal));
    return newVal;
  }
  return static_cast<NamedType *>(val->getValue());
}
