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
#ifndef FC_TYPE_H
#define FC_TYPE_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

#include <map>

namespace fc {

class ASTContextImpl;
class ASTContext;

class Type {

  ASTContext &FC;

public:
  enum TypeID {

    // Intrinsic types
    // TODO Currently all integer all defined here
    //      Do we need a separate class?
    VoidID = 1,
    Int16ID,  // integer(kind = 2)
    Int32ID,  // integer(kind = 4)
    Int64ID,  // integer(kind = 8)
    Int128ID, // integer(kind = 16)
    RealID,
    DoubleID,
    ComplexID,
    CharacterID,
    StringCharID,
    LogicalID,
    // ID used for parsing dummy arguments.
    // actual types will assigned once we parse
    // specification arguments.
    DummyArgID,

    // Type is not declared or known yet.
    UndeclaredID,

    // Non-intrinsic and undeclared type where only it's name is known.
    NamedID,

    // Similar to Undeclared, but we know that
    // its a function call.
    UndeclaredFnID,
    VarArgID,
    // Derived Types,
    // TODO is separate data type required for strings?
    ArrayID,
    StructID,
    PointerID,

    FunctionID,
    ModuleID,
  };

  // Return the type id of this type
  TypeID getTypeID() { return ID; }

  TypeID getTypeID() const { return ID; }

  // Return true if this is 'Integer(kind = 2)'
  constexpr bool isVoidTy() const { return ID == VoidID; }

  // Return true if this is 'Integer(kind = 2)'
  constexpr bool isInt16Ty() const { return ID == Int16ID; }

  // Return true if this is 'Integer(kind = 4)'
  constexpr bool isInt32Ty() const { return ID == Int32ID; }

  // Return true if this  is 'Integer(kind = 8)'
  constexpr bool isInt64Ty() const { return ID == Int64ID; }

  // Return true if this is 'Integer(kind = 16)'
  constexpr bool isInt128Ty() const { return ID == Int128ID; }

  // Return true if this is 'Real*4'
  constexpr bool isRealTy() const { return ID == RealID; }

  // Return true if this is 'Real*8'
  constexpr bool isDoubleTy() const { return ID == DoubleID; }

  // Return true if this is  'Complex'
  constexpr bool isComplexTy() const { return ID == ComplexID; }

  // Return true if this is 'Character'
  constexpr bool isCharacterTy() const { return ID == CharacterID; }

  // Return true if this is 'Logical'
  constexpr bool isLogicalTy() const { return ID == LogicalID; }

  // Return true if this is Array of any type
  constexpr bool isArrayTy() const { return ID == ArrayID; }

  constexpr bool isStructTy() const { return ID == StructID; }

  constexpr bool isPointerTy() const { return ID == PointerID; }

  constexpr bool isNamedTy() const { return ID == NamedID; }

  // Return true if this is Array with dynamic bounds.
  bool isDynArrayTy() const;

  // Return true if this is String type
  constexpr bool isStringCharTy() const { return ID == StringCharID; }

  // Return true if this is Dummy type
  constexpr bool isDummyArgTy() const { return ID == DummyArgID; }

  bool isStringArrTy();

  constexpr bool isDerivedTy() const {
    switch (ID) {
    case ArrayID:
    case FunctionID:
    case ModuleID:
    case PointerID:
    case StructID:
    case ComplexID:
    case NamedID:
      return true;
    default:
      return false;
    }
  }

  constexpr bool isFunctionTy() const { return (ID == FunctionID); }

  constexpr bool isModuleTy() const { return (ID == ModuleID); }

  constexpr bool isUndeclaredTy() const { return (ID == UndeclaredID); }

  constexpr bool isUndeclaredFnTy() const { return (ID == UndeclaredFnID); }

  constexpr bool isVarArgTy() const { return (ID == VarArgID); }

  constexpr bool isUndeclared() const {
    return isUndeclaredTy() || isUndeclaredFnTy();
  }

  bool isIntegralTy() const;

  bool isFloatingTy() const;

  // Either integral ty or floating ty.
  bool isNumericalTy() const { return isIntegralTy() || isFloatingTy(); }

  constexpr unsigned getSizeInBits() const {
    assert(size != 0);
    return size;
  }

  // Static functions to create each type
  static Type *getVoidTy(ASTContext &FC);

  static Type *getInt16Ty(ASTContext &FC);

  static Type *getInt32Ty(ASTContext &FC);

  static Type *getInt64Ty(ASTContext &FC);

  static Type *getInt128Ty(ASTContext &FC);

  static Type *getRealTy(ASTContext &FC);

  static Type *getDoubleTy(ASTContext &FC);

  static Type *getCharacterTy(ASTContext &FC);

  static Type *getLogicalTy(ASTContext &FC);

  static Type *getStringCharTy(ASTContext &FC);

  static Type *getUndeclaredTy(ASTContext &FC);

  static Type *getDummyArgTy(ASTContext &FC);

  static Type *getModueTy(ASTContext &FC);

  static Type *getUndeclaredFnTy(ASTContext &FC);

  static Type *getVarArgTy(ASTContext &FC);

  static Type *getCoreElementType(Type *type);

  virtual std::string dump(llvm::raw_ostream &OS, int level = 0,
                           bool isEntity = false) const;

  inline ASTContext &getContext() const { return FC; }

protected:
  friend class ASTContextImpl;
  Type(TypeID Id, ASTContext &FC, unsigned size = 0)
      : FC(FC), ID(Id), size(size) {}
  virtual ~Type() {}

private:
  TypeID ID;
  unsigned size;
};

typedef std::pair<long int, long int> ArrayBounds;
typedef llvm::SmallVector<ArrayBounds, 2> ArrBoundsList;

class ArrayType : public Type {

private:
  Type *elementTy;
  int numDims;
  ArrBoundsList boundsList;
  bool isDynamic;

public:
  static ArrayType *get(ASTContext &FC, Type *elementTy, ArrBoundsList &list);
  static ArrayType *get(ASTContext &FC, Type *elementTy, int numDims);

  ArrBoundsList getBoundsList() const { return boundsList; }

  ArrBoundsList &getBoundsList() { return boundsList; }

  llvm::SmallVector<int, 2> getDimSize() const;

  static bool classof(const Type *type) { return type->getTypeID() == ArrayID; }

  Type *getElementTy() const { return elementTy; }

  void setElementTy(Type *newTy) { elementTy = newTy; }

  std::string dump(llvm::raw_ostream &OS, int level = 0,
                   bool isEntity = false) const override;

  int getNumDims() const { return numDims; }

  bool boundsEmpty() const { return boundsList.empty(); }

protected:
  friend class ASTContextImpl;
  // For static arrays.
  ArrayType(Type *elementTy, ArrBoundsList &list, ASTContext &FC)
      : Type(ArrayID, FC), elementTy(elementTy), numDims(list.size()),
        boundsList(list), isDynamic(false) {}

  // For dynamic arrays.
  ArrayType(Type *elementTy, int numDims, ASTContext &FC)
      : Type(ArrayID, FC), elementTy(elementTy), numDims(numDims),
        isDynamic(true) {}

  ~ArrayType() {}
};

class StructType : public Type {
private:
  std::string name;

  llvm::SmallVector<Type *, 2> typeList;
  llvm::SmallVector<std::string, 2> fieldNameList;

  std::map<std::string, unsigned> fieldNameToIdxMap;

  // The following fields are non-general and are set depending on what this
  // StructType represents.

  // Check if this struct type represents the dynamic array.
  bool isDynArray;

  // Tracks the name of the DTD this StructType represents. In this
  // case this.name will be the fully-qualified DTD name.
  std::string dtdName;

protected:
  friend class ASTContextImpl;
  StructType(llvm::StringRef name, llvm::SmallVector<Type *, 2> &list,
             llvm::SmallVector<std::string, 2> &fieldNameList, ASTContext &FC)
      : Type(Type::StructID, FC), name(name.str()), typeList(list),
        fieldNameList(fieldNameList) {
    assert(fieldNameList.size() == list.size());
    unsigned i = 0;
    for (std::string fieldName : fieldNameList)
      fieldNameToIdxMap[fieldName] = i++;
  }

  ~StructType() {}

public:
  static StructType *get(ASTContext &FC, llvm::StringRef name,
                         llvm::SmallVector<Type *, 2> &list,
                         llvm::SmallVector<std::string, 2> &fieldNameList,
                         bool isDynArray);

  static bool classof(const Type *type);

  inline llvm::SmallVector<Type *, 2> getTypeList() const { return typeList; }

  inline std::string getName() const { return name; }

  inline llvm::SmallVector<std::string, 2> getNameList() const {
    return fieldNameList;
  }

  constexpr bool isDynArrayRep() const { return isDynArray; }

  inline llvm::StringRef getFieldName(unsigned I) const {
    return fieldNameList[I];
  }

  unsigned getIndexOf(std::string fieldName) {
    return fieldNameToIdxMap[fieldName];
  }

  Type *getContainedType(unsigned I) const;

  Type *getContainedType(std::string fieldName) {
    return getContainedType(getIndexOf(fieldName));
  }

  void setContainedType(Type *type, unsigned I);

  llvm::StringRef getDTDName() const { return llvm::StringRef(dtdName); }

  void setDTDName(llvm::StringRef name) { dtdName = name.str(); }

  std::string dump(llvm::raw_ostream &OS, int level = 0,
                   bool isEntity = false) const override;
};

class PointerType : public Type {
private:
  Type *eleTy;

protected:
  friend class ASTContextImpl;
  PointerType(Type *returnTy, ASTContext &FC)
      : Type(Type::PointerID, FC), eleTy(returnTy) {}

  ~PointerType() {}

public:
  static PointerType *get(ASTContext &FC, Type *eleTy);

  static bool classof(const Type *type);

  Type *getElementType() const { return eleTy; }

  void setElementTy(Type *newTy) { eleTy = newTy; }

  std::string dump(llvm::raw_ostream &OS, int level = 0,
                   bool isEntity = false) const override;
};

class ComplexType : public Type {
private:
  // size of real part in bytes.
  unsigned kindSize;

protected:
  friend class ASTContextImpl;
  ComplexType(unsigned kind, ASTContext &FC)
      : Type(Type::ComplexID, FC), kindSize(kind) {}

  ~ComplexType() {}

public:
  static ComplexType *get(ASTContext &FC, unsigned kind);

  static bool classof(const Type *type);

  std::string dump(llvm::raw_ostream &OS, int level = 0,
                   bool isEntity = false) const override;

  constexpr unsigned getKind() const { return kindSize; }
};

class NamedType : public Type {
  const std::string name;

protected:
  friend class ASTContextImpl;
  explicit NamedType(llvm::StringRef name, ASTContext &FC)
      : Type(Type::NamedID, FC), name(name.lower()) {}

  ~NamedType() {}

public:
  static NamedType *get(ASTContext &FC, llvm::StringRef name);

  static bool classof(const Type *type) {
    auto typeID = type->getTypeID();
    return typeID == NamedID;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0,
                   bool isEntity = false) const override;

  llvm::StringRef getName() const { return llvm::StringRef(name); }
};

class FunctionType : public Type {
private:
  Type *returnTy;
  llvm::SmallVector<Type *, 2> typeList;

protected:
  friend class ASTContextImpl;
  FunctionType(Type *returnTy, llvm::SmallVector<Type *, 2> &list,
               ASTContext &FC)
      : Type(Type::FunctionID, FC), returnTy(returnTy), typeList(list) {}

  ~FunctionType() {}

public:
  static FunctionType *get(ASTContext &FC, Type *returnTy,
                           llvm::ArrayRef<Type *> list);
  static FunctionType *get(ASTContext &FC, Type *returnTy,
                           llvm::SmallVector<Type *, 2> &list);

  static FunctionType *getUndeclaredFuncTy(ASTContext &FC);

  static FunctionType *getVoidUndeclaredFuncTy(ASTContext &FC);

  static bool classof(const Type *type);

  Type *getReturnType() const { return returnTy; }

  llvm::SmallVector<Type *, 2> getArgList() const { return typeList; }

  Type *getArgType(unsigned I);

  void setArgType(Type *type, unsigned I) {
    assert(I < typeList.size());
    typeList[I] = type;
  }

  std::string dump(llvm::raw_ostream &OS, int level = 0,
                   bool isEntity = false) const override;
};

class Constant {
private:
  llvm::SmallVector<std::string, 2> valueList;
  Type *type;

public:
  llvm::StringRef getValue() const { return valueList[0]; }

  Type *getType() const { return type; }

  // FIXME: leaking memory.
  static Constant *Create(std::string value, Type *type) {
    return new Constant(value, type);
  }

  // FIXME: leaking memory.
  static Constant *Create(llvm::SmallVector<std::string, 2> &constValList,
                          ArrayType *type) {
    return new Constant(constValList, type);
  }

  long int getInt() {
    assert(getType()->isIntegralTy());
    return std::stol(getValue());
  }

  double getFloat() {
    assert(getType()->isFloatingTy());
    return std::stod(getValue());
  }

  bool getBool() {
    assert(getType()->isLogicalTy());
    std::string value = getValue();
    std::transform(value.begin(), value.end(), value.begin(), ::tolower);
    if (value == ".true.")
      return true;
    else
      return false;
  }

  llvm::SmallVector<std::string, 2> &getArrValue() { return valueList; }

protected:
  explicit Constant(llvm::StringRef value, Type *type) : type(type) {
    valueList.push_back(value);
  }

  explicit Constant(llvm::SmallVector<std::string, 2> &constValList,
                    ArrayType *type)
      : valueList(constValList), type(type) {}
};

} // namespace fc
#endif
