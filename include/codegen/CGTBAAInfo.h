#ifndef FC_CG_TBAA_INFO_H
#define FC_CG_TBAA_INFO_H

#include "AST/ParserTreeCommon.h"
#include "codegen/CGASTHelper.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/MDBuilder.h"
#include "llvm/IR/Module.h"

namespace fc {

static int tbaaCount = 0;

class CGTBAAInfo {

private:
  llvm::Module *TheModule;
  llvm::MDBuilder builder;

  llvm::MDNode *Root;
  llvm::MDNode *tbaaPtr;

  // FLANG Genrated heirarchy is
  //  _Root
  //   |
  //   |_unlimited pointer
  //     |
  //     |_ int, float
  //     |
  //     |_ long, double
  //     |
  //     |_ any pointer
  //
  // TODO : Currently don't know when to create new nodes.
  //        So using same nodes for entire module.
  // Type specific nodes
  llvm::MDNode *intTy;
  llvm::MDNode *longTy;
  llvm::MDNode *floatTy;
  llvm::MDNode *doubleTy;
  llvm::MDNode *anyPtrTy;

  llvm::MDNode *intMDN;
  llvm::MDNode *longMDN;
  llvm::MDNode *floatMDN;
  llvm::MDNode *doubleMDN;
  llvm::MDNode *anyPtrMDN;

  std::map<llvm::Type *, llvm::MDNode *> MDCache;
  std::map<llvm::Value *, std::map<uint64_t, llvm::MDNode *>> StructMDCache;

  std::map<llvm::Value *, llvm::MDNode *> MDPtrCache;

public:
  CGTBAAInfo(llvm::Module *M)
      : TheModule(M), builder(M->getContext()), Root(nullptr),
        tbaaPtr(nullptr) {}

  void initTBAA(llvm::StringRef name) {
    std::string suffix = name.str() + std::to_string(tbaaCount);
    tbaaCount++;
    Root = builder.createTBAARoot("FC TBAA " + suffix);
    tbaaPtr = builder.createTBAAScalarTypeNode("unlimited ptr", Root, 0);
    intTy = builder.createTBAAScalarTypeNode("intTy", tbaaPtr, 0);
    longTy = builder.createTBAAScalarTypeNode("longTy", tbaaPtr, 0);
    floatTy = builder.createTBAAScalarTypeNode("floatTy", tbaaPtr, 0);
    doubleTy = builder.createTBAAScalarTypeNode("doubleTy", tbaaPtr, 0);
    anyPtrTy = builder.createTBAAScalarTypeNode("anyPtrTy", tbaaPtr, 0);
    intMDN = nullptr;
    longMDN = nullptr;
    floatMDN = nullptr;
    doubleMDN = nullptr;
    anyPtrMDN = nullptr;
  }

  void resetTBAA() {
    MDCache.clear();
    StructMDCache.clear();
    MDPtrCache.clear();
    Root = nullptr;
    tbaaPtr = nullptr;
    intTy = nullptr;
    longTy = nullptr;
    floatTy = nullptr;
    doubleTy = nullptr;
    anyPtrTy = nullptr;
  }

  llvm::MDNode *getRoot() {
    assert(Root);
    return Root;
  }

  llvm::MDNode *getTBAAPtr() {
    assert(tbaaPtr);
    return tbaaPtr;
  }

  llvm::MDNode *getIntTy() { return intTy; }

  llvm::MDNode *getLongTy() { return longTy; }

  llvm::MDNode *getFloatTy() { return floatTy; }

  llvm::MDNode *getDoubleTy() { return doubleTy; }

  llvm::MDNode *getAnyPtrTy() { return anyPtrTy; }

  llvm::MDNode *getIntMDN() {
    if (!intMDN)
      intMDN = builder.createTBAAStructTagNode(intTy, intTy, 0);
    return intMDN;
  }

  llvm::MDNode *getLongMDN() {
    if (!longMDN)
      longMDN = builder.createTBAAStructTagNode(longTy, longTy, 0);
    return longMDN;
  }

  llvm::MDNode *getFloatMDN() {
    if (!floatMDN)
      floatMDN = builder.createTBAAStructTagNode(floatTy, floatTy, 0);
    return floatMDN;
  }

  llvm::MDNode *getDoubleMDN() {
    if (!doubleMDN)
      doubleMDN = builder.createTBAAStructTagNode(doubleTy, doubleTy, 0);
    return doubleMDN;
  }

  llvm::MDNode *getAnyPtrMDN() {
    if (!anyPtrMDN)
      anyPtrMDN = builder.createTBAAStructTagNode(anyPtrTy, anyPtrTy, 0);
    return anyPtrMDN;
  }

  llvm::MDNode *getTBAAForType(llvm::Type *Ty) {
    switch (Ty->getTypeID()) {
    case llvm::Type::IntegerTyID: {
      if (Ty->isIntegerTy(32))
        return intTy;
      if (Ty->isIntegerTy(64))
        return longTy;
    }
    case llvm::Type::FloatTyID:
      return floatTy;
    case llvm::Type::DoubleTyID:
      return doubleTy;
    case llvm::Type::PointerTyID:
      return anyPtrTy;
    case llvm::Type::ArrayTyID:
      return anyPtrTy;
    case llvm::Type::StructTyID: {
      auto structTy = llvm::dyn_cast<llvm::StructType>(Ty);
      assert(structTy);
      return getTBAANodeForStructType(structTy);
    }

    default:
      break;
    }

    return nullptr;
    llvm::errs() << "Type " << *Ty << "\n";
    llvm_unreachable("Type not handled");
  }

  llvm::MDNode *getTBAANodeForPtr(llvm::Value *Ptr) {
    if (MDPtrCache.find(Ptr) != MDPtrCache.end())
      return MDPtrCache[Ptr];

    tbaaCount++;
    std::string name = "ptr" + std::to_string(tbaaCount);
    auto base = builder.createTBAAScalarTypeNode(name, tbaaPtr, 0);
    auto MDN = builder.createTBAAStructTagNode(base, base, 0);
    return MDPtrCache[Ptr] = MDN;
  }

  llvm::MDNode *getTBAANodeForType(llvm::Type *Ty) {
    if (MDCache.find(Ty) != MDCache.end())
      return MDCache[Ty];

    switch (Ty->getTypeID()) {
    case llvm::Type::IntegerTyID: {
      if (Ty->isIntegerTy(32))
        return MDCache[Ty] = getIntMDN();
      if (Ty->isIntegerTy(64))
        return MDCache[Ty] = getLongMDN();
    }
    case llvm::Type::FloatTyID:
      return MDCache[Ty] = getFloatMDN();
    case llvm::Type::DoubleTyID:
      return MDCache[Ty] = getDoubleMDN();
    case llvm::Type::PointerTyID:
      return MDCache[Ty] = getAnyPtrMDN();
    case llvm::Type::ArrayTyID:
      return MDCache[Ty] = getAnyPtrMDN();
    case llvm::Type::StructTyID: {
      auto structTy = llvm::dyn_cast<llvm::StructType>(Ty);
      assert(structTy);
      return getTBAANodeForStructType(structTy);
    }
    default:
      break;
    }

    return nullptr;
    llvm::errs() << "Type " << *Ty << "\n";
    llvm_unreachable("Type not handled");
  }

  llvm::MDNode *getTBAANodeForStructType(llvm::StructType *Ty) {
    if (MDCache.find(Ty) != MDCache.end())
      return MDCache[Ty];

    auto &DL = TheModule->getDataLayout();
    auto SL = DL.getStructLayout(Ty);
    std::vector<std::pair<llvm::MDNode *, uint64_t>> nodes;

    unsigned numElements = Ty->getNumElements();
    for (int i = 0; i < numElements; ++i) {
      auto MDN = getTBAAForType(Ty->getElementType(i));
      if (!MDN)
        return nullptr;
      auto offset = SL->getElementOffset(i);
      nodes.push_back(std::pair<llvm::MDNode *, uint64_t>(MDN, offset));
    }

    tbaaCount++;
    std::string name = Ty->getName().str() + std::to_string(tbaaCount);

    auto structMDN = builder.createTBAAStructTypeNode(name, nodes);
    MDCache[Ty] = structMDN;
    return structMDN;
  }

  llvm::MDNode *getTBAAForStruct(llvm::Value *Ptr, llvm::Type *Ty,
                                 uint64_t offset, llvm::Type *elementTy) {

    auto structTy = llvm::dyn_cast<llvm::StructType>(Ty);
    assert(structTy);
    llvm::MDNode *structMDN = nullptr;
    if (StructMDCache.find(Ptr) != StructMDCache.end()) {
      structMDN = MDPtrCache[Ptr];
      if (StructMDCache[Ptr].find(offset) != StructMDCache[Ptr].end()) {
        return StructMDCache[Ptr][offset];
      }
    }

    if (!structMDN) {
      structMDN = getTBAANodeForStructType(structTy);
      MDPtrCache[Ptr] = structMDN;
    }

    auto elementMDN = getTBAAForType(elementTy);
    if (!elementMDN || !structMDN)
      return nullptr;

    auto &DL = TheModule->getDataLayout();
    auto SL = DL.getStructLayout(structTy);
    auto newOffset = SL->getElementOffset(offset);
    auto tagMDN =
        builder.createTBAAStructTagNode(structMDN, elementMDN, newOffset);
    StructMDCache[Ptr][offset] = tagMDN;
    return tagMDN;
  }
};
} // namespace fc

#endif
