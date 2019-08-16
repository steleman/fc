#ifndef FC_CONTEXT_H
#define FC_CONTEXT_H

#include "common/Debug.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/Allocator.h"

namespace fc {
class Diagnostics;
class ASTContextImpl;
class Type;
class ArrayType;
class FunctionType;

class ASTContext {

private:
  mutable llvm::BumpPtrAllocator allocator;

protected:
  friend class Type;
  friend class ArrayType;
  friend class FunctionType;
  friend class StructType;
  friend class PointerType;
  friend class ComplexType;
  friend class SymbolTable;
  friend class NamedType;
  ASTContextImpl *impl;

public:
  Diagnostics &Diag;
  llvm::StringRef inputFileName;

  explicit ASTContext(Diagnostics &Diag, llvm::StringRef filename);

  llvm::StringRef getName() { return inputFileName; }

  void *allocate(size_t bytes, size_t align = 8) const;

  void deallocate(void *ptr) const;

  ~ASTContext();

  void printStats() { allocator.PrintStats(); }
};
} // namespace fc

inline void *operator new(size_t Bytes, const fc::ASTContext &C,
                          size_t Alignment = 8) {
  return C.allocate(Bytes, Alignment);
}

inline void operator delete(void *Ptr, const fc::ASTContext &C, size_t) {
  C.deallocate(Ptr);
}

#endif
