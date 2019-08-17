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
