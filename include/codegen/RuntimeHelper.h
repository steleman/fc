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
#ifndef FC_CODEGEN_RUNTIME_HELPER_H
#define FC_CODEGEN_RUNTIME_HELPER_H

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"

namespace fc {
struct RuntimeHelper {
private:
  std::unique_ptr<llvm::Module> &M;
  llvm::LLVMContext &Context;

public:
  explicit RuntimeHelper(std::unique_ptr<llvm::Module> &_M,
                         llvm::LLVMContext &C)
      : M(_M), Context(C) {}

  // Names.
  const char *sprintfFnName = "__fc_runtime_sprintf";
  const char *printFnName = "__fc_runtime_print";
  const char *writeFnName = "__fc_runtime_write";
  const char *readFnName = "__fc_runtime_scan";
  const char *openFnName = "__fc_runtime_open";
  const char *fileReadFnName = "__fc_runtime_fread";
  const char *fileWriteFnName = "__fc_runtime_fwrite";
  const char *fileCloseFnName = "__fc_runtime_close";
  const char *stringToIntFnName = "__fc_runtime_stoi";
  const char *stringToIntArrayFnName = "__fc_runtime_stoia";
  const char *intToStringFnName = "__fc_runtime_itos";
  const char *isysClockFnName = "__fc_runtime_isysClock";

  llvm::Function *getSprintfFunction();
  llvm::Function *getPrintFunction();
  llvm::Function *getWriteFunction();
  llvm::Function *getStrCmpFunction();
  llvm::Function *getStrCpyFunction();
  llvm::Function *getStrCatFunction();
  llvm::Function *getReadFunction();
  llvm::Function *getOpenFunction();
  llvm::Function *getFileReadFunction();
  llvm::Function *getFileWriteFunction();
  llvm::Function *getCloseFunction();
  llvm::Function *getStringToIntFunction();
  llvm::Function *getStringToIntArrayFunction();
  llvm::Function *getIntToStringFunction();
  llvm::Function *getISysClockFunction();

  void fillPrintArgsFor(llvm::Value *val,
                        llvm::SmallVectorImpl<llvm::Value *> &argsList,
                        llvm::Value *arrDimSize, llvm::IRBuilder<> *IRB,
                        bool isDynArr = false);

  void fillReadArgsFor(llvm::Value *val,
                       llvm::SmallVectorImpl<llvm::Value *> &argsList,
                       llvm::Value *arrDimSize, llvm::IRBuilder<> *IRB,
                       bool isString);

  void fillOpenArgsFor(llvm::Value *unit, llvm::Value *fileName,
                       llvm::SmallVectorImpl<llvm::Value *> &argList,
                       llvm::IRBuilder<> *IRB);
};
} // namespace fc

#endif
