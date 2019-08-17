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
#include "sema/Sema.h"
#include "AST/ASTContext.h"
#include "AST/ASTPass.h"
#include "llvm/Support/CommandLine.h"

using namespace fc;
using namespace ast;

llvm::cl::opt<bool> RunConstProp("const-prop",
                                 llvm::cl::desc("Run constant propagation"),
                                 llvm::cl::init(true));

bool Sema::run() {

  ASTPassManager semaPM(parseTree, "Sema Pass Manager");

  // Passes before dumping the module file.
  semaPM.addPass(createTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createExprTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createSymbolResolverPass(semaPM.getContext(), false));
  semaPM.addPass(createConstPropPass(semaPM.getContext()));
  semaPM.addPass(createParamConstPropPass(semaPM.getContext()));
  semaPM.addPass(createStmtTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createFunctionTypeUpdaterPass(semaPM.getContext()));

  // Passes after dumping the mod file.
  semaPM.addPass(createUseStmtHandlerPass(semaPM.getContext()));
  semaPM.addPass(createSymbolResolverPass(semaPM.getContext(), false));
  semaPM.addPass(createTypeUpdaterPass(semaPM.getContext(), true));
  semaPM.addPass(createPartRefSymResolverPass(semaPM.getContext()));
  semaPM.addPass(createConstPropPass(semaPM.getContext()));
  semaPM.addPass(createStmtTypeUpdaterPass(semaPM.getContext()));

  // TODO: This should be called only once. Remove earlier call after
  //       modfile dumper todo is fixed
  semaPM.addPass(createFunctionTypeUpdaterPass(semaPM.getContext()));

  // FIXME : This is a hack as, module is already dumped once in
  //         usestmthandler. UseStmtHandler should not depend on
  //         module file when the module is defined in the same file.
  //
  // Module file has to dumped after all types are resolved!
  semaPM.addPass(createModFileDumperPass(semaPM.getContext()));
  semaPM.addPass(createSymbolResolverPass(semaPM.getContext(), true));
  semaPM.addPass(createUndeclaredErrorPrinter(semaPM.getContext()));

  // Here is where the expansion of the AST starts.
  semaPM.addPass(createBinaryOpHandlerPass(semaPM.getContext()));
  semaPM.addPass(createFormatHandlerPass(semaPM.getContext()));
  semaPM.addPass(createConstructExpanderPass(semaPM.getContext()));
  semaPM.addPass(createConstPropPass(semaPM.getContext()));
  semaPM.addPass(createParamConstPropPass(semaPM.getContext()));
  semaPM.addPass(createFunctionTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createArraySectionReplacerPass(semaPM.getContext()));
  semaPM.addPass(createArraySecExpanderPass(semaPM.getContext()));
  semaPM.addPass(createDeclEliminatorPass(semaPM.getContext()));
  semaPM.addPass(createIntrinsicExpanderPass(semaPM.getContext()));
  semaPM.addPass(createArraySecExpanderPass(semaPM.getContext()));
  semaPM.addPass(createExprTypeUpdaterPass(semaPM.getContext()));
  semaPM.addPass(createArrBoundsIntrinExpanderPass(semaPM.getContext()));
  return semaPM.run();
}
